/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#include "tm.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "gengen.h"
#include "structs.h"
#include "../../libgcobol/gcobolio.h"
#include "../../libgcobol/cobol-endian.h"
#include "../../libgcobol/charmaps.h"
#include "show_parse.h"

// These are convenience values used by the ADD 1 TO routines.  I am putting
// them here rather than cluttering up subroutine calls with them.
#define uchar_f_node build_int_cst_type(UCHAR, 0x0F)
#define uchar_ten_node build_int_cst_type(UCHAR, 10)
static tree tzero; // '0' in ascii or ebcdic
static tree tnine;

void
set_up_on_exception_label(cbl_label_t *arithmetic_label)
  {
  if( arithmetic_label )
    {
    if( !arithmetic_label->structs.arith_error )
      {
      arithmetic_label->structs.arith_error
        = static_cast<cbl_arith_error_t *>
                                  (xmalloc(sizeof(struct cbl_arith_error_t)));
      // Set up the address pairs for this clause
      gg_create_goto_pair(&arithmetic_label->structs.arith_error->over.go_to,
                          &arithmetic_label->structs.arith_error->over.label);
      gg_create_goto_pair(&arithmetic_label->structs.arith_error->into.go_to,
                          &arithmetic_label->structs.arith_error->into.label);
      gg_create_goto_pair(&arithmetic_label->structs.arith_error->bottom.go_to,
                          &arithmetic_label->structs.arith_error->bottom.label);
      }
    }
  }

void
set_up_compute_error_label(cbl_label_t *compute_label)
  {
  if( compute_label )
    {
    if( !compute_label->structs.compute_error )
      {
      compute_label->structs.compute_error
        = static_cast<cbl_compute_error_t *>
          (xmalloc(sizeof(struct cbl_compute_error_t)));
      compute_label->structs.compute_error->compute_error_code
        = gg_define_variable(INT, 0L);
      }
    }
  }

static void
set_up_arithmetic_error_handler(cbl_label_t *error,
                                cbl_label_t *not_error)
  {
  Analyze();
  // There might, or might not, be error and/or not_error labels:
  set_up_on_exception_label(error);
  set_up_on_exception_label(not_error);
  }

static void
arithmetic_operation( size_t nC, cbl_num_result_t *C,
                      size_t nA, cbl_refer_t *A,
                      size_t nB, cbl_refer_t *B,
                      cbl_arith_format_t format,
                      const cbl_label_t *error,
                      const cbl_label_t *not_error,
                      tree compute_error, // Pointer to int
                      const char *operation,
                      cbl_refer_t *remainder = NULL)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT_AB("performing ", operation, "")
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("calling ", operation, "")
    for(size_t ii=0; ii<nA; ii++)
      {
      TRACE1_INDENT
      gg_fprintf( trace_handle,
                  1, "parameter A[%ld]: ",
                  build_int_cst_type(SIZE_T, ii));
      TRACE1_REFER("", A[ii], "");
      }
    for(size_t ii=0; ii<nB; ii++)
      {
      TRACE1_INDENT
      gg_fprintf( trace_handle,
                  1, "parameter B[%ld]: ",
                  build_int_cst_type(SIZE_T, ii));
      TRACE1_REFER("", B[ii], "");
      }
    }

  // We need to split up cbl_num_result_t into two arrays, one for the refer_t
  // and a second for the cbl_round_t enums.

  // Allocate nC+1 in case this is a divide with a REMAINDER

  std::vector<cbl_refer_t> results(nC + 1);
  int ncount = 0;

  // We have to take into account the possibility the quotient of the division
  // can affect the disposition of the remainder.  In particular, some of the
  // NIST tests have the construction

  // DIVIDE A BY B GIVING C REMAINDER TABLE(C)

  // Which seems, somehow, unnatural.

  cbl_refer_t temp_remainder;
  cbl_field_t temp_field = {};

  if( remainder )
    {
    // We need a duplicate of the remainder, because we have to take into count
    // the possibility of a size error in moving the remainder into place
    temp_field.type = remainder->field->type;
    temp_field.attr = (remainder->field->attr | intermediate_e) & ~initialized_e;
    temp_field.level = 1;
    temp_field.data = remainder->field->data;
    temp_field.codeset = remainder->field->codeset ;
    parser_symbol_add(&temp_field);
    temp_remainder.field = &temp_field;

    // For division, the optional remainder goes onto the beginning of the
    // list
    results[ncount++] = temp_remainder;
    }
  tree array_of_int_type = build_array_type_nelts(INT, nC+1);
  tree arithmetic_rounds = gg_define_variable(array_of_int_type);
  for(size_t i=0; i<nC; i++)
    {
    results[ncount] = C[i].refer;
    gg_assign(  gg_array_value(arithmetic_rounds, ncount),
                build_int_cst_type(INT, C[i].rounded));
    ncount += 1;
    }

  // REMAINDER_PRESENT means what it says.
  // ON_SIZE_ERROR means that the ON SIZE ERROR phrase is present

  int call_flags =   (( error || not_error ) ? ON_SIZE_ERROR : 0)
                   + (remainder ? REMAINDER_PRESENT : 0);

  gcc_assert(compute_error);

  // Having done all that work, we now need to break out the various different
  // arithmetic routines that implement the various possibilities,

  tree referlets_A = build_array_of_referlets(nA, A);
  tree referlets_B = build_array_of_referlets(nB, B);
  tree referlets_C = build_array_of_referlets(ncount, results.data());
  gg_call(VOID,
          operation,
          build_int_cst_type(INT, format),
          build_int_cst_type(SIZE_T, nA),
          referlets_A,
          build_int_cst_type(SIZE_T, nB),
          referlets_B,
          build_int_cst_type(SIZE_T, ncount),
          referlets_C,
          gg_pointer_to_array(arithmetic_rounds),
          build_int_cst_type(INT, call_flags),
          compute_error,
          NULL_TREE);
  TRACE1
    {
    for(size_t ii=0; ii<nC; ii++)
      {
      TRACE1_INDENT
      gg_fprintf( trace_handle,
                  1, "result: C[%ld]: ",
                  build_int_cst_type(SIZE_T, ii));
      TRACE1_REFER("", C[ii].refer, "");
      }
    TRACE1_END
    }

  // We just did an operation.
  IF( gg_indirect(compute_error), ne_op, integer_zero_node )
    {
    gg_call(  VOID,
              "__gg__process_compute_error",
              gg_indirect(compute_error),
              NULL_TREE);
    }
  ELSE
    ENDIF

  if( remainder )
    {
    parser_move(*remainder, temp_remainder);
    }

  SHOW_PARSE
    {
    SHOW_PARSE_END
    }
  }

static void
arithmetic_error_handler( cbl_label_t *error,
                          cbl_label_t *not_error,
                          tree compute_error) // Pointer to int with bits
  {
  Analyze();
  if( error )
    {
    // We had an ON SIZE ERROR phrase
    IF( gg_indirect(compute_error), ne_op, integer_zero_node)
      {
      // The ON SIZE ERROR imperative takes precedence over exception processing
      // So, we set the global exception code to zero.  This leaves intact the
      // stashed data needed for FUNCTION EXCEPTION-STATUS, but will preclude
      // any declarative processing
      gg_assign(var_decl_exception_code, integer_zero_node);

      // There was some kind of error, so we execute the ON SIZE ERROR
      // imperative
      gg_append_statement( error->structs.arith_error->into.go_to );
      }
    ELSE
      ENDIF
    }

  if( not_error )
    {
    IF( gg_indirect(compute_error), eq_op, integer_zero_node)
      {
      // There wasn't a computation error
      gg_append_statement( not_error->structs.arith_error->into.go_to );
      }
    ELSE
    ENDIF
    }

  // With the operation and the two possible GO TOs laid down, it's time
  // to create the target labels for exiting the ON [NOT] SIZE ERROR blocks:
  if( error )
    {
    gg_append_statement( error->structs.arith_error->bottom.label );
    }
  if( not_error )
    {
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }
  }

static bool
is_somebody_float(size_t nA, const cbl_refer_t *A)
  {
  bool retval = false;
  for(size_t i=0; i<nA; i++)
    {
    if(A[i].field->type == FldFloat)
      {
      retval = true;
      break;
      }
    }
  return retval;
  }

static bool
is_somebody_float(size_t nC, const cbl_num_result_t *C)
  {
  bool retval = false;
  for(size_t i=0; i<nC; i++)
    {
    if(C[i].refer.field->type == FldFloat)
      {
      retval = true;
      break;
      }
    }
  return retval;
  }

static bool
all_results_integer(size_t nC, const cbl_num_result_t *C)
  {
  bool retval = true;

  for(size_t i=0; i<nC; i++)
    {
    if( !is_pure_integer(C[i].refer.field) )
      {
      retval = false;
      break;
      }
    }
  return retval;
  }

static bool
all_refers_integer(size_t nC, const cbl_refer_t *C)
  {
  bool retval = true;

  for(size_t i=0; i<nC; i++)
    {
    if( !is_pure_integer(C[i].field) )
      {
      retval = false;
      break;
      }
    }
  return retval;
  }

static tree
largest_binary_term(size_t nA, const cbl_refer_t *A)
  {
  tree retval = NULL_TREE;
  for(size_t i=0; i<nA; i++)
    {
    if( !is_pure_integer(A[i].field) || A[i].field->type == FldFloat )
      {
      // We are prepared to work only with binary integers
      retval = NULL_TREE;
      break;
      }
    if(    A[i].field->type == FldLiteralN
        || A[i].field->type == FldNumericBinary
        || A[i].field->type == FldNumericBin5
        || A[i].field->type == FldIndex
        || A[i].field->type == FldPointer
        || (   A[i].field->type == FldAlphanumeric
            && strcmp(A[i].field->name, "ZEROS") == 0 )
        )
      {
      // This is an integer type that can be worked with quickly
      retval = tree_type_from_refer(A[i]);
      }
    else
      {
      // This is a type we don't care to deal with for fast arithmetic
      retval = NULL_TREE;
      break;
      }
    }
  return retval;
  }

static bool
fast_add( size_t nC, cbl_num_result_t *C,
          size_t nA, const cbl_refer_t *A,
          cbl_arith_format_t format,
    const cbl_label_t *error,
    const cbl_label_t *not_error)
  {
  /*  ADD A     TO D:           nC==1, nA==1,  D += A.
      ADD A B C TO D:           nC==1, nA==3,  D = (A + B + C)
      ADD A B C TO D E          nC==2, nA==3
      ADD A     TO B GIVING D   nC==1, nA==2, format==giving_e
      ADD A B C TO D GIVING X Y nC==2, nA==3, format==giving_e   */
  bool retval = false;
  if(    all_results_integer(nC, C)
      && all_refers_integer(nA, A)
      && !error
      && !not_error )
    {
    Analyze();
    // All targets are non-PICTURE binaries:
    tree term_type = largest_binary_term(nA, A);
    if( term_type )
      {
      tree dest_type = tree_type_from_refer(C[0].refer);
      // All the numbers are integers without rdigits
      if(    nC == 1
          && nA == 1
          && format != giving_e
          )
        {
        // This is the simplest case of all.  Just add A to C.  We can't
        // naively add A to multiple C, because of the possibility of
        // ADD A TO A B C.  That would change A before A gets added to B and
        // C, which is not how COBOL works.

        tree A_value;
        get_binary_value(A_value, A[0], dest_type);

        if( refer_is_clean(C[0].refer) )
          {
          // We are accumulating into memory

          if(is_working_storage(C[0].refer)
             && C[0].refer.field->offset == 0 )
            {
            gg_assign(  C[0].refer.field->data_decl_node,
                        gg_cast(TREE_TYPE(C[0].refer.field->data_decl_node),
                        gg_add( C[0].refer.field->data_decl_node, A_value)));
            }
          else
            {
            tree dest_addr = member(C[0].refer.field->var_decl_node,
                                    "data");
            tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
            gg_assign(  gg_indirect(ptr),
                        gg_add( gg_indirect(ptr),
                                A_value));
            }
          }
        else
          {
          tree dest_addr = gg_add(member(C[0].refer.field->var_decl_node,
                                        "data"),
                                  refer_offset(C[0].refer));
          tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
          // We are accumulating into memory
          gg_assign(  gg_indirect(ptr),
                      gg_add( gg_indirect(ptr),
                              A_value));
          }
        }
      else if(   nC == 1
              && nA == 2
              && format == giving_e )
        {
        // This is the very common ADD A TO B GIVING C
          {
          // Make C = A[0] + A[1]
          tree dest_addr;
          if( refer_is_clean(C[0].refer) )
            {
            dest_addr = member(C[0].refer.field->var_decl_node, "data");
            }
          else
            {
            dest_addr = gg_add(member(C[0].refer.field->var_decl_node, "data"),
                               refer_offset(C[0].refer));
            }
          dest_addr = gg_cast(build_pointer_type(dest_type), dest_addr);

          tree A_value;
          get_binary_value(A_value, A[0], dest_type);

          tree B_value;
          get_binary_value(B_value, A[1], dest_type);

          gg_assign(  gg_indirect(dest_addr),
                      gg_add( A_value,
                              B_value));
          }
        }
      else
        {
        // We need to calculate the sum of all the A[] terms using term_type as
        // the intermediate type:

        tree sum    ;
        tree addend ;
        get_binary_value(sum, A[0].field, term_type);

        // Add in the rest of them:
        for(size_t i=1; i<nA; i++)
          {
          get_binary_value( addend, A[i].field, term_type);
          gg_assign(sum, gg_add(sum, addend));
          }

        // We now either accumulate into C[n] or assign to C[n]:
        for(size_t i=0; i<nC; i++ )
          {
          tree dest_addr = gg_add(member(C[i].refer.field->var_decl_node,
                                        "data"),
                                  refer_offset(C[i].refer));
          tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
          if( format == giving_e )
            {
            // We are assigning
            gg_assign(  gg_indirect(ptr),
                        gg_cast(dest_type, sum));
            }
          else
            {
            // We are accumulating
            gg_assign(  gg_indirect(ptr),
                        gg_add( gg_indirect(ptr),
                                gg_cast(dest_type, sum)));
            }
          }
        }
      retval = true;
      }
    }
  return retval;
  }

static bool
fast_subtract(size_t nC, cbl_num_result_t *C,
              size_t nA, const cbl_refer_t *A,
              size_t nB, const cbl_refer_t *B,
              cbl_arith_format_t format,
        const cbl_label_t *error,
        const cbl_label_t *not_error)
  {
  /*  SUBTRACT A FROM D:       nC==1, nA==1, nB==0:  D -= A.
      SUBTRACT A B C FROM D:   nC==1, nA==3, nB==0:  D -= (A + B + C)
      SUBTRACT A B C FROM D E  nC==2, nA==3
      SUBTRACT A B C FROM D GIVING X Y
                               nC==2, nA==3, nB==1  */
  bool retval = false;
  if(    all_refers_integer(nA, A)
      && all_refers_integer(nB, B)
      && all_results_integer(nC, C)
      && !error
      && !not_error
      )
    {
    Analyze();
    // All targets are non-PICTURE binaries:
    //gg_insert_into_assembler("# DUBNER addition START");
    tree term_type = largest_binary_term(nA, A);

    if( term_type && format == giving_e )
      {
      tree term_type_B = largest_binary_term(nB, B);
      if( term_type_B )
        {
        if(TREE_INT_CST_LOW(TYPE_SIZE(term_type_B))
                                    > TREE_INT_CST_LOW(TYPE_SIZE(term_type)) )
          {
          term_type = term_type_B;
          }
        }
      else
        {
        term_type = NULL_TREE;
        }
      }

    if( term_type )
      {
      // All the terms are things we can work with.
      // All the numbers are integers without rdigits
      if(    nC == 1
          && nA == 1
          && nB <= 1
          )
        {
        // This is the simplest case of all.  Just subtract A from C.
        tree dest_type = tree_type_from_refer(C[0].refer);
        tree A_value;
        get_binary_value(A_value, A[0], dest_type);
        if( format == giving_e )
          {
          // Make C = B - A
          tree dest_addr;
          if( refer_is_clean(C[0].refer) )
            {
            dest_addr = member(C[0].refer.field->var_decl_node, "data");
            }
          else
            {
            dest_addr = gg_add(member(C[0].refer.field->var_decl_node, "data"),
                               refer_offset(C[0].refer));
            }
          dest_addr = gg_cast(build_pointer_type(dest_type), dest_addr);

          tree B_value;
          get_binary_value(B_value, B[0], dest_type);
          gg_assign(  gg_indirect(dest_addr),
                      gg_cast(dest_type, gg_subtract( B_value,
                                                      A_value)));
          }
        else
          {
          // Make C = C - A
          if( refer_is_clean(C[0].refer) )
            {
            tree dest_addr = member(C[0].refer.field->var_decl_node,
                                    "data");
            tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
            // We are subtracting from memory
            gg_assign(  gg_indirect(ptr),
                        gg_subtract( gg_indirect(ptr),
                                A_value));
            }
          else
            {
            tree dest_addr = gg_add(member(C[0].refer.field->var_decl_node,
                                          "data"),
                                    refer_offset(C[0].refer));
            tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
            // We are subtracting from memory
            gg_assign(  gg_indirect(ptr),
                        gg_subtract( gg_indirect(ptr),
                                A_value));
            }
          }
        }
      else
        {
        // We need to calculate the sum of all the A[] terms using term_type as
        // the intermediate type:

        tree sum    ;
        tree addend ;
        get_binary_value(sum, A[0].field, term_type);

        // Add in the rest of them:
        for(size_t i=1; i<nA; i++)
          {
          get_binary_value(addend, A[i].field, term_type);
          gg_assign(sum, gg_add(sum, addend));
          }
        //gg_printf("The intermediate sum is %ld\n", gg_cast(LONG, sum), NULL_TREE);

        if( format == giving_e )
          {
          // We now subtract the sum from B[0]
          get_binary_value(addend, B[0].field, term_type);
          gg_assign(sum, gg_subtract(addend, sum));
          }

        // We now either accumulate into C[n] or assign to C[n]:
        for(size_t i=0; i<nC; i++ )
          {
          tree dest_type = tree_type_from_refer(C[i].refer);
          tree dest_addr = gg_add(member(C[i].refer.field->var_decl_node, "data"),
                                  refer_offset(C[i].refer));
          tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
          if( format == giving_e )
            {
            // We are assigning
            gg_assign(  gg_indirect(ptr),
                        gg_cast(dest_type, sum));
            }
          else
            {
            // We are subtracting the sum from C[i]
            gg_assign(  gg_indirect(ptr),
                        gg_subtract(gg_indirect(ptr),
                                    gg_cast(dest_type, sum)));
            }
          }
        }
      retval = true;
      }
    }
  return retval;
  }

static bool
fast_multiply(size_t nC, cbl_num_result_t *C,
              size_t nA, const cbl_refer_t *A,
              size_t nB, const cbl_refer_t *B)
  {
  bool retval = false;
  if( all_results_integer(nC, C) )
    {
    Analyze();
    // All targets are non-PICTURE binaries:
    //gg_insert_into_assembler("# DUBNER addition START");
    tree term_type = largest_binary_term(nA, A);

    if( term_type && nB )
      {
      tree term_type_B = largest_binary_term(nB, B);
      if( term_type_B )
        {
        if(TREE_INT_CST_LOW(TYPE_SIZE(term_type_B))
                                    > TREE_INT_CST_LOW(TYPE_SIZE(term_type)) )
          {
          term_type = term_type_B;
          }
        }
      else
        {
        term_type = NULL_TREE;
        }
      }

    if( term_type )
      {
      // All the terms are things we can work with.

      tree valA ;
      tree valB ;
      get_binary_value(valA, A[0].field, term_type);

      if( nB )
        {
        // This is a MULTIPLY Format 2
        get_binary_value(valB, B[0].field, term_type);
        gg_assign(valA, gg_multiply(valA, valB));
        }

      // We now either multiply into C[n] or assign A * B to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type = tree_type_from_refer(C[i].refer);
        tree dest_addr = gg_add(member(C[i].refer.field->var_decl_node, "data"),
                                refer_offset(C[i].refer));
        tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
        if( nB )
          {
          // We put A * B into C
          gg_assign(gg_indirect(ptr), gg_cast(dest_type, valA));
          }
        else
          {
          // We multiply C = valA * C
          gg_assign(gg_indirect(ptr),
                    gg_multiply(gg_indirect(ptr), valA));
          }
        }
      retval = true;
      }

    //gg_insert_into_assembler("# DUBNER addition END ");
    }
  return retval;
  }

static bool
fast_divide(size_t nC, cbl_num_result_t *C,
            size_t nA, const cbl_refer_t *A,
            size_t nB, const cbl_refer_t *B,
      const cbl_refer_t             &remainder)
  {
  bool retval = false;
  if( all_results_integer(nC, C) )
    {
    Analyze();
    // All targets are non-PICTURE binaries:
    //gg_insert_into_assembler("# DUBNER addition START");
    tree term_type = largest_binary_term(nA, A);

    if( term_type && nB )
      {
      tree term_type_B = largest_binary_term(nB, B);
      if( term_type_B )
        {
        if(TREE_INT_CST_LOW(TYPE_SIZE(term_type_B))
                                    > TREE_INT_CST_LOW(TYPE_SIZE(term_type)) )
          {
          term_type = term_type_B;
          }
        }
      else
        {
        term_type = NULL_TREE;
        }
      }

    if( term_type )
      {
      // All the terms are things we can work with.

      tree divisor  ;
      tree dividend ;
      tree quotient = NULL_TREE;
      get_binary_value(divisor, A[0].field, term_type);

      if( nB )
        {
        // This is a MULTIPLY Format 2, where we are dividing A into B and
        // assigning that to C
        get_binary_value(dividend, B[0].field, term_type);

        quotient = gg_define_variable(term_type);
        // Yes, in this case the divisor and dividend are switched.  Things are
        // tough all over.
        gg_assign(quotient, gg_divide(divisor, dividend));
        }

      // We now either divide into C[n] or assign dividend/divisor to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type = tree_type_from_refer(C[i].refer);
        tree dest_addr = gg_add(member( C[i].refer.field->var_decl_node,
                                        "data"),
                                refer_offset(C[i].refer));
        tree ptr = gg_cast(build_pointer_type(dest_type), dest_addr);
        if( nB )
          {
          // We put A * B into C
          gg_assign(gg_indirect(ptr), gg_cast(dest_type, quotient));
          }
        else
          {
          // We divide the divisor into C
          gg_assign(gg_indirect(ptr),
                    gg_divide(gg_indirect(ptr), divisor));
          }

        // This is where we handle any remainder, keeping in mind that for
        // nB != 0, the actual dividend is in the value we have named
        // "divisor".

        // We calculate the remainder by calculating
        //    dividend minus quotient * divisor
        if( remainder.field )
          {
          dest_addr = gg_add( member(remainder.field->var_decl_node, "data"),
                              refer_offset(remainder));
          dest_type = tree_type_from_refer(remainder);
          ptr = gg_cast(build_pointer_type(dest_type), dest_addr);

          gg_assign(gg_indirect(ptr),
                    gg_cast(dest_type, gg_subtract(divisor,
                                       gg_multiply(quotient, dividend))));
          }
        }
      retval = true;
      }

    //gg_insert_into_assembler("# DUBNER addition END ");
    }
  return retval;
  }

static bool
add_floats( size_t nC, cbl_num_result_t *C,
            size_t nA, cbl_refer_t *A,
            cbl_arith_format_t format,
            cbl_label_t *error,
            cbl_label_t *not_error,
            tree         compute_error )
  {
  bool handled = false;

  bool computation_is_float =    is_somebody_float(nA, A)
                              || is_somebody_float(nC, C);
  // We now start deciding which arithmetic routine we are going to use:
  if( computation_is_float )
    {
    switch( format )
      {
      case no_giving_e:
        {
        // Float format 1

        set_up_arithmetic_error_handler(error,
                                        not_error);
        // Do phase 1, which calculates the subtotal and puts it into a
        // temporary location
        arithmetic_operation( 0, NULL,
                              nA, A,
                              0, NULL,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__add_float_phase1");

        // Do phase 2, which accumulates the subtotal into each target location in turn
        for(size_t i=0; i<nC; i++)
          {
          arithmetic_operation(1, &C[i],
                                0, NULL,
                                0, NULL,
                                format,
                                error,
                                not_error,
                                compute_error,
                                "__gg__addf1_float_phase2");
          }
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;
        break;
        }

      case giving_e:
        {
        // Float format 2
        set_up_arithmetic_error_handler(error,
                                        not_error);
        // Do phase 1, which calculates the subtotal and puts it into a
        // temporary location
        arithmetic_operation( 0, NULL,
                              nA, A,
                              0, NULL,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__add_float_phase1");

        // Do phase 2, which puts the subtotal into each target location in turn
        for(size_t i=0; i<nC; i++)
          {
          arithmetic_operation(1, &C[i],
                                0, NULL,
                                0, NULL,
                                format,
                                error,
                                not_error,
                                compute_error,
                                "__gg__float_phase2_assign_to_c");
          }
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;
        break;
        }

      case corresponding_e:
        {
        // Float format 3
        gcc_assert(nA == nC);

        set_up_arithmetic_error_handler(error,
                                        not_error);
        arithmetic_operation(nC, C,
                              nA, A,
                              0, NULL,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__addf3");
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;
        break;
        }

      case not_expected_e:
        gcc_unreachable();
        break;
      }
    }
  return handled;
  }

static void
ordinary_add_format_1( size_t nC, cbl_num_result_t *C,
                      size_t nA, cbl_refer_t *A,
                      cbl_arith_format_t format,
                      cbl_label_t *error,
                      cbl_label_t *not_error,
                      tree         compute_error )
  {
  set_up_arithmetic_error_handler(error,
                                  not_error);
  // Do phase 1, which calculates the subtotal and puts it into a
  // temporary location
  arithmetic_operation( 0, NULL,
                        nA, A,
                        0, NULL,
                        format,
                        error,
                        not_error,
                        compute_error,
                        "__gg__add_fixed_phase1");

  // Do phase 2, which accumulates the subtotal into each target location
  // in turn
  for(size_t i=0; i<nC; i++)
    {
    arithmetic_operation(1, &C[i],
                          0, NULL,
                          0, NULL,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__addf1_fixed_phase2");
    }
  arithmetic_error_handler( error,
                            not_error,
                            compute_error);
  }

static void
ordinary_subtract_format_1( size_t nC, cbl_num_result_t *C,
                            size_t nA, cbl_refer_t *A,
                            cbl_arith_format_t format,
                            cbl_label_t *error,
                            cbl_label_t *not_error,
                            tree         compute_error )
  {
  set_up_arithmetic_error_handler(error,
                                  not_error);
  // Do phase 1, which calculates the subtotal and puts it into a
  // temporary location
  arithmetic_operation( 0, NULL,
                        nA, A,
                        0, NULL,
                        format,
                        error,
                        not_error,
                        compute_error,
                        "__gg__add_fixed_phase1");

  // Do phase 2, which subtracts the subtotal from each target in turn
  for(size_t i=0; i<nC; i++)
    {
    arithmetic_operation(1, &C[i],
                          0, NULL,
                          0, NULL,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__subtractf1_fixed_phase2");
    }
  arithmetic_error_handler( error,
                            not_error,
                            compute_error);
  }

static void
add_case_1( tree pointer,
            tree tdelta,
      const charmap_t *charmap,
            int delta,
            tree counter)
  {
  // This is Case 1: Adding a positive number to the positive target.  The
  // target can be PIC 9999 or PIC S9999.

  tree top_goto;
  tree top_label;
  tree break_goto;
  tree break_label;
  gg_create_goto_pair(&top_goto,
                      &top_label);
  gg_create_goto_pair(&break_goto,
                      &break_label);

  // We start off by adding tdelta to the first digit:
  gg_assign(gg_indirect(pointer),
            gg_add(gg_indirect(pointer), tdelta));
  // This is our first decision point.  We added a positive value to
  // an ASCII (or EBCDIC) digit.  If the result is less than or equal
  // to '9', then we are done.
  IF( gg_indirect(pointer), le_op, tnine )
    {
    if( charmap->is_like_ebcdic() && delta >= 7 )
      {
      /* EBCDIC leads to an odd situation.  The range of digits in
         EBCDIC is xF0 though 0xF9.  That means that when DELTA is
         >= 7 and the units digit is '9', the sum of 7 + 0xF9 is 0x100.
         That's a carry condition, but we are working in UCHAR space,
         so it looks to us like zero.  And, so, we need some extra
         logic so that we notice it that the zero is actually a
         carry condition, and not something we are supposed to ignore.

         The largest possible value we might see is 9 + 0xF9, which is
         0x102, which to us looks like 0x02, so if the result is zero,
         one, or two, we need to enter the carry condition.  */
      IF( gg_indirect(pointer), ge_op, build_int_cst_type(UCHAR, 3) )
        {
        // We are done with adding delta to C[0].
        gg_append_statement(break_goto);
        }
      ELSE
        {
        // Fall through to carry processing
        }
      ENDIF
      }
    else
      {
      // We are done with adding delta to C[0].
      gg_append_statement(break_goto);
      }
    }
  ELSE
    {
    // Fall through to carry processing
    }
  ENDIF
  // We added delta to the current digit, and the result was bigger
  // than '9'.  Normalize that digit by subtracting ten from it
  gg_assign(gg_indirect(pointer),
            gg_subtract(gg_indirect(pointer),
                        build_int_cst_type(UCHAR, 10)));

  // This is the top of the carry loop:
  gg_append_statement(top_label);
  IF( counter, le_op, integer_one_node )
    {
    // We have rippled through every digit, meaning we just added
    // 1 to 9999, yielding 0000.

    set_exception_code(ec_size_truncation_e);

    gg_append_statement(break_goto);
    }
  ELSE
    {
    }
  ENDIF
  // Move the pointer one digit to the left
  gg_decrement(pointer);
  // Propagate the carry
  gg_increment(gg_indirect(pointer));

  IF( gg_indirect(pointer), le_op, tnine )
    {
    // We are done with adding delta to C[0].
    gg_append_statement(break_goto);
    }
  ELSE
    {
    }
  ENDIF
  // By incrementing this place, it went past '9'.  Wrap it back to
  // zero
  gg_assign(gg_indirect(pointer), tzero);
  // And go see if there are more digits that need carry propagation:
  gg_decrement(counter);
  gg_append_statement(top_goto);

  // That was the end of the carry propagation loop.  At this point
  // we are done; somebody will jump to us from inside the loops:
  gg_append_statement(break_label);
  }

static void
add_case_2( tree pointer,
            tree tdelta,
            tree counter,
            int  digits)
  {
  // This is Case 2: Adding a negative number to the positive target.  The
  // target is assumed to be an unsigned PIC 9999.

  /* The tricky thing about this case is when you go downward through zero.
     The logic we use is that 0 minus 1 is negative 1, and when you move
     negative 1 to PIC 9999, the result is 0001.  That logic is extended here,
     and so `SUBTRACT 3 FROM FOO` where FOO is PIC 9999 and has the value 1,
     results in FOO being 0002.

     There is an edge case:  Consider the PIC 99V99 value 0.21  Adding -1 to
     that results in -0.79, which needs to go into the PIC 99V99 as 0079. We
     don't try to do that here; an add_case_2 can't be used to do that
     calculation. */

  tree top_goto;
  tree top_label;
  tree break_goto;
  tree break_label;
  gg_create_goto_pair(&top_goto,
                      &top_label);
  gg_create_goto_pair(&break_goto,
                      &break_label);

  // We start off by adding tdelta to the first digit:
  gg_assign(gg_indirect(pointer),
            gg_add(gg_indirect(pointer), tdelta));
  // This is our first decision point.  We added a negative value to
  // an ASCII (or EBCDIC) digit.  If the result is greater than or equal
  // to '0', then we are done.
  IF( gg_indirect(pointer), ge_op, tzero )
    {
    // We are done with adding delta to C[0].
    gg_append_statement(break_goto);
    }
  ELSE
    {
    // Fall through to carry processing
    }
  ENDIF
  // We added delta to the current digit, and the result was less than
  // than '0'.  Normalize that digit by adding ten to it
  gg_assign(gg_indirect(pointer),
            gg_add(gg_indirect(pointer),
                   build_int_cst_type(UCHAR, 10)));

  // This is the top of the carry loop:
  gg_append_statement(top_label);
  IF( counter, le_op, integer_one_node )
    {
    // We have rippled through every digit, meaning we just added
    // subtracted, for example, 1 from zero, yielding 9999, or we subtracted
    // 3 from 0001, yielding 9998.  As discussed above, we need to convert the
    // rightmost place from '8' to '2', and we have to set the other places to
    // '0'.

    // 'pointer' is still pointing to the leftmost digit.  Counter is equal to
    // one.  'digits' was provided to us; for PIC 9999, it is four.

    WHILE( counter, lt_op, build_int_cst_type(INT, digits) )
      {
      gg_assign(gg_indirect(pointer), tzero);
      gg_increment(pointer);
      gg_increment(counter);
      }
    WEND
    /* 'pointer' points to the rightmost place.  When we start with, say '8',
       we want to end up with '2'.  The formula for that is

             '9' + '0' + 1 - *pointer

       Don't take my word for it.  Check it.  */
    tree sum1 = gg_add(tnine, tzero);
    tree sum2 = gg_add(sum1, build_int_cst_type(UCHAR, 1));
    gg_assign( gg_indirect(pointer), gg_subtract(sum2, gg_indirect(pointer)));

    gg_append_statement(break_goto);
    }
  ELSE
    {
    }
  ENDIF
  // Move the pointer one digit to the left
  gg_decrement(pointer);
  // Propagate the carry
  gg_decrement(gg_indirect(pointer));

  IF( gg_indirect(pointer), ge_op, tzero )
    {
    // We are done with adding delta to C[0].
    gg_append_statement(break_goto);
    }
  ELSE
    {
    }
  ENDIF
  // By decrementing this place, it went past '0'.  Wrap it back to
  // nine
  gg_assign(gg_indirect(pointer), tnine);
  // And go see if there are more digits that need carry propagation:
  gg_decrement(counter);
  gg_append_statement(top_goto);

  // That was the end of the carry propagation loop.  At this point
  // we are done; somebody will jump to us from inside the loops:
  gg_append_statement(break_label);
  }

static void
add_case_3( tree pointer,
            tree tdelta,
            tree counter)
  {
  /* Case 3 is adding a positive N to a negative value.

     Because the target is a PIC S9999, we are starting off with something like
     "123t", which means -1234.  Adding 1 to it means going to -1233, which
     means we have to decrement the 't' rather than incrementing it.

     After doing that operation, we have to check to see if we arrived at, or
     went past, zero.  When that happens we have to adjust that final digit,
     and make the result positive.  */

  tree break_goto;
  tree break_label;
  gg_create_goto_pair(&break_goto,
                      &break_label);

  // We subtract the positive value from the rightmost digit.  This will bring
  // the negative value closer to zero.

  gg_assign(gg_indirect(pointer),
            gg_subtract(gg_indirect(pointer), tdelta));

  IF( gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
      lt_op,
      uchar_ten_node )
    {
    // There was no carry, so we are done.
    }
  ELSE
    {
    // We need to adjust that rightmost digit:
    gg_assign(gg_indirect(pointer),
              gg_subtract(gg_indirect(pointer), uchar_ten_node));

    // We need to propagate the carry to the left.
    WHILE(counter, gt_op, integer_one_node)
      {
      gg_decrement(pointer);
      IF( gg_indirect(pointer), ne_op, tzero )
        {
        // Somebody is non-zero, so the result is a negative number whose
        // final digit is zero.
        gg_decrement(gg_indirect(pointer));
        gg_append_statement(break_goto);
        }
      ELSE
        {
        // The digit is zero.  Convert it to '9', and keep going.
        gg_assign(gg_indirect(pointer), tnine);
        }
      ENDIF
      gg_decrement(counter);
      }
    WEND
    }
  ENDIF
  gg_append_statement(break_label);
  }

static void
add_case_4( tree pointer,
            tree tdelta,
            tree counter)
  {
  /* Case 4 is adding a negative N to a value starting off negative.

     Because the target is a PIC S9999, we are starting off with something like
     "123t", which means -1234.  Adding -1 to it means going to -1235, which
     means we have to increment the 't' rather than decrementing it.

     After doing that operation, we have to check to see if we rolled over into
     "0000" which must be converted to a positive value.  Otherwise, if we
     carry out, we just leave it be, and raise the ec_truncation exception. */

  IF( gg_subtract(gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
                  tdelta),
      lt_op,
      uchar_ten_node )
    {
    // The digit minus tdelta is less than ten, so we can just do that
    // operation
    gg_assign(gg_indirect(pointer),
              gg_subtract(gg_indirect(pointer), tdelta));
    }
  ELSE
    {
    tree break_goto;
    tree break_label;
    gg_create_goto_pair(&break_goto,
                        &break_label);

    // Do the operation that will require a carry.  This next instruction is
    // equivalent to {digit-tdelta - 10}
    gg_assign(gg_indirect(pointer),
              gg_subtract(gg_indirect(pointer),
                          gg_add(tdelta,
                                 uchar_ten_node)));
    // And now we start rippling the carry
    WHILE( counter, gt_op, integer_one_node )
      {
      gg_decrement(pointer);
      IF( gg_indirect(pointer), lt_op, tnine )
        {
        // The digit is less than '9', so we are done here.
        gg_increment(gg_indirect(pointer));
        gg_append_statement(break_goto);
        }
      ELSE
        {
        // Set that place to '0', and keep propagating the carry.
        gg_assign(gg_indirect(pointer), tzero);
        }
      ENDIF
      gg_decrement(counter);
      }
    WEND
    // Arriving here means we have carried off the end, which is a truncation
    // situation.
    set_exception_code(ec_size_truncation_e);

    gg_append_statement(break_label);
    }
  ENDIF
  }

static bool
add_litN_to_numdisp(size_t nC, cbl_num_result_t *C,
                    size_t nA, cbl_refer_t *A,
                    cbl_arith_format_t format,
                    cbl_label_t *error,
                    cbl_label_t *not_error,
                    tree         compute_error,
                    bool         subtracting)
  {
  /* This routine handles adding a literal value N in the range of -9 through
     +9 to a Numeric Display variable when the codeset is Single Byte Coded
     ASCII or EBCDIC.
                        */
  bool handled = false;

  if(    format == no_giving_e
      && !error
      && !not_error
      && nC == 1
      && nA == 1
      &&   A[0].field->type == FldLiteralN
      &&   C[0].refer.field->type == FldNumericDisplay
      && !(C[0].refer.field->attr & scaled_e)
      &&   C[0].refer.field->codeset.stride() == 1)
    {
    // We are adding a FldLiteral to a FldNumericDisplay.

    // Get the integer value of the literal:
    REAL_VALUE_TYPE val = TREE_REAL_CST(A[0].field->data.value_of());
    int delta = (int)real_to_integer (&val);
    val = real_value_truncate (TYPE_MODE (float_type_node), val);
    REAL_VALUE_TYPE rival;
    real_from_integer (&rival, VOIDmode, delta, SIGNED);

    if( real_identical (&val, &rival) && delta == 0 )
      {
      // val has no fractional part, which means delta is the exact integer
      // part of val.

      // And delta is zero.  This is a weird degenerate case.  But adding zero
      // to anything means we are already done.
      handled = true;
      return handled;
      }

    int  digits = C[0].refer.field->data.digits;
    int rdigits = C[0].refer.field->data.rdigits;

    if( digits == rdigits )
      {
      // This is another degenerate case.  We are being asked to add an integer
      // to a value whose PICTURE is something like V9999.  This is beyond our
      // capabilities.
      if( !subtracting )
        {
        ordinary_add_format_1(nC, C,
                             nA, A,
                             format,
                             error,
                             not_error,
                             compute_error );
        }
      else
        {
        ordinary_subtract_format_1(nC, C,
                                   nA, A,
                                   format,
                                   error,
                                   not_error,
                                   compute_error );
        }

      handled = true;
      return handled;
      }

    if( real_identical (&val, &rival)
        && delta >= -9
        && delta <=  9 )
      {
      delta = subtracting ? -delta : delta;

      // delta is a non-zero integer in the range of -9 to 9.
      tree tdelta = build_int_cst_type(UCHAR, delta);

      charmap_t *charmap =
                         __gg__get_charmap(C[0].refer.field->codeset.encoding);
      tzero = build_int_cst_type(UCHAR,
                                 charmap->mapped_character(ascii_zero));
      tnine = build_int_cst_type(UCHAR,
                                 charmap->mapped_character(ascii_nine));

      // Build up an integer constant for conveniently handling the various
      // PICTURE possibilities for a numeric display variable.
      typedef enum
        { UIT = 0, // unsignable, internal, trailing
          UIL = 1, // unsignable, internal, leading  (impossible)
          UST = 2, // unsignable, separate, trailing (impossible)
          USL = 3, // unsignable, separate, leading  (impossible)
          SIT = 4, // signable,   internal, trailing
          SIL = 5, // signable,   internal, leading
          SST = 6, // signable,   separate, trailing
          SSL = 7, // signable,   separate, leading
        } SIGN;
      int the_attributes =  ((C[0].refer.field->attr & signable_e) ? 4 : 0)
                          + ((C[0].refer.field->attr & separate_e) ? 2 : 0)
                          + ((C[0].refer.field->attr & leading_e ) ? 1 : 0) ;
      SIGN signbits = static_cast<SIGN>(the_attributes);

      // We need a pointer to the units digit of the data.  For a PIC 999v99
      // value of 123.45, we need a pointer to the '3':
      int units_offset = (signbits == SSL ? 1 : 0)
                       + C[0].refer.field->data.digits
                       - C[0].refer.field->data.rdigits
                       - 1;
      tree base;
      // Now and forever, base points to the data area of C[0]
      get_location(base, C[0].refer);

      tree units = gg_define_variable(UCHAR_P);
      // Now and forever, units points to the units digit of C[0]
      gg_assign(units, gg_add(base, build_int_cst_type(SIZE_T, units_offset)));

      // Now and forever, signloc points to the location of the byte containing
      // the sign information:
      int signloc_offset=0;
      switch(signbits)
        {
        case UIT:
        case UIL:
        case UST:
        case USL:
          signloc_offset = 0;
          break;
        case SIT:
          signloc_offset = digits-1;
          break;
        case SIL:
          signloc_offset = 0;
          break;
        case SST:
          signloc_offset = digits;
          break;
        case SSL:
          signloc_offset = 0;
          break;
        }
      tree counter = gg_define_variable(INT, digits - rdigits);
      tree pointer = gg_define_variable(UCHAR_P);
      gg_assign(pointer, units);
      if( !(C[0].refer.field->attr & signable_e) )
        {
        // The target is not signable
        if( delta >= 1 )
          {
          // Adding a positive number to an unsignable target.
          add_case_1(pointer,
                     tdelta,
                     charmap,
                     delta,
                     counter);
          handled = 1;
          }
        else
          {
          // Adding a negative number to an unsignable target.

          /* The edge case is when we are doing something like ADD 1 to 0.21,
             where the computed value is -0.79, and ends up in the target as
             0.79.  We don't try to do that, and instead pass off such
             calculations to the ordinary arithmetic routine.   */
          if( rdigits )
            {
            // When there are rdigits, we might have to call ordinary_add
            // First, we look at the rightmost units digit.
            IF( gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
                ge_op,
                tdelta )
              {
              // There will be no carry from the lowest order digit, so it is
              // safe to use add_case_2
              add_case_2(pointer,
                         tdelta,
                         counter,
                         digits - rdigits);
              }
            ELSE
              {
              // There will be a carry from the rightmost digit.  We have to
              // check the other digits.  If any are non-zero, then it is safe
              // to use add_case_2
              tree break_goto;
              tree break_label;
              gg_create_goto_pair(&break_goto,
                                  &break_label);
              WHILE( counter, gt_op, integer_one_node )
                {
                gg_decrement(pointer);
                IF( gg_indirect(pointer), ne_op, tzero )
                  {
                  // One of the left digits is non-zero, which means it is safe
                  // to use add_case_2()
                  gg_assign(counter, build_int_cst_type(INT, digits-rdigits));
                  gg_assign(pointer, units);
                  add_case_2(pointer,
                             tdelta,
                             counter,
                             digits - rdigits);
                  gg_append_statement(break_goto);
                  }
                ELSE
                  {
                  gg_decrement(counter);
                  }
                ENDIF
                }
              WEND
              // If you get here, that means we are adding a negative value
              // to something with rdigits and we have a carry from the
              // rightmost place, and all of the other digits are zero.
              if( !subtracting )
                {
                ordinary_add_format_1(nC, C,
                                     nA, A,
                                     format,
                                     error,
                                     not_error,
                                     compute_error );
                }
              else
                {
                ordinary_subtract_format_1(nC, C,
                                           nA, A,
                                           format,
                                           error,
                                           not_error,
                                           compute_error );
                }
              gg_append_statement(break_label);
              }
            ENDIF
            }
          else
            {
            add_case_2(pointer,
                       tdelta,
                       counter,
                       digits - rdigits);
            }
          handled = true;
          }
        }
      else if( signbits == SIT )
        {
        // The target is signable, and it is of the type PIC S9999, which is
        // the most common.

        tree signloc = gg_define_variable(UCHAR_P);
        gg_assign(signloc,
                  gg_add(base, build_int_cst_type(SIZE_T, signloc_offset)));
        if( delta >= 1 )
          {
          tree break_goto;
          tree break_label;
          gg_create_goto_pair(&break_goto,
                              &break_label);
          IF( gg_indirect(signloc), ge_op, tzero )
            {
            IF( gg_indirect(signloc), le_op, tnine )
              {
              // The signloc byte is between '0' and '9'.

              // We are adding a positive to a signable positive value
              // This is the same as adding a positive value to an unsignable
              // value:
              add_case_1(pointer,
                         tdelta,
                         charmap,
                         delta,
                         counter);
              gg_append_statement(break_goto);
              }
            ELSE
              {
              }
            ENDIF
            }
          ELSE
            {
            }
          ENDIF
          /* We are adding a positive value to a negative value. */

          IF( gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
              gt_op,
              tdelta )
            {
            // The rightmost digit is bigger than tdelta, so it's safe to use
            // the fast routine, because the result will stay negative.
            add_case_3(pointer,
                      tdelta,
                      counter);
            gg_append_statement(break_goto);
            }
          ELSE
            {
            }
          ENDIF
          // Either the rightmost digit is zero, or there will be a carry.
          // If any of the remaining digits is non-zero, then the result will
          // stay negative.

          WHILE( counter, gt_op, integer_one_node )
            {
            gg_decrement(pointer);
            IF( gg_indirect(pointer), ne_op, tzero )
              {
              // One of the remaining digits is non-zero, so we can still
              // use the fast routine:
              gg_assign(counter, build_int_cst_type(INT, digits-rdigits));
              gg_assign(pointer, units);
              add_case_3(pointer,
                        tdelta,
                        counter);
              gg_append_statement(break_goto);
              }
            ELSE
              {
              }
            ENDIF
            gg_decrement(counter);
            }
          WEND
          // since we are doing something like ADD 1 to -00.21, we need to
          // use ordinary arithmetic to cope with the switch to +00.79
          if( !subtracting )
            {
            ordinary_add_format_1(nC, C,
                                 nA, A,
                                 format,
                                 error,
                                 not_error,
                                 compute_error );
            }
          else
            {
            ordinary_subtract_format_1(nC, C,
                                       nA, A,
                                       format,
                                       error,
                                       not_error,
                                       compute_error );
            }
          gg_append_statement(break_label);
          }
        else
          {
          // We are adding a negative value to a signable value.

          tree break_goto;
          tree break_label;
          gg_create_goto_pair(&break_goto,
                              &break_label);

          IF( gg_indirect(signloc), ge_op, tzero )
            {
            IF( gg_indirect(signloc), le_op, tnine )
              {
              // The signloc byte is between '0' and '9'.
              // We are adding a negative value to a positive signable value.

              IF( gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
                  gt_op,
                  gg_negate(tdelta) )
                {
                // The units digit is non-zero, so we can use the same routine
                // we use for unsignable positives:
                add_case_2(pointer,
                           tdelta,
                           counter,
                           digits);
                gg_append_statement(break_goto);
                }
              ELSE
                {
                }
              ENDIF

              // The rightmost digit is zero.  Check the remaining digits of the
              // integer part:
              WHILE( counter, gt_op, integer_one_node )
                {
                gg_decrement(pointer);
                IF( gg_indirect(pointer), ne_op, tzero )
                  {
                  // One of the remaining digits is non-zero, so we can still
                  // use the fast routine:
                  gg_assign(counter, build_int_cst_type(INT, digits-rdigits));
                  gg_assign(pointer, units);
                  add_case_2(pointer,
                             tdelta,
                             counter,
                             digits);
                  gg_append_statement(break_goto);
                  }
                ELSE
                  {
                  }
                ENDIF
                gg_decrement(counter);
                }
              WEND
              // Arriving here means the integer part of the positive signable
              // is zero.
              // We are dealing with something like 00.21
              if( !subtracting )
                {
                ordinary_add_format_1(nC, C,
                                     nA, A,
                                     format,
                                     error,
                                     not_error,
                                     compute_error );
                }
              else
                {
                ordinary_subtract_format_1(nC, C,
                                           nA, A,
                                           format,
                                           error,
                                           not_error,
                                           compute_error );
                }
              gg_append_statement(break_goto);
              }
            ELSE
              {
              }
            ENDIF
            }
          ELSE
            {
            }
          ENDIF

          // We are adding a negative value to negative S9999

          add_case_4(pointer,
                     tdelta,
                     counter);

          // Special case:  When we do ADD -1 to -99.00, add_case_4 actually
          // comes back with -00.00.  So, we have to check the digits; when
          // they are all zero, we make the value positive.
          //if( rdigits )
            {
            gg_assign(units, gg_add(units, build_int_cst_type(SIZE_T, rdigits)));
            gg_assign(pointer, units);
            IF( gg_bitwise_and(gg_indirect(pointer), uchar_f_node),
                ne_op,
                build_int_cst_type(UCHAR, 0) )
              {
              gg_append_statement(break_goto);
              }
            ELSE
              {
              }
            ENDIF
            gg_assign(counter, build_int_cst_type(INT, digits));
            WHILE( counter, gt_op, integer_one_node )
              {
              gg_decrement(pointer);
              IF( gg_indirect(pointer),
                  ne_op,
                  tzero )
                {
                gg_append_statement(break_goto);
                }
              ELSE
                {
                }
              ENDIF
              gg_decrement(counter);
              }
            WEND
            // Getting here means that we are looking at -negative zero.  Make
            // it positive
            gg_assign(gg_indirect(units), tzero);
            }
          gg_append_statement(break_label);
          }
        handled = true;
        }
      }
    }
  return handled;
  }

static bool
add_format_1( size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              cbl_arith_format_t format,
              cbl_label_t *error,
              cbl_label_t *not_error,
              tree         compute_error )
  {
  bool handled = false;
  if( format == no_giving_e )
    {
    // Fixed format 1
    handled = add_litN_to_numdisp( nC, C,
                                   nA, A,
                                   format,
                                   error,
                                   not_error,
                                   compute_error,
                                   false); // false means adding
    if( !handled )
      {
      ordinary_add_format_1(nC, C,
                           nA, A,
                           format,
                           error,
                           not_error,
                           compute_error );
      handled = true;
      }
    }

  return handled;
  }

static bool
add_format_2( size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              cbl_arith_format_t format,
              cbl_label_t *error,
              cbl_label_t *not_error,
              tree         compute_error )
  {
  bool handled = false;
  if( format == giving_e )
    {
    // Fixed format 2

    set_up_arithmetic_error_handler(error,
                                    not_error);
    // Do phase 1, which calculates the subtotal and puts it into a
    // temporary location
    arithmetic_operation( 0, NULL,
                          nA, A,
                          0, NULL,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__add_fixed_phase1");

    // Do phase 2, which puts the subtotal into each target location in turn
    for(size_t i=0; i<nC; i++)
      {
      arithmetic_operation( 1, &C[i],
                            0, NULL,
                            0, NULL,
                            format,
                            error,
                            not_error,
                            compute_error,
                            "__gg__fixed_phase2_assign_to_c");
      }
    arithmetic_error_handler( error,
                              not_error,
                              compute_error);

    handled = true;
    }
  return handled;
  }

static bool
add_format_3( size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              cbl_arith_format_t format,
              cbl_label_t *error,
              cbl_label_t *not_error,
              tree         compute_error )
  {
  bool handled = false;
  if( format == corresponding_e )
    {
    // Fixed format 3
    gcc_assert(nA == nC);

    set_up_arithmetic_error_handler(error,
                                    not_error);
    arithmetic_operation(nC, C,
                          nA, A,
                          0, NULL,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__addf3");
    arithmetic_error_handler( error,
                              not_error,
                              compute_error);
    handled = true;
    }
  return handled;
  }

void
parser_add( size_t nC, cbl_num_result_t *C,
            size_t nA, cbl_refer_t *A,
            cbl_arith_format_t format,
            cbl_label_t *error,
            cbl_label_t *not_error,
            void        *compute_error_p ) // Cast this to a tree INT *
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    fprintf(stderr, " A[" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)nA);
    for(size_t i=0; i<nA; i++)
      {
      if(i > 0)
        {
        fprintf(stderr, ",");
        }
      fprintf(stderr, "%s", A[i].field->name);
      }

    fprintf(stderr, "%s", format==giving_e? " GIVING" : "");

    fprintf(stderr, " C[" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)nC);
    for(size_t i=0; i<nC; i++)
      {
      if(i > 0)
        {
        fprintf(stderr, ",");
        }
      fprintf(stderr, "%s", C[i].refer.field->name);
      }

    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  bool handled = fast_add(nC, C, nA, A, format, error, not_error) ;

  tree compute_error = (tree)compute_error_p;

  if( !handled )
    {
    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }

    // See if somebody in the addition is a float:
    handled = add_floats( nC, C,
                          nA, A,
                          format,
                          error,
                          not_error,
                          compute_error );
    }

  if( !handled )
    {
    handled = add_format_1(nC, C,
                           nA, A,
                           format,
                           error,
                           not_error,
                           compute_error );
    }

  if( !handled )
    {
    handled = add_format_2(nC, C,
                           nA, A,
                           format,
                           error,
                           not_error,
                           compute_error );
    }

  if( !handled )
    {
    handled = add_format_3(nC, C,
                           nA, A,
                           format,
                           error,
                           not_error,
                           compute_error );
    }

  gcc_assert( handled );
  }

void
parser_add( const cbl_refer_t& cref,
            const cbl_refer_t& aref,
            const cbl_refer_t& bref,
            cbl_round_t rounded)
  {
  // This is the simple and innocent C = A + B
  cbl_num_result_t C[1];
  C[0].rounded = rounded;
  C[0].refer = cref;

  cbl_refer_t A[2];
  A[0] = aref;
  A[1] = bref;

  parser_add( 1, C,
              2, A,
              giving_e,
              NULL,
              NULL );
  }

void
parser_multiply(size_t nC, cbl_num_result_t *C,
                size_t nA, cbl_refer_t *A,
                size_t nB, cbl_refer_t *B,
                cbl_label_t *error,
                cbl_label_t *not_error,
                void *compute_error_p ) // This is a pointer to an int
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  if( !error && !not_error && fast_multiply(nC, C,
                                            nA, A,
                                            nB, B) )
    {

    }
  else
    {
    tree compute_error = (tree)compute_error_p;

    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }

    if( nB == 0 )
      {
      // This is a FORMAT 1 multiply

      set_up_arithmetic_error_handler(error,
                                      not_error);
      // Phase 1 just converts identifier 1 to its intermediate form
      arithmetic_operation( 0, NULL,
                            nA, A,
                            0, NULL,
                            not_expected_e,
                            error,
                            not_error,
                            compute_error,
                            "__gg__multiplyf1_phase1");

      // Phase2 multiplies the intermediate by each destination in turn
      for(size_t i=0; i<nC; i++)
        {
        arithmetic_operation( 1, &C[i],
                              0, NULL,
                              0, NULL,
                              not_expected_e,
                              error,
                              not_error,
                              compute_error,
                              "__gg__multiplyf1_phase2");
        }
      arithmetic_error_handler( error,
                                not_error,
                                compute_error);

      }
    else
      {
      // This is a FORMAT 2 multiply
      set_up_arithmetic_error_handler(error,
                                      not_error);
      arithmetic_operation( nC, C,
                            nA, A,
                            nB, B,
                            not_expected_e,
                            error,
                            not_error,
                            compute_error,
                            "__gg__multiplyf2");
      arithmetic_error_handler( error,
                                not_error,
                                compute_error);
      }
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("result operand C[0]: ", C[0].refer.field, "");
    TRACE1_END
    }
  }

void
parser_divide(  size_t nC, cbl_num_result_t *C,  // C = A / B
                size_t nA, cbl_refer_t *A,
                size_t nB, cbl_refer_t *B,
                cbl_refer_t remainder,
                cbl_label_t *error,
                cbl_label_t *not_error,
                void *compute_error_p ) // This is a pointer to an int
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  if( !error && !not_error && fast_divide(nC, C,
                                          nA, A,
                                          nB, B,
                                          remainder) )
    {
    // We were able to do a fast divide operation.
    }
  else
    {
    tree compute_error = (tree)compute_error_p;

    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }

    if( nB == 0 && !remainder.field )
      {
      // This is a format 1 division
      set_up_arithmetic_error_handler(error,
                                      not_error);
      arithmetic_operation(0, NULL,
                            nA, A,
                            0, NULL,
                            not_expected_e,
                            NULL,
                            NULL,
                            compute_error,
                            "__gg__multiplyf1_phase1");

      for(size_t i=0; i<nC; i++)
        {
        arithmetic_operation(1, &C[i],
                              0, NULL,
                              0, NULL,
                              not_expected_e,
                              error,
                              not_error,
                              compute_error,
                              "__gg__dividef1_phase2");
        }
      arithmetic_error_handler( error,
                                not_error,
                                compute_error);
      }

    if( nB && !remainder.field )
      {
      // This is a format 2/3 division
      set_up_arithmetic_error_handler(error,
                                      not_error);
      arithmetic_operation(nC, C,
                            1,  A,
                            1,  B,
                            not_expected_e,
                            error,
                            not_error,
                            compute_error,
                            "__gg__dividef23");

      arithmetic_error_handler( error,
                                not_error,
                                compute_error);
      }

    if( remainder.field )
      {
      // This is a format 4/5 division
      set_up_arithmetic_error_handler(error,
                                      not_error);
      arithmetic_operation(1,  C,
                            1,  A,
                            1,  B,
                            not_expected_e,
                            error,
                            not_error,
                            compute_error,
                            "__gg__dividef45",
                            &remainder);

      arithmetic_error_handler( error,
                                not_error,
                                compute_error);
      }
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  }

void
parser_multiply(const cbl_refer_t& cref,
                const cbl_refer_t& aref,
                const cbl_refer_t& bref,
                cbl_round_t rounded )
  {
  cbl_num_result_t C[1];
  C[0].rounded = rounded;
  C[0].refer = cref;

  cbl_refer_t A[1];
  A[0] = aref;

  cbl_refer_t B[1];
  B[0] = bref;

  parser_multiply(1, C,
                  1, B,
                  1, A,
                  NULL,
                  NULL );
  }

void
parser_divide(  const cbl_refer_t& cref,
                const cbl_refer_t& aref,
                const cbl_refer_t& bref,
                cbl_round_t rounded,
                const cbl_refer_t& remainder_ref )
  {
  cbl_num_result_t C[1];
  C[0].rounded = rounded;
  C[0].refer = cref;

  cbl_refer_t A[1];
  A[0] = aref;

  cbl_refer_t B[1];
  B[0] = bref;

  parser_divide(  1, C,
                  1, A,
                  1, B,
                  remainder_ref,
                  NULL,
                  NULL );
  }

void
parser_compute( cbl_refer_t *tgt,
                const std::deque<rpn_t>& operations,
                cbl_label_t * compute_error_label )
  {
  RETURN_IF_PARSE_ONLY;
  CHECK_FIELD(tgt->field);
  gcc_assert(operations.size());

  set_up_compute_error_label(compute_error_label);

  gg_assign(var_decl_default_compute_error, integer_zero_node);
  tree compute_error =    compute_error_label
                        ? gg_get_address_of( compute_error_label->
                                             structs.compute_error->
                                             compute_error_code)
                        : gg_get_address_of(var_decl_default_compute_error) ;

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_REF(" dest: ", *tgt);
    char *psz;
    for(auto op : operations)
      {
      if( op.op )
        {
        psz = xasprintf("%c", op.op);
        }
      else
        {
        if( !op.term.field )
          {
          psz = xasprintf("both op.op and op.term.field are NULL");
          }
        else
          {
          psz = xasprintf("%s", op.term.field->name);
          }
        }

      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(psz);
      free(psz);
      }
    SHOW_PARSE_END
    }

  std::string opstring;
  std::vector<const cbl_field_t *>fields;
  std::vector<tree> offsets;

  //bool compute_float = false;
  const char *routine = "__gg__compute_fixed";
  for(auto op : operations)
    {
    if(    op.op == '^'
        || (op.term.field && op.term.field->type == FldFloat) )
      {
      //compute_float = true;
      routine = "__gg__compute_float";
      }
    opstring += op.op ? op.op : ascii_P;
    fields.push_back(op.term.field);
    offsets.push_back(refer_offset(op.term));
    }

  // The final element on the stacks is for the destination field:
  fields.push_back(tgt->field);
  offsets.push_back(refer_offset(*tgt));

  tree retval = gg_define_variable(INT);
  gg_assign(retval,
            gg_call_expr( INT,
                          routine,
                          gg_string_literal(opstring.c_str()),
                          gg_array_of_field_pointers(fields),
                          gg_array_of_uchar_p(offsets),
                          NULL_TREE));
  gg_assign(gg_indirect(compute_error), retval);
  }

void
parser_compute( std::vector<cbl_num_result_t>& results,
                const std::deque<rpn_t>& operations,
                cbl_label_t *on_error,
                cbl_label_t *not_error,
                cbl_label_t *compute_error)
  {
  bool compute_float = false;
  for(auto op : operations)
    {
    if(    op.op == '^'
        || (op.term.field && op.term.field->type == FldFloat) )
      {
      compute_float = true;
      break;
      }
    }

  cbl_field_t *temp_field;

  if( compute_float )
    {
    temp_field = new_temporary(FldFloat,
                               nullptr,
                               intermediate_e);
    }
  else
    {
    temp_field = new_temporary(FldNumericBin5,
                               nullptr,
                               signable_e);
    }
  cbl_refer_t  temp_refer;
  temp_refer.field = temp_field;

  parser_compute( &temp_refer, operations, compute_error );

  if( !results.empty() )
    {
    parser_assign(results.size(),
                  results.data(),
                  temp_refer,
                  on_error,
                  not_error,
                  compute_error);
    }
  }

void
parser_op( struct cbl_refer_t cref,
           struct cbl_refer_t aref,
           int op,
           struct cbl_refer_t bref,
           struct cbl_label_t *compute_error_label)
  {
  Analyze();
  set_up_compute_error_label(compute_error_label);

  gg_assign(var_decl_default_compute_error, integer_zero_node);
  tree compute_error =    compute_error_label
                        ? gg_get_address_of( compute_error_label->
                                             structs.compute_error->
                                             compute_error_code)
                        : gg_get_address_of(var_decl_default_compute_error) ;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_REF(" ", cref)
    SHOW_PARSE_TEXT(" = ")
    SHOW_PARSE_REF("", aref)
    char ach[4] = "   ";
    ach[1] = op;
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_REF("", bref)
    SHOW_PARSE_END
    }

  // We have to do the trace in before/after mode; parser_op(a, a, op, a)
  // is a legitimate call.
  TRACE1
    {
    TRACE1_HEADER
    char ach[4] = "   ";
    ach[1] = op;
    TRACE1_TEXT_ABC("operation is \"", ach, "\"")
    TRACE1_INDENT
    TRACE1_REFER("operand A: ", aref, "")
    TRACE1_INDENT
    TRACE1_REFER("operand B: ", bref, "")
    TRACE1_INDENT
    TRACE1_TEXT_ABC("result will be ", cref.field->name, "")
    TRACE1_END
    }

  struct cbl_num_result_t for_call = {};
  for_call.rounded = truncation_e;
  for_call.refer = cref;

  switch(op)
    {
    case '+':
      {
      cbl_refer_t A[2];
      A[0] = aref;
      A[1] = bref;
      parser_add( 1, &for_call,
                  2, A,
                  giving_e,
                  NULL,
                  NULL,
                  compute_error );
      break;
      }

    case '-':
      {
      cbl_refer_t A[1];
      cbl_refer_t B[1];
      A[0] = bref;
      B[0] = aref;
      // Yes, the A-ness and B-ness are not really consistent
      parser_subtract(1, &for_call,
                      1, A,
                      1, B,
                      giving_e,
                      NULL,
                      NULL,
                      compute_error );
      break;
      }

    case '*':
      {
      cbl_refer_t A[1];
      cbl_refer_t B[1];
      A[0] = bref;
      B[0] = aref;
      parser_multiply(1, &for_call,
                      1, A,
                      1, B,
                      NULL,
                      NULL,
                      compute_error );
      break;
      }

    case '/':
      {
      cbl_refer_t A[1];
      cbl_refer_t B[1];
      A[0] = aref;
      B[0] = bref;
      parser_divide(1, &for_call,
                    1, A,
                    1, B,
                    NULL,
                    NULL,
                    NULL,
                    compute_error );
      break;
      }

    case '^':
      {
      arithmetic_operation(   1, &for_call,
                              1, &aref,
                              1, &bref,
                              no_giving_e,
                              NULL,
                              NULL,
                              compute_error,
                              "__gg__pow",
                              NULL);
      break;
      }
    default:
      cbl_internal_error( "%<parser_op()%> doesn%'t know how to "
                          "evaluate %<%s = %s %c %s%>",
                          cref.field->name,
                          aref.field->name,
                          op,
                          bref.field->name);
      break;
    }
  }

static bool
subtract_floats( size_t nC, cbl_num_result_t *C,
            size_t nA, cbl_refer_t *A,
            size_t nB, cbl_refer_t *B,
            cbl_arith_format_t format,
            cbl_label_t *error,
            cbl_label_t *not_error,
            tree         compute_error )
  {
  bool handled = false;

  bool computation_is_float =    is_somebody_float(nA, A)
                              || is_somebody_float(nC, C);

  // We now start deciding which arithmetic routine we are going to use:

  if( computation_is_float )
    {
    switch( format )
      {
      case no_giving_e:
        {
        // Float format 1

        set_up_arithmetic_error_handler(error,
                                        not_error);
        // Do phase 1, which calculates the subtotal and puts it into a
        // temporary location
        arithmetic_operation( 0, NULL,
                              nA, A,
                              0, NULL,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__add_float_phase1");

        // Do phase 2, which subtracts the subtotal from each target in turn
        for(size_t i=0; i<nC; i++)
          {
          arithmetic_operation(1, &C[i],
                                0, NULL,
                                0, NULL,
                                format,
                                error,
                                not_error,
                                compute_error,
                                "__gg__subtractf1_float_phase2");
          }
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;

        break;
        }

      case giving_e:
        {
        // Float SUBTRACT Format 2

        gcc_assert(nB == 1);
        set_up_arithmetic_error_handler(error,
                                        not_error);
        // Do phase 1, which calculates the subtotal and puts it into a
        // temporary location
        arithmetic_operation( 0, NULL,
                              nA, A,
                              nB, B,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__subtractf2_float_phase1");

        // Do phase 2, which puts the subtotal into each target location in turn
        for(size_t i=0; i<nC; i++)
          {
          arithmetic_operation(1, &C[i],
                                0, NULL,
                                0, NULL,
                                format,
                                error,
                                not_error,
                                compute_error,
                                "__gg__float_phase2_assign_to_c");
          }
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;
        break;
        }

      case corresponding_e:
        {
        // Float format 3
        gcc_assert(nA == nC);

        set_up_arithmetic_error_handler(error,
                                        not_error);
        arithmetic_operation(nC, C,
                              nA, A,
                              0, NULL,
                              format,
                              error,
                              not_error,
                              compute_error,
                              "__gg__subtractf3");
        arithmetic_error_handler( error,
                                  not_error,
                                  compute_error);

        handled = true;

        break;
        }

      case not_expected_e:
        gcc_unreachable();
        break;
      }
    }
  return handled;
  }

static bool
subtract_format_1(size_t nC, cbl_num_result_t *C,
                  size_t nA, cbl_refer_t *A,
                  cbl_arith_format_t format,
                  cbl_label_t *error,
                  cbl_label_t *not_error,
                  tree         compute_error )
  {
  bool handled = false;
  if(format == no_giving_e)
    {
    // Fixed format 1
    handled = add_litN_to_numdisp( nC, C,
                                   nA, A,
                                   format,
                                   error,
                                   not_error,
                                   compute_error,
                                   true); // false means subtraction
    if( !handled )
      {
      ordinary_subtract_format_1(nC, C,
                                 nA, A,
                                 format,
                                 error,
                                 not_error,
                                 compute_error );
      handled = true;
      }
    }
  return handled;
  }

static bool
subtract_format_2(size_t nC, cbl_num_result_t *C,
                  size_t nA, cbl_refer_t *A,
                  size_t nB, cbl_refer_t *B,
                  cbl_arith_format_t format,
                  cbl_label_t *error,
                  cbl_label_t *not_error,
                  tree         compute_error )
  {
  bool handled = false;
  if(format == giving_e)
    {
    // Fixed SUBTRACT Format 2

    gcc_assert(nB == 1);
    set_up_arithmetic_error_handler(error,
                                    not_error);
    // Do phase 1, which calculates the subtotal and puts it into a
    // temporary location
    arithmetic_operation( 0, NULL,
                          nA, A,
                          nB, B,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__subtractf2_fixed_phase1");

    // Do phase 2, which puts the subtotal into each target location in turn
    for(size_t i=0; i<nC; i++)
      {
      arithmetic_operation( 1, &C[i],
                            0, NULL,
                            0, NULL,
                            format,
                            error,
                            not_error,
                            compute_error,
                            "__gg__fixed_phase2_assign_to_c");
      }
    arithmetic_error_handler( error,
                              not_error,
                              compute_error);

    handled = true;
    }
  return handled;
  }

static bool
subtract_format_3(size_t nC, cbl_num_result_t *C,
                  size_t nA, cbl_refer_t *A,
                  cbl_arith_format_t format,
                  cbl_label_t *error,
                  cbl_label_t *not_error,
                  tree         compute_error )
  {
  bool handled = false;
  if( format == corresponding_e )
    {
    // Fixed format 3
    gcc_assert(nA == nC);

    set_up_arithmetic_error_handler(error,
                                    not_error);
    arithmetic_operation(nC, C,
                          nA, A,
                          0, NULL,
                          format,
                          error,
                          not_error,
                          compute_error,
                          "__gg__subtractf3");
    arithmetic_error_handler( error,
                              not_error,
                              compute_error);

    handled = true;
    }
  return handled;
  }

void
parser_subtract(size_t nC, cbl_num_result_t *C, // C = B - A
                size_t nA, cbl_refer_t *A,
                size_t nB, cbl_refer_t *B,
                cbl_arith_format_t format,
                cbl_label_t *error,
                cbl_label_t *not_error,
                void        *compute_error_p ) // Cast this to a tree / int *
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    fprintf(stderr, " A[" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)nA);
    for(size_t i=0; i<nA; i++)
      {
      if(i > 0)
        {
        fprintf(stderr, ",");
        }
      fprintf(stderr, "%s", A[i].field->name);
      }

    fprintf(stderr, " B[" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)nB);
    for(size_t i=0; i<nB; i++)
      {
      if(i > 0)
        {
        fprintf(stderr, ",");
        }
      fprintf(stderr, "%s", B[i].field->name);
      }

    fprintf(stderr, " C[" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)nC);
    for(size_t i=0; i<nC; i++)
      {
      if(i > 0)
        {
        fprintf(stderr, ",");
        }
      fprintf(stderr, "%s", C[i].refer.field->name);
      }

    SHOW_PARSE_END
    }

  //  We are going to look for configurations that allow us to do binary
  //  arithmetic and quickly assign the results:

  bool handled = fast_subtract( nC, C,
                                nA, A,
                                nB, B,
                                format,
                                error,
                                not_error) ;
  tree compute_error = (tree)compute_error_p;
  if( !handled )
    {
    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }
    handled = subtract_floats(nC, C,
                              nA, A,
                              nB, B,
                              format,
                              error,
                              not_error,
                              compute_error );
    }

  if(!handled)
    {
    handled = subtract_format_1( nC, C,
                                 nA, A,
                                 format,
                                 error,
                                 not_error,
                                 compute_error );
    }

  if(!handled)
    {
    handled = subtract_format_2( nC, C,
                                 nA, A,
                                 nB, B,
                                 format,
                                 error,
                                 not_error,
                                 compute_error );
    }

  if(!handled)
    {
    handled = subtract_format_3( nC, C,
                                 nA, A,
                                 format,
                                 error,
                                 not_error,
                                 compute_error );
    }

  gcc_assert(handled);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  }

void
parser_subtract(const cbl_refer_t& cref, // cref = aref - bref
                const cbl_refer_t& aref,
                const cbl_refer_t& bref,
                cbl_round_t rounded )
  {
  cbl_num_result_t C[1];
  C[0].rounded = rounded;
  C[0].refer = cref;

  cbl_refer_t A[1];
  A[0] = aref;

  cbl_refer_t B[1];
  B[0] = bref;

  parser_subtract(1, C,   // Beware: C = A - B, but the order has changed
                  1, B,
                  1, A,
                  giving_e,
                  NULL,
                  NULL );
  }
