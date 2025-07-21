/*
 * Copyright (c) 2021-2025 Symas Corporation
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
#include "show_parse.h"

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
        = gg_define_int(0);
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
arithmetic_operation(size_t nC, cbl_num_result_t *C,
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

  if( nC+1 <= MIN_FIELD_BLOCK_SIZE )
    {
    // We know there is room in our existing buffer
    }
  else
    {
    // We might have to allocate more space:
    gg_call(VOID,
            "__gg__resize_int_p",
            gg_get_address_of(var_decl_arithmetic_rounds_size),
            gg_get_address_of(var_decl_arithmetic_rounds),
            build_int_cst_type(SIZE_T, nC+1),
            NULL_TREE);
    }

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
    temp_field.data.memsize   = remainder->field->data.memsize ;
    temp_field.data.capacity  = remainder->field->data.capacity;
    temp_field.data.digits    = remainder->field->data.digits  ;
    temp_field.data.rdigits   = remainder->field->data.rdigits ;
    temp_field.data.initial   = remainder->field->data.initial ;
    temp_field.data.picture   = remainder->field->data.picture ;
    parser_symbol_add(&temp_field);
    temp_remainder.field = &temp_field;

    // For division, the optional remainder goes onto the beginning of the
    // list
    results[ncount++] = temp_remainder;
    }
  for(size_t i=0; i<nC; i++)
    {
    results[ncount] = C[i].refer;
    gg_assign(  gg_array_value(var_decl_arithmetic_rounds, ncount),
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

  build_array_of_treeplets(1, nA, A);
  build_array_of_treeplets(2, nB, B);
  build_array_of_treeplets(3, ncount, results.data());

  gg_call(VOID,
          operation,
          build_int_cst_type(INT, format),
          build_int_cst_type(SIZE_T, nA),
          build_int_cst_type(SIZE_T, nB),
          build_int_cst_type(SIZE_T, ncount),
          var_decl_arithmetic_rounds,
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
all_results_binary(size_t nC, const cbl_num_result_t *C)
  {
  bool retval = true;

  for(size_t i=0; i<nC; i++)
    {
    if(C[i].refer.field->data.digits != 0 || C[i].refer.field->type == FldFloat )
      {
      retval = false;
      break;
      }
    }
  return retval;
  }

static tree
largest_binary_term(size_t nA, cbl_refer_t *A)
  {
  tree retval = NULL_TREE;
  uint32_t max_capacity = 0;
  int      is_negative  = 0;

  for(size_t i=0; i<nA; i++)
    {
    if( A[i].field->data.rdigits || A[i].field->type == FldFloat )
      {
      // We are prepared to work only with integers
      retval = NULL_TREE;
      break;
      }
    if(    A[i].field->type == FldLiteralN
//        || A[i].field->type == FldNumericDisplay
        || A[i].field->type == FldNumericBinary
        || A[i].field->type == FldNumericBin5
        || A[i].field->type == FldIndex
        || A[i].field->type == FldPointer  )
      {
      // This is an integer type that can be worked with quickly
      is_negative |= ( A[i].field->attr & signable_e );
      max_capacity = std::max(max_capacity, A[i].field->data.capacity);
      retval = tree_type_from_size(max_capacity, is_negative);
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
          size_t nA, cbl_refer_t *A,
          cbl_arith_format_t format )
  {
  bool retval = false;
  if( all_results_binary(nC, C) )
    {
    Analyze();
    // All targets are non-PICTURE binaries:
    //gg_insert_into_assembler("# DUBNER addition START");
    tree term_type = largest_binary_term(nA, A);
    if( term_type )
      {
      // All the terms are things we can work with.

      // We need to calculate the sum of all the A[] terms using term_type as
      // the intermediate type:

      tree sum     = gg_define_variable(term_type);
      tree addend  = gg_define_variable(term_type);
      get_binary_value( sum,
                        NULL,
                        A[0].field,
                        refer_offset(A[0]));

      // Add in the rest of them:
      for(size_t i=1; i<nA; i++)
        {
        get_binary_value( addend,
                          NULL,
                          A[i].field,
                          refer_offset(A[i]));
        gg_assign(sum, gg_add(sum, addend));
        }
      //gg_printf("The intermediate sum is %ld\n", gg_cast(LONG, sum), NULL_TREE);

      // We now either accumulate into C[n] or assign to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type = tree_type_from_size(C[i].refer.field->data.capacity, 0);
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
          // We are accumulating
          gg_assign(  gg_indirect(ptr),
                      gg_add( gg_indirect(ptr),
                              gg_cast(dest_type, sum)));
          }
        }
      retval = true;
      }

    //gg_insert_into_assembler("# DUBNER addition END ");
    }
  return retval;
  }

static bool
fast_subtract(size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              size_t nB, cbl_refer_t *B,
              cbl_arith_format_t format)
  {
  bool retval = false;
  if( all_results_binary(nC, C) )
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

      // We need to calculate the sum of all the A[] terms using term_type as
      // the intermediate type:

      tree sum     = gg_define_variable(term_type);
      tree addend  = gg_define_variable(term_type);
      get_binary_value(sum, NULL, A[0].field, refer_offset(A[0]));

      // Add in the rest of them:
      for(size_t i=1; i<nA; i++)
        {
        get_binary_value(sum, NULL, A[i].field, refer_offset(A[i]));
        gg_assign(sum, gg_add(sum, addend));
        }
      //gg_printf("The intermediate sum is %ld\n", gg_cast(LONG, sum), NULL_TREE);

      if( format == giving_e )
        {
        // We now subtract the sum from B[0]
        get_binary_value(addend, NULL, B[0].field, refer_offset(B[0]));
        gg_assign(sum, gg_subtract(addend, sum));
        }

      // We now either accumulate into C[n] or assign to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type = tree_type_from_size(C[i].refer.field->data.capacity, 0);
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
      retval = true;
      }
    }
  return retval;
  }

static bool
fast_multiply(size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              size_t nB, cbl_refer_t *B)
  {
  bool retval = false;
  if( all_results_binary(nC, C) )
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

      tree valA    = gg_define_variable(term_type);
      tree valB    = gg_define_variable(term_type);
      get_binary_value(valA, NULL, A[0].field, refer_offset(A[0]));

      if( nB )
        {
        // This is a MULTIPLY Format 2
        get_binary_value(valB, NULL, B[0].field, refer_offset(B[0]));
        gg_assign(valA, gg_multiply(valA, valB));
        }

      // We now either multiply into C[n] or assign A * B to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type = tree_type_from_size(C[i].refer.field->data.capacity, 0);
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
            size_t nA, cbl_refer_t *A,
            size_t nB, cbl_refer_t *B,
      const cbl_refer_t             &remainder)
  {
  bool retval = false;
  if( all_results_binary(nC, C) )
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

      tree divisor  = gg_define_variable(term_type);
      tree dividend = gg_define_variable(term_type);
      tree quotient = NULL_TREE;
      get_binary_value(divisor, NULL, A[0].field, refer_offset(A[0]));

      if( nB )
        {
        // This is a MULTIPLY Format 2, where we are dividing A into B and
        // assigning that to C
        get_binary_value(dividend, NULL, B[0].field, refer_offset(B[0]));

        quotient = gg_define_variable(term_type);
        // Yes, in this case the divisor and dividend are switched.  Things are
        // tough all over.
        gg_assign(quotient, gg_divide(divisor, dividend));
        }

      // We now either divide into C[n] or assign dividend/divisor to C[n]:
      for(size_t i=0; i<nC; i++ )
        {
        tree dest_type =
                       tree_type_from_size(C[i].refer.field->data.capacity, 0);
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
          dest_type = tree_type_from_size(remainder.field->data.capacity, 0);
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

void
parser_add( size_t nC, cbl_num_result_t *C,
            size_t nA, cbl_refer_t *A,
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

  bool handled = false;

  if( fast_add( nC, C,
                nA, A,
                format) )
    {
    handled = true;
    }
  else
    {
    tree compute_error = (tree)compute_error_p;
    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }

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
    else
      {
      switch( format )
        {
        case no_giving_e:
          {
          // Fixed format 1

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
                                  "__gg__addf1_fixed_phase2");
            }
          arithmetic_error_handler( error,
                                    not_error,
                                    compute_error);

          handled = true;
          break;
          }

        case giving_e:
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
            arithmetic_operation(1, &C[i],
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
          break;
          }

        case corresponding_e:
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
          break;
          }

        case not_expected_e:
          gcc_unreachable();
          break;
        }
      }
    }

  assert( handled );
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

  if( fast_multiply(nC, C,
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

  if( fast_divide(nC, C,
                  nA, A,
                  nB, B,
                  remainder) )
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

  //  no_giving_e is format 1; giving_e is format 2.

  bool handled = false;

  if( fast_subtract(nC, C,
                    nA, A,
                    nB, B,
                    format) )
    {
    handled = true;
    }
  else
    {
    tree compute_error = (tree)compute_error_p;
    if( compute_error == NULL )
      {
      gg_assign(var_decl_default_compute_error, integer_zero_node);
      compute_error = gg_get_address_of(var_decl_default_compute_error);
      }
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
                                  "__gg__fixed_phase2_assign_to_c");
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
    else
      {
      switch( format )
        {
        case no_giving_e:
          {
          // Fixed format 1

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

          handled = true;

          break;
          }

        case giving_e:
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
          break;
          }

        case corresponding_e:
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
          break;
          }

        case not_expected_e:
          gcc_unreachable();
          break;
        }
      }
    }

  if( !handled )
    {
    abort();
    }
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
