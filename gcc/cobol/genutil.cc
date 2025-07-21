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
#include "gengen.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "structs.h"
#include "../../libgcobol/gcobolio.h"
#include "../../libgcobol/charmaps.h"
#include "show_parse.h"
#include "../../libgcobol/exceptl.h"
#include "exceptg.h"

bool internal_codeset_is_ebcdic() { return gcobol_feature_internal_ebcdic(); }

bool exception_location_active = true;
bool skip_exception_processing = true;

bool suppress_dest_depends = false;

std::vector<std::string>current_filename;

tree var_decl_exception_code;         // int         __gg__exception_code;
tree var_decl_exception_file_status;  // int         __gg__exception_file_status;
tree var_decl_exception_file_name;    // const char *__gg__exception_file_name;
tree var_decl_exception_statement;    // const char *__gg__exception_statement;
tree var_decl_exception_source_file;  // const char *__gg__exception_source_file;
tree var_decl_exception_line_number;  // int         __gg__exception_line_number;
tree var_decl_exception_program_id;   // const char *__gg__exception_program_id;
tree var_decl_exception_section;      // const char *__gg__exception_section;
tree var_decl_exception_paragraph;    // const char *__gg__exception_paragraph;

tree var_decl_default_compute_error;  // int         __gg__default_compute_error;
tree var_decl_rdigits;                // int         __gg__rdigits;
tree var_decl_unique_prog_id;         // size_t      __gg__unique_prog_id;

tree var_decl_entry_location;         // This is for managing ENTRY statements
tree var_decl_exit_address;           // This is for implementing pseudo_return_pop

tree var_decl_call_parameter_signature; // char   *__gg__call_parameter_signature
tree var_decl_call_parameter_count;     // int __gg__call_parameter_count
tree var_decl_call_parameter_lengths;   // size_t *__gg__call_parameter_count

tree var_decl_return_code;             // short __gg__data_return_code

tree var_decl_arithmetic_rounds_size;  // size_t __gg__arithmetic_rounds_size;
tree var_decl_arithmetic_rounds;       // int*   __gg__arithmetic_rounds;
tree var_decl_fourplet_flags_size;     // size_t __gg__fourplet_flags_size;
tree var_decl_fourplet_flags;          // int*   __gg__fourplet_flags;

tree var_decl_treeplet_1f; // cblc_field_pp_type_node , "__gg__treeplet_1f"
tree var_decl_treeplet_1o; // SIZE_T_P                , "__gg__treeplet_1o"
tree var_decl_treeplet_1s; // SIZE_T_P                , "__gg__treeplet_1s"
tree var_decl_treeplet_2f; // cblc_field_pp_type_node , "__gg__treeplet_2f"
tree var_decl_treeplet_2o; // SIZE_T_P                , "__gg__treeplet_2o"
tree var_decl_treeplet_2s; // SIZE_T_P                , "__gg__treeplet_2s"
tree var_decl_treeplet_3f; // cblc_field_pp_type_node , "__gg__treeplet_3f"
tree var_decl_treeplet_3o; // SIZE_T_P                , "__gg__treeplet_3o"
tree var_decl_treeplet_3s; // SIZE_T_P                , "__gg__treeplet_3s"
tree var_decl_treeplet_4f; // cblc_field_pp_type_node , "__gg__treeplet_4f"
tree var_decl_treeplet_4o; // SIZE_T_P                , "__gg__treeplet_4o"
tree var_decl_treeplet_4s; // SIZE_T_P                , "__gg__treeplet_4s"

// There are times when I need to insert a NOP into the code, mainly to force
// a .loc directive into the assembly language so that the GDB-COBOL debugger
// can show the COBOL source code.  This is true, for example, the CONTINUE
// statement which otherwise would produce no assembly language. Since I
// wasn't successful figuring out how to create an actual NOP assembly language
// instruction, I instead gg_assign(var_decl_nop, integer_zero_node)
tree var_decl_nop;                // int         __gg__nop;
tree var_decl_main_called;        // int         __gg__main_called;

#if 0
#define REFER(a)
#else
#define REFER(a) do                                \
  {                                             \
  if( getenv("REFER") )                         \
    {                                           \
    fprintf(stderr, "REFER %s %s\n", __func__, a);    \
    }                                           \
  }while(0);
#endif

int
get_scaled_rdigits(cbl_field_t *field)
  {
  int retval;
  if( !(field->attr & scaled_e) )
    {
    // The value is not P-scaled, so we just use the unchanged rdigits value
    retval = field->data.rdigits;
    }
  else
    {
    if( field->data.rdigits < 0 )
      {
      // The PIC string was something like 999PPPP, which means an rdigits value
      // of -4.  We return zero; somebody else will have the job of multiplying
      // the three significant digits by 10^4 to get the magnitude correct.
      retval = 0;
      }
    else
      {
      // The PIC string was something like PPPP999, which means an rdigits value
      // of +4.  We return an rdigits value of 4 + 3 = 7, which will mean that
      // the three significant digits will be scaled to 0.0000999
      retval = field->data.digits + field->data.rdigits;
      }
    }
  return retval;
  }

int
get_scaled_digits(cbl_field_t *field)
  {
  int retval;
  if( !(field->attr & scaled_e) )
    {
    // The value is not P-scaled, so we just use the unchanged rdigits value
    retval = field->data.digits;
    }
  else
    {
    if( field->data.rdigits < 0 )
      {
      // The PIC string was something like 999PPPP, which means an rdigits value
      // of -4.  digits is 3, reflecting the 9(3).  We return seven, reflecting
      // that all of the final digits are to the left of the decimal point
      retval = field->data.digits - field->data.rdigits;
      }
    else
      {
      // The PIC string was something like PPPP999, which means an rdigits value
      // of +4.  We return and rdigits value of 4 + 3 = 7, which will mean that
      // the three significant digits will be scaled to 0.0000999 and all of the
      // seven digits are to the left of the decimal point
      retval = field->data.digits + field->data.rdigits;
      }
    }
  return retval;
  }

tree
tree_type_from_digits(size_t digits, int signable)
  {
  tree retval = NULL_TREE;

  if( signable )
    {
    if(digits <= 2 )
      {
      retval = CHAR;
      }
    else if (digits <= 4 )
      {
      retval = SHORT;
      }
    else if (digits <= 9 )
      {
      retval = INT;
      }
    else if (digits <= 18 )
      {
      retval = LONGLONG;
      }
    else
      {
      retval = INT128;
      }
    }
  else
    {
    if(digits <= 2 )
      {
      retval = UCHAR;
      }
    else if (digits <= 4 )
      {
      retval = USHORT;
      }
    else if (digits <= 9 )
      {
      retval = UINT;
      }
    else if (digits <= 18 )
      {
      retval = ULONGLONG;
      }
    else
      {
      retval = UINT128;
      }
    }
  return retval;
  }

void
get_integer_value(tree value, // We know this is a LONG
                  cbl_field_t *field,
                  tree         offset,
                  bool check_for_fractional_digits)
  {
  if( field->type == FldLiteralN && field->data.rdigits==0 )
    {
    gg_assign(value, gg_cast(LONG, field->data_decl_node));
    return;
    }

  Analyze();
  // Call this routine when you know the result has to be an integer with no
  // rdigits.  This routine became necessary the first time I saw an
  // intermediate value for an array subscript:  table((3 + 1) / 2))
  //
  // If the field_i has rdigits, and if any of those rdigits are non-zero, we
  // return a 1 so that our caller can decide what to do.

  static tree temp    = gg_define_variable(INT128, "..giv_temp",    vs_file_static);
  static tree rdigits = gg_define_variable(INT,    "..giv_rdigits", vs_file_static);

  if( field->attr & intermediate_e )
    {
    // Get the binary value, which for 99V99 can be 1234, meaning 12.34
    get_binary_value(temp, NULL, field, offset);

    // Pick up the run-time number of rdigits:
    gg_assign(rdigits, gg_cast(INT, member(field, "rdigits")));

    // Scale by the number of rdigits, which turns 12.34 into 12.
    // When check_for_fractional_digits is true, __gg__rdigits will be set
    // to 1 for 12.34, and will be set to zero 12.00
    scale_by_power_of_ten(temp,
                          gg_negate(rdigits),
                          check_for_fractional_digits);
    }
  else
    {
    get_binary_value(temp, rdigits, field, offset);
    scale_by_power_of_ten_N(temp,
                            -get_scaled_rdigits(field),
                            check_for_fractional_digits);
    }
  gg_assign(value, gg_cast(TREE_TYPE(value), temp));
  }

static
tree  // This is a SIZE_T
get_any_capacity(cbl_field_t *field)
  {
  if( field->attr & (any_length_e | intermediate_e) )
    {
    return member(field->var_decl_node, "capacity");
    }
  else
    {
    return build_int_cst_type(SIZE_T, field->data.capacity);
    }
  }

/*  This routine, used by both get_data_offset and refer_refmod_length,
    fetches the refmod_from and refmod_length.  If ec-bound-ref-mod checking
    is enabled, it does those checks and sets the exception condition when they
    are violated.

    The return value for refstart is the actual offset, that is val(7:3) returns
    the value 7-1, that is, 6.
    */
static
void
get_and_check_refstart_and_reflen(  tree         refstart,// LONG returned value
                                    tree         reflen,  // LONG returned value
                              const cbl_refer_t &refer)
  {
  const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );

  if( !enabled_exceptions.match(ec_bound_ref_mod_e) )
    {
    // This is normal operation -- no exception checking.  Thus, we won't
    // be trying to check for boundaries or integerness.  And the programmer
    // is accepting the responsibility for bad code:  "If you specify
    // disaster, disaster is what you get."

    get_integer_value(refstart,
                      refer.refmod.from->field,
                      refer_offset(*refer.refmod.from));
    gg_decrement(refstart);

    if( refer.refmod.len )
      {
      // The length was specified, so that's what we return:
      get_integer_value(reflen,
                        refer.refmod.len->field,
                        refer_offset(*refer.refmod.len));
      }
    else
      {
      // The length was not specified, so we need to return the distance
      // between refmod.from and the end of the field:
      gg_assign(reflen, gg_subtract( get_any_capacity(refer.field), refstart) );
      }
    return;
    }

  // ec_bound_ref_mode_e checking is enabled:

  get_integer_value(refstart,
                    refer.refmod.from->field,
                    refer_offset(*refer.refmod.from),
                    CHECK_FOR_FRACTIONAL_DIGITS);

  IF( var_decl_rdigits,
      ne_op,
      integer_zero_node )
    {
    // The value for refstart had non-zero decimal places.  This is an
    // error condition:
    set_exception_code(ec_bound_ref_mod_e);
    gg_assign(refstart, gg_cast(LONG, integer_one_node));
    gg_assign(var_decl_rdigits, integer_zero_node);
    }
  ELSE
    ENDIF

  // Make refstart zero-based:
  gg_decrement(refstart);

  IF( refstart, lt_op, build_int_cst_type(LONG, 0 ) )
    {
    // A negative value for refstart is an error condition:
    set_exception_code(ec_bound_ref_mod_e);
    gg_assign(refstart, gg_cast(LONG, integer_zero_node));
    // Set reflen to one here, because otherwise it won't be established.
    gg_assign(reflen, gg_cast(TREE_TYPE(reflen), integer_one_node));
    }
  ELSE
    {
    IF( refstart, gt_op, gg_cast(TREE_TYPE(refstart), get_any_capacity(refer.field)) )
      {
      // refstart greater than zero is an error condition:
      set_exception_code(ec_bound_ref_mod_e);
      gg_assign(refstart, gg_cast(LONG, integer_zero_node));
      // Set reflen to one here, because otherwise it won't be established.
      gg_assign(reflen, gg_cast(TREE_TYPE(reflen), integer_one_node));
      }
    ELSE
      {
      if( refer.refmod.len )
        {
        get_integer_value(reflen,
                          refer.refmod.len->field,
                          refer_offset(*refer.refmod.len),
                          CHECK_FOR_FRACTIONAL_DIGITS);
        IF( var_decl_rdigits,
            ne_op,
            integer_zero_node )
          {
          // length is not an integer, which is an error condition
          set_exception_code(ec_bound_ref_mod_e);
          gg_assign(reflen, gg_cast(LONG, integer_one_node));
          gg_assign(var_decl_rdigits, integer_zero_node);
          }
        ELSE
          {
          // The length is an integer, so we can keep going.
          IF( reflen, lt_op, gg_cast(LONG, integer_one_node) )
            {
            // length is too small, which is an error condition.
            set_exception_code(ec_bound_ref_mod_e);
            gg_assign(reflen, gg_cast(LONG, integer_one_node));
            }
          ELSE
            {
            IF( gg_add(refstart, reflen),
                gt_op,
                gg_cast(TREE_TYPE(refstart), get_any_capacity(refer.field)) )
              {
              // Start + Length is too large, which yet again is an error
              // condition
              set_exception_code(ec_bound_ref_mod_e);

              // Our intentions are honorable.  But at this point, where
              // we notice that start + length is too long, the
              // get_data_offset routine has already been run and
              // it's too late to actually change the refstart.  There are
              // theoretical solutions to this -- mainly,
              // get_data_offset needs to check the start + len for
              // validity.  But I am not going to do it now.  Think of this
              // as the TODO item.
              gg_assign(refstart, gg_cast(LONG, integer_zero_node));
              gg_assign(reflen, gg_cast(LONG, integer_one_node));
              }
            ELSE
              {
              // There are no problems, so there is no error condition, and
              // refstart and reflen are correct.
              }
              ENDIF
            }
            ENDIF
          }
          ENDIF
        }
      else
        {
        // There is no refmod length, so we default to the remaining characters
        gg_assign(reflen, gg_subtract(get_any_capacity(refer.field),
                                      refstart));
        }
      }
      ENDIF
    }
    ENDIF
  }

void
get_depending_on_value_from_odo(tree retval, cbl_field_t *odo)
  {
  /*  This routine, called only when we know there is an OCCURS DEPENDING ON
      clause, returns the current value of the DEPENDING ON variable.  When
      ec_bound_odo_e is turned on, and there is any kind of ec-bound-odo
      error condition, the value returned is occurs.bounds.lower.

      This should ensure that there is no memory violation in the event of a
      declarative with a RESUME NEXT STATEMENT, or before the default_condition
      processing can do a controlled exit.
      */
  const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );
  cbl_field_t *depending_on;
  depending_on = cbl_field_of(symbol_at(odo->occurs.depending_on));

  if( !enabled_exceptions.match(ec_bound_odo_e) )
    {
    // With no exception testing, just pick up the value.  If there is a
    // the programmer will simply have to live with the consequences.
    get_integer_value(retval,
                      depending_on,
                      NULL);
    return;
    }

  // Bounds checking is enabled, so we test the DEPENDING ON value to be
  // between the lower and upper OCCURS limits:
  get_integer_value(retval,
                    depending_on,
                    NULL,
                    CHECK_FOR_FRACTIONAL_DIGITS);

  IF( var_decl_rdigits, ne_op, integer_zero_node )
    {
    // This needs to evaluate to an integer
    set_exception_code(ec_bound_odo_e);
    gg_assign(retval, build_int_cst_type( TREE_TYPE(retval),
                                          odo->occurs.bounds.lower));
    gg_assign(var_decl_rdigits, integer_zero_node);
    }
  ELSE
    ENDIF

  IF( retval, gt_op, build_int_cst_type(TREE_TYPE(retval),
                                        odo->occurs.bounds.upper) )
    {
    set_exception_code(ec_bound_odo_e);
    gg_assign(retval, build_int_cst_type( TREE_TYPE(retval),
                                          odo->occurs.bounds.lower));
    }
  ELSE
    {
    IF( retval, lt_op, build_int_cst_type(TREE_TYPE(retval),
                                          odo->occurs.bounds.lower) )
      {
      set_exception_code(ec_bound_odo_e);
      gg_assign(retval, build_int_cst_type( TREE_TYPE(retval),
                                            odo->occurs.bounds.lower));
      }
    ELSE
      ENDIF
    IF( retval, lt_op, gg_cast(TREE_TYPE(retval), integer_zero_node) )
      {
      set_exception_code(ec_bound_odo_e);
      gg_assign(retval, gg_cast(TREE_TYPE(retval), integer_zero_node));
      }
    ELSE
      ENDIF
    }
    ENDIF
  }

static
void
get_depending_on_value(tree retval, const cbl_refer_t &refer)
  {
  /*  This routine, called only when we know there is an OCCURS DEPENDING ON
      clause, returns the current value of the DEPENDING ON variable.  When
      ec_bound_odo_e is turned on, and there is any kind of ec-bound-odo
      error condition, the value returned is occurs.bounds.lower.

      This should ensure that there is no memory violation in the event of a
      declarative with a RESUME NEXT STATEMENT, or before the default_condition
      processing can do a controlled exit.
      */
  cbl_field_t *odo = symbol_find_odo(refer.field);
  get_depending_on_value_from_odo(retval, odo);
  }

static
tree
get_data_offset(const cbl_refer_t &refer,
                      int *pflags = NULL)
  {
  Analyze();
  // This routine returns a tree which is the size_t offset to the data in the
  // refer/field

  // Because this is for source / sending variables, checks are made for
  // OCCURS DEPENDING ON violations (when those exceptions are enabled)

  tree retval = gg_define_variable(SIZE_T);
  gg_assign(retval, size_t_zero_node);

  // We have a refer.
  // At the very least, we have an constant offset
  int all_flags = 0;

  if( refer.nsubscript() )
    {
    REFER("subscript");
    // We have at least one subscript:

    // Figure we have three subscripts, so nsubscript is 3
    // Figure that the subscripts are {5, 4, 3}

    // We expect that starting from refer.field, that three of our ancestors --
    // call them A1, A2, and A3 -- have occurs clauses.

    // We need to start with the rightmost subscript, and work our way up through
    // our parents.  As we find each parent with an OCCURS, we increment qual_data
    // by (subscript-1)*An->data.capacity

    // Establish the field_t pointer for walking up through our ancestors:
    cbl_field_t *parent = refer.field;

    int all_flag_bit = 1;
    // Note the backwards test, because refer->nsubscript is an unsigned value
    for(size_t i=refer.nsubscript()-1; i<refer.nsubscript(); i-- )
      {
      // We need to search upward for an ancestor with occurs_max:
      while(parent)
        {
        if( parent->occurs.ntimes() )
          {
          break;
          }
        parent = parent_of(parent);
        }
      // we might have an error condition at this point:
      if( !parent )
        {
        cbl_internal_error("Too many subscripts");
        }
      // Pick up the integer value of the subscript:
      tree subscript  = gg_define_variable(LONG);

      if( (refer.subscripts[i].field->attr & FIGCONST_MASK) == zero_value_e )
        {
        // This refer is a figconst ZERO; we treat it as an ALL ZERO
        // This is our internal representation for ALL, as in TABLE(ALL)

        // Set the subscript to 1
        gg_assign(subscript,
                  build_int_cst_type( TREE_TYPE(subscript), 1));
        // Flag this position as ALL
        all_flags |= all_flag_bit;
        }
      else
        {
        const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );
        if( !enabled_exceptions.match(ec_bound_subscript_e) )
          {
          // With no exception testing, just pick up the value
          get_integer_value(subscript,
                            refer.subscripts[i].field,
                            refer_offset(refer.subscripts[i]));
          }
        else
          {
          get_integer_value(subscript,
                            refer.subscripts[i].field,
                            refer_offset(refer.subscripts[i]),
                            CHECK_FOR_FRACTIONAL_DIGITS);
          IF( var_decl_rdigits,
              ne_op,
              integer_zero_node )
            {
            // The subscript isn't an integer
            set_exception_code(ec_bound_subscript_e);
            gg_assign(subscript, build_int_cst_type(TREE_TYPE(subscript), 1));
            gg_assign(var_decl_rdigits, integer_zero_node);
            }
          ELSE
            {
            IF( subscript, lt_op, gg_cast(TREE_TYPE(subscript),
                                          integer_one_node) )
              {
              // The subscript is too small
              set_exception_code(ec_bound_subscript_e);
              gg_assign(subscript, build_int_cst_type(TREE_TYPE(subscript),
                                                      1));
              }
            ELSE
              {
              IF( subscript,
                  ge_op,
                  build_int_cst_type( TREE_TYPE(subscript),
                                      parent->occurs.ntimes()) )
                {
                // The subscript is too large
                set_exception_code(ec_bound_subscript_e);
                gg_assign(subscript, build_int_cst_type(TREE_TYPE(subscript),
                                                        1));
                }
              ELSE
                {
                }
                ENDIF
              }
              ENDIF
            }
          ENDIF
          }
        }

      all_flag_bit <<= 1;

      // Although we strictly don't need to look at the ODO value at this
      // point, we do want it checked for the purposes of ec-bound-odo

      const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );

      if( enabled_exceptions.match(ec_bound_odo_e) )
        {
        if( parent->occurs.depending_on )
          {
          static tree value64 = gg_define_variable( LONG,
                                                    ".._gdos_value64",
                                                    vs_file_static);
          cbl_field_t *odo = symbol_find_odo(parent);
          get_depending_on_value_from_odo(value64, odo);
          }
        }

      // Subscript is now a one-based integer
      // Make it zero-based:

      gg_decrement(subscript);

      tree augment = gg_multiply(subscript, get_any_capacity(parent));
      gg_assign(retval, gg_add(retval, gg_cast(SIZE_T, augment)));

      parent = parent_of(parent);
      }
    }

  if( refer.refmod.from )
    {
    REFER("refmod refstart");
    // We have a refmod to deal with
    static tree refstart = gg_define_variable(LONG, "..gdo_refstart", vs_file_static);
    static tree reflen   = gg_define_variable(LONG, "..gdo_reflen", vs_file_static);
    get_and_check_refstart_and_reflen(refstart, reflen, refer);

    gg_assign(retval, gg_add(retval, gg_cast(SIZE_T, refstart)));
    }

  if( pflags )
    {
    *pflags = all_flags;
    }

  return retval;
  }

static tree tree_type_from_field(const cbl_field_t *field);

void
get_binary_value( tree value,
                  tree rdigits,
                  cbl_field_t *field,
                  tree         field_offset,
                  tree         hilo
                  )
  {
  Analyze();
  if( hilo )
    {
    gg_assign(hilo, integer_zero_node);
    }

  bool needs_scaling = true;
  static const bool debugging=false;

  // Very special case:
  if( strcmp(field->name, "ZEROS") == 0 )
    {
    gg_assign(value, gg_cast(TREE_TYPE(value), integer_zero_node));
    if( rdigits )
      {
      gg_assign(rdigits, gg_cast(TREE_TYPE(rdigits), integer_zero_node));
      }
    return;
    }

  static tree pointer = gg_define_variable(UCHAR_P, "..gbv_pointer", vs_file_static);
  static tree pend = gg_define_variable(UCHAR_P, "..gbv_pend", vs_file_static);

  switch(field->type)
    {
    case FldLiteralN:
      {
      if( SCALAR_FLOAT_TYPE_P(value) )
        {
        cbl_internal_error("cannot get %<float%> value from %s", field->name);
        }
      else
        {
        if( rdigits )
          {
          gg_assign(rdigits, build_int_cst_type(TREE_TYPE(rdigits),
                                                field->data.rdigits));
          }
        tree dest_type   = TREE_TYPE(value);
        tree source_type = tree_type_from_field(field);

        gg_assign(value,
                  gg_cast(dest_type,
                          gg_indirect( gg_cast(build_pointer_type(source_type),
                              gg_get_address_of(field->data_decl_node)))));
        }

      break;
      }

    case FldNumericDisplay:
      {
      Analyzer.Message("FldNumericDisplay");
      // Establish the source
      tree source_address = get_data_address(field, field_offset);

      // We need to check early on for HIGH-VALUE and LOW-VALUE
      // Pick up the byte
      tree digit = gg_get_indirect_reference(source_address, NULL_TREE);
      IF( digit, eq_op, build_int_cst(UCHAR, 0xFF) )
        {
        if( hilo )
          {
          gg_assign(hilo, integer_one_node);
          }
        if( rdigits )
          {
          gg_assign(rdigits,
                    build_int_cst_type( TREE_TYPE(rdigits),
                                        get_scaled_rdigits(field)));
          }
        gg_assign(value, build_int_cst_type(TREE_TYPE(value), 0xFFFFFFFFFFFFFFFUL));
        }
      ELSE
        {
        IF( digit, eq_op, build_int_cst(UCHAR, 0x00) )
          {
          if( hilo )
            {
            gg_assign(hilo, integer_minus_one_node);
            }
          }
        ELSE
          {
          // Establish rdigits:
          if( rdigits )
            {
            gg_assign(rdigits,
                    build_int_cst_type( TREE_TYPE(rdigits),
                                        get_scaled_rdigits(field)));
            }
          // Zero out the destination
          gg_assign(value, gg_cast(TREE_TYPE(value), integer_zero_node));
          // Pick up a pointer to the source bytes:

          gg_assign(pointer, source_address);

          // This is the we-are-done pointer
          gg_assign(pend, gg_add( pointer,
                                  get_any_capacity(field)));

          static tree signbyte = gg_define_variable(UCHAR, "..gbv_signbyte", vs_file_static);

          // The big decision is whether or not the variable is signed:
          if( field->attr & signable_e )
            {
            // The variable is signed
            if( field->attr & separate_e )
              {
              // The sign byte is separate
              if( field->attr & leading_e)
                {
                // The first byte is '+' or '-'
                gg_increment(pointer);
                }
              else
                {
                // The final byte is '+' or '-'
                gg_decrement(pend);
                }
              }
            else
              {
              // The sign byte is internal
              if( field->attr & leading_e)
                {
                // The first byte has the sign bit:
                gg_assign(signbyte,
                          gg_get_indirect_reference(source_address, NULL_TREE));
                if( internal_codeset_is_ebcdic() )
                  {
                  // We need to make sure the EBCDIC sign bit is ON, for positive
                  gg_assign(gg_get_indirect_reference(source_address, NULL_TREE),
                            gg_bitwise_or(signbyte,
                                          build_int_cst_type( UCHAR,
                                                              NUMERIC_DISPLAY_SIGN_BIT)));
                  }
                else
                  {
                  // We need to make sure the ascii sign bit is Off, for positive
                  gg_assign(gg_get_indirect_reference(source_address, NULL_TREE),
                            gg_bitwise_and( signbyte,
                                            build_int_cst_type( UCHAR,
                                                                ~NUMERIC_DISPLAY_SIGN_BIT)));
                  }
                }
              else
                {
                // The final byte has the sign bit:
                gg_assign(signbyte,
                          gg_get_indirect_reference(source_address,
                                                    build_int_cst_type(SIZE_T,
                                                    field->data.capacity-1)));
                if( internal_codeset_is_ebcdic() )
                  {
                  // We need to make sure the EBCDIC sign bit is ON, for positive
                  gg_assign(gg_get_indirect_reference(source_address,
                                                      build_int_cst_type( SIZE_T,
                                                                          field->data.capacity-1)),
                            gg_bitwise_or(signbyte,
                                          build_int_cst_type( UCHAR,
                                                              NUMERIC_DISPLAY_SIGN_BIT)));
                  }
                else
                  {
                  // We need to make sure the ASCII sign bit is Off, for positive
                  gg_assign(gg_get_indirect_reference(source_address,
                                                      build_int_cst_type( SIZE_T,
                                                                          field->data.capacity-1)),
                            gg_bitwise_and( signbyte,
                                            build_int_cst_type( UCHAR,
                                                                ~NUMERIC_DISPLAY_SIGN_BIT)));
                  }
                }
              }
            }
          // We can now set up the byte-by-byte processing loop:
          if( internal_codeset_is_ebcdic() )
            {
            // We are working in EBCDIC
            WHILE( pointer, lt_op, pend )
              {
              // Pick up the byte
              digit = gg_get_indirect_reference(pointer, NULL_TREE);
              IF( digit, lt_op, build_int_cst_type(UCHAR, EBCDIC_ZERO) )
                {
                // break on a non-digit
                gg_assign(pointer, pend);
                }
              ELSE
                {
                IF( digit, gt_op, build_int_cst_type(UCHAR, EBCDIC_NINE) )
                  {
                  // break on a non-digit
                  gg_assign(pointer, pend);
                  }
                ELSE
                  {
                  // Whether ASCII or EBCDIC, the bottom four bits tell the tale:
                  // Multiply our accumulator by ten:
                  gg_assign(value, gg_multiply(value, build_int_cst_type(TREE_TYPE(value), 10)));
                  // And add in the current digit
                  gg_assign(value,
                            gg_add(value, gg_cast(TREE_TYPE(value), gg_bitwise_and( digit,
                                                                                    build_int_cst_type(UCHAR, 0x0F) ))));
                  gg_increment(pointer);
                  }
                  ENDIF
                }
                ENDIF
              }
              WEND
            }
          else
            {
            // We are working in ASCII:
            WHILE( pointer, lt_op, pend )
              {
              // Pick up the byte
              digit = gg_get_indirect_reference(pointer, NULL_TREE);
              // Whether ASCII or EBCDIC, the bottom four bits tell the tale:
              // Multiply our accumulator by ten:
              gg_assign(value, gg_multiply(value, build_int_cst_type(TREE_TYPE(value), 10)));
              // And add in the current digit
              gg_assign(value, gg_add(value, gg_cast(TREE_TYPE(value), gg_bitwise_and(digit, build_int_cst_type(UCHAR, 0x0F)))));
              gg_increment(pointer);
              }
              WEND
            }

          // Value contains the binary value.  The last thing is to apply -- and
          // undo -- the signable logic:

          if( field->attr & signable_e )
            {
            // The variable is signed
            if( field->attr & separate_e )
              {
              // The sign byte is separate
              if( field->attr & leading_e)
                {
                // The first byte is '+' or '-'
                if( internal_codeset_is_ebcdic() )
                  {
                  // We are operating in EBCDIC, so we look for a 96 (is minus sign)
                  IF( gg_get_indirect_reference(source_address, NULL_TREE),
                                                eq_op,
                                                build_int_cst_type(UCHAR, 96) )
                    {
                    gg_assign(value, gg_negate(value));
                    }
                  ELSE
                    ENDIF
                  }
                else
                  {
                  // We are operating in ASCII
                  IF( gg_get_indirect_reference(source_address, NULL_TREE),
                                                eq_op,
                                                build_int_cst_type(UCHAR, '-') )
                    {
                    gg_assign(value, gg_negate(value));
                    }
                  ELSE
                    ENDIF
                  }
                }
              else
                {
                // The final byte is '+' or '-'
                if( internal_codeset_is_ebcdic() )
                  {
                  // We are operating in EBCDIC, so we look for a 96 (is minus sign)
                  IF( gg_get_indirect_reference(source_address, build_int_cst_type(SIZE_T, field->data.capacity-1)),
                                                eq_op,
                                                build_int_cst_type(UCHAR, 96) )
                    {
                    gg_assign(value, gg_negate(value));
                    }
                  ELSE
                    ENDIF
                  }
                else
                  {
                  // We are operating in ASCII
                  IF( gg_get_indirect_reference(source_address, build_int_cst_type(SIZE_T, field->data.capacity-1)),
                                                eq_op,
                                                build_int_cst_type(UCHAR, '-') )
                    {
                    gg_assign(value, gg_negate(value));
                    }
                  ELSE
                    ENDIF
                  }
                }
              }
            else
              {
              // The sign byte is internal.  Check the sign bit
              if(internal_codeset_is_ebcdic())
                {
                IF( gg_bitwise_and( signbyte,
                                          build_int_cst_type( UCHAR,
                                                              NUMERIC_DISPLAY_SIGN_BIT)), eq_op, build_int_cst_type(UCHAR, 0) )
                  {
                  // The EBCDIC sign bit was OFF, so negate the result
                  gg_assign(value, gg_negate(value));
                  }
                ELSE
                  ENDIF
                }
              else
                {
                IF( gg_bitwise_and( signbyte,
                                          build_int_cst_type( UCHAR,
                                                              NUMERIC_DISPLAY_SIGN_BIT)), ne_op, build_int_cst_type(UCHAR, 0) )
                  {
                  // The ASCII sign bit was on, so negate the result
                  gg_assign(value, gg_negate(value));
                  }
                ELSE
                  ENDIF
                }
              // It's time to put back the original data:
              if( field->attr & leading_e)
                {
                // The first byte has the sign bit:
                gg_assign(gg_get_indirect_reference(source_address, NULL_TREE),
                          signbyte);
                }
              else
                {
                // The final byte has the sign bit:
                gg_assign(gg_get_indirect_reference(source_address,
                                                    build_int_cst_type(SIZE_T, field->data.capacity-1)),
                          signbyte);
                }
              }
            }
          }
        ENDIF
        }
      ENDIF

      break;
      }

    case FldNumericBinary:
      {
      // As of this writing, the source value is big-endian
      // We have to convert it to a little-endian destination.
      tree dest   = gg_cast(build_pointer_type(UCHAR), gg_get_address_of(value));
      tree source = get_data_address(field, field_offset);

      size_t dest_nbytes   = gg_sizeof(value);
      size_t source_nbytes = field->data.capacity;

      if( debugging )
        {
        gg_printf("dest_bytes/source_bytes %ld/%ld\n",
                  build_int_cst_type(SIZE_T, dest_nbytes),
                  build_int_cst_type(SIZE_T, source_nbytes),
                  NULL_TREE);
        gg_printf("Starting value: ", NULL_TREE);
        hex_dump(source, source_nbytes);
        gg_printf("\n", NULL_TREE);
        }

      if( dest_nbytes <= source_nbytes )
        {
        // Destination is too small.  We will move what we can, throwing away
        // the most significant source bytes:
        for(size_t i=0; i<dest_nbytes; i++)
          {
          gg_assign(gg_array_value(dest, i),
                    gg_array_value(source, source_nbytes-1-i) );
          }
        }
      else
        {
        // Destination is too big.  We'll need to fill the high-order bytes with
        // either 0x00 for positive numbers, or 0xFF for negative
        static tree extension = gg_define_variable( UCHAR,
                                                    "..gbv_extension",
                                                    vs_file_static);
        if( field->attr & signable_e )
          {
          IF( gg_array_value(gg_cast(build_pointer_type(SCHAR), source)), lt_op, gg_cast(SCHAR, integer_zero_node) )
            {
            gg_assign(extension, build_int_cst_type(UCHAR, 0xFF));
            }
          ELSE
            {
            gg_assign(extension, build_int_cst_type(UCHAR, 0));
            }
            ENDIF
          }
        else
          {
          gg_assign(extension, build_int_cst_type(UCHAR, 0));
          }

        // Flip the source end-for-end and put it into the dest:
        size_t i=0;
        while(i < source_nbytes)
          {
          gg_assign(gg_array_value(dest, i),
                    gg_array_value(source, source_nbytes-1-i) );
          i += 1;
          }
          // Fill the extra high-end bytes with 0x00 or 0xFF extension

        while(i < dest_nbytes)
          {
          gg_assign(gg_array_value(dest, i),
                    extension);
          i += 1;
          }
        }
      if( debugging )
        {
        gg_printf("Ending value:  ", NULL_TREE);
        hex_dump(dest, dest_nbytes);
        gg_printf("\n", NULL_TREE);
        }
      break;
      }

    case FldNumericBin5:
    case FldIndex:
    case FldPointer:
      {
      if( field->attr & intermediate_e )
        {
        // It is a intermediate, so rdigits has to come from the run-time structure
        if( rdigits )
          {
          gg_assign(rdigits,
                    gg_cast( TREE_TYPE(rdigits),
                             member(field, "rdigits")));
          }
        }
      else
        {
        // It isn't an intermediate, so we can safely use field->rdigits
        if( rdigits )
          {
          gg_assign(rdigits,
                    build_int_cst_type( TREE_TYPE(rdigits),
                                        get_scaled_rdigits(field)));
          }
        }
      tree source_address = get_data_address(field, field_offset);
      tree dest_type = TREE_TYPE(value);
      tree source_type = tree_type_from_size( field->data.capacity,
                                              field->attr & signable_e);
      if( debugging && rdigits)
        {
        gg_printf("get_binary_value bin5 rdigits: %d\n", rdigits, NULL_TREE);
        }

      gg_assign(value,
                gg_cast(dest_type,
                        gg_indirect(gg_cast( build_pointer_type(source_type),
                                             source_address ))));
      break;
      }

    case FldPacked:
      {
      // Zero out the destination:
      gg_assign(value, gg_cast(TREE_TYPE(value), integer_zero_node));
      gg_assign(pointer, get_data_address(field, field_offset));
      gg_assign(pend,
                gg_add(pointer,
                       build_int_cst_type(SIZE_T, field->data.capacity-1)));

      // Convert all but the last byte of the packed decimal sequence
      WHILE( pointer, lt_op, pend )
        {
        // Convert the first nybble
        gg_assign(value, gg_multiply(value, build_int_cst_type(TREE_TYPE(value), 10)));
        gg_assign(value, gg_add(value, gg_cast(TREE_TYPE(value), gg_rshift(gg_get_indirect_reference(pointer, NULL_TREE), build_int_cst(UINT, 4)))));

        // Convert the second nybble
        gg_assign(value, gg_multiply(value, build_int_cst_type(TREE_TYPE(value), 10)));
        gg_assign(value, gg_add(value, gg_cast(TREE_TYPE(value), gg_bitwise_and(gg_get_indirect_reference(pointer, NULL_TREE), build_int_cst_type(UCHAR, 0xF)))));
        gg_increment(pointer);
        }
        WEND

      // This is the final byte:
      gg_assign(value, gg_multiply(value, build_int_cst_type(TREE_TYPE(value), 10)));
      gg_assign(value, gg_add(value, gg_cast(TREE_TYPE(value), gg_rshift(gg_get_indirect_reference(pointer, NULL_TREE), build_int_cst(UINT, 4)))));

      IF( gg_bitwise_and(gg_get_indirect_reference(pointer, NULL_TREE), build_int_cst_type(UCHAR, 0xF)), eq_op, build_int_cst_type(UCHAR, 0x0D) )
        {
        gg_assign(value, gg_negate(value));
        }
      ELSE
        {
        IF( gg_bitwise_and(gg_get_indirect_reference(pointer, NULL_TREE), build_int_cst_type(UCHAR, 0xF)), eq_op, build_int_cst_type(UCHAR, 0x0B) )
          {
          gg_assign(value, gg_negate(value));
          }
        ELSE
          ENDIF
        }
        ENDIF
      break;
      }

    case FldFloat:
      {
      // We are going to assume that the float value contains an integer.
      if( rdigits )
        {
        gg_assign(rdigits,
                  gg_cast( TREE_TYPE(rdigits), integer_zero_node));
        }
      gg_assign(value,
                gg_cast(TREE_TYPE(value),
                        gg_call_expr( INT128,
                                      "__gg__integer_from_float128",
                                      gg_get_address_of(field->var_decl_node),
                                      NULL_TREE)));
      needs_scaling = false;
      break;
      }

    default:
      {
      char *err = xasprintf("%s(): We know not how to"
                            " get a binary value from %s\n",
                            __func__,
                            cbl_field_type_str(field->type) );
      cbl_internal_error("%s", err);
      abort();
      // break; // break not needed after abort();
      }
    }

  if( needs_scaling )
    {
    if( field->attr & scaled_e )
      {
      if( field->data.rdigits < 0 )
        {
        scale_by_power_of_ten_N(value, -field->data.rdigits);
        }
      }
    }
  }

static tree
tree_type_from_field(const cbl_field_t *field)
  {
  gcc_assert(field);
  return tree_type_from_size(field->data.capacity, field->attr & signable_e);
  }

tree
get_data_address( cbl_field_t *field,
                  tree         offset)  // Offset is SIZE_T
  {
  if( offset )
    {
    return gg_cast( UCHAR_P,
                    gg_add( gg_cast(SIZE_T,
                                    member( field->var_decl_node,
                                            "data")),
                            offset));
    }
  else
    {
    return member(field->var_decl_node, "data");
    }
  }

FIXED_WIDE_INT(128)
get_power_of_ten(int n)
  {
  // 2** 64 = 1.8E19
  // 2**128 = 3.4E38
  FIXED_WIDE_INT(128) retval = 1;
  static const int MAX_POWER = 19 ;
  static const unsigned long long pos[MAX_POWER+1] =
    {
    1ULL,                       // 00
    10ULL,                      // 01
    100ULL,                     // 02
    1000ULL,                    // 03
    10000ULL,                   // 04
    100000ULL,                  // 05
    1000000ULL,                 // 06
    10000000ULL,                // 07
    100000000ULL,               // 08
    1000000000ULL,              // 09
    10000000000ULL,             // 10
    100000000000ULL,            // 11
    1000000000000ULL,           // 12
    10000000000000ULL,          // 13
    100000000000000ULL,         // 14
    1000000000000000ULL,        // 15
    10000000000000000ULL,       // 16
    100000000000000000ULL,      // 17
    1000000000000000000ULL,     // 18
    10000000000000000000ULL,    // 19
    };
  if( n < 0 || n>MAX_POWER*2)     // The most we can handle is 10**38
    {
    fprintf(stderr, "Trying to raise 10 to %d as an int128, which we can't do.\n", n);
    fprintf(stderr, "The problem is in %s.\n", __func__);
    abort();
    }
  if( n <= MAX_POWER )
    {
    // Up to 10**18 we do directly:
    retval = pos[n];
    }
  else
    {
    // 19 through 38 is handled in a second step, because when this was written,
    // GCC couldn't handle 128-bit constants:
    retval = pos[n/2];
    retval *= retval;
    if( n & 1 )
      {
      retval *= 10;
      }
    }
  return retval;
  }

void
scale_by_power_of_ten_N(tree value,
                      int N,
                      bool check_for_fractional)
  {
  // This routine is called when we know N at compile time.

  Analyze();
  Analyzer.Message("takes int N");
  if( N == 0 )
    {
    }
  else if( N > 0 )
    {
    tree value_type = TREE_TYPE(value);
    FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(N);
    gg_assign(value, gg_multiply(value, wide_int_to_tree( value_type,
                                  power_of_ten)));
    }
  if( N < 0 )
    {
    tree value_type = TREE_TYPE(value);
    FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(-N);
    if( check_for_fractional )
      {
      IF( gg_mod(value, wide_int_to_tree( value_type,
                                          power_of_ten)),
          ne_op,
          gg_cast(value_type, integer_zero_node) )
        {
        gg_assign(var_decl_rdigits, integer_one_node);
        }
      ELSE
        ENDIF
      }
    gg_assign(value, gg_divide(value, wide_int_to_tree( value_type,
                                  power_of_ten)));
    }
  }

tree
scale_by_power_of_ten(tree value,
                      tree N,
                      bool check_for_fractional)
  {
  Analyze();
  static tree retval = gg_define_variable(INT, "..sbpot2_retval", vs_file_static);

  if( check_for_fractional )
    {
    // Our caller expects us to return 1 if value was something like 99v99 and
    // the fractional part was non-zero
    gg_assign(value,
              gg_cast(TREE_TYPE(value),
                      gg_call_expr(INT128,
                                   "__gg__scale_by_power_of_ten_1",
                                   gg_cast(INT128, value),
                                   N,
                                   NULL_TREE)));
    }
  else
    {
    // Our caller does not expect us to test for fractional values
    gg_assign(value,
              gg_cast(TREE_TYPE(value),
                      gg_call_expr(INT128,
                                   "__gg__scale_by_power_of_ten_2",
                                   gg_cast(INT128, value),
                                   N,
                                   NULL_TREE)));

    }

  gg_assign(retval, integer_zero_node);
  return retval;
  }

void
scale_and_round(tree value,
                int  value_rdigits,
                bool target_is_signable,
                int  target_rdigits,
                cbl_round_t rounded)
  {
  if( !target_is_signable )
    {
    // The target has to be positive, so take the absolute value of the input
    gg_assign(value, gg_abs(value));
    }

  if( target_rdigits >= value_rdigits )
    {
    // The value doesn't have enough rdigits.  All we need to do is multiply it
    // by a power of ten to get it right:
    scale_by_power_of_ten_N(value,
                          target_rdigits - value_rdigits);
    }
  else
    {
    // The value has too few rdigits.
    switch(rounded)
      {
      case nearest_away_from_zero_e:
        {
        // This is rounding away from zero

        // We want to adjust value so that the extra digit is in the units
        // place:
        scale_by_power_of_ten_N(value,
                              target_rdigits - value_rdigits + 1);
        // Add five to the result:
        IF( value, lt_op, gg_cast(TREE_TYPE(value), integer_zero_node) )
          {
          gg_assign(value,
                    gg_add( value,
                            build_int_cst_type(TREE_TYPE(value), -5)));
          }
        ELSE
          {
          gg_assign(value,
                    gg_add( value,
                            build_int_cst_type(TREE_TYPE(value), +5)));
          }
        // And now get rid of the lowest decimal digit
        scale_by_power_of_ten_N(value, -1);

        break;
        }

      case truncation_e:
        {
        // Without rounding, just scale the result
        scale_by_power_of_ten_N(value, target_rdigits - value_rdigits);
        break;
        }
      default:
        abort();
        break;
      }
    }
  }

void
hex_dump(tree data, size_t bytes)
  {
  gg_printf("0x", NULL_TREE);
  for(size_t i=0; i<bytes; i++)
    {
    gg_printf("%2.2x",
              gg_cast(UINT,
                      gg_array_value( gg_cast(build_pointer_type(UCHAR), data),
                                      i)),
              NULL_TREE);
    }
  }

tree
tree_type_from_size(size_t bytes, int signable)
  {
  tree retval = NULL_TREE;

  if( signable )
    {
    switch( bytes )
      {
      case 1:
        retval = CHAR;
        break;
      case 2:
        retval = SHORT;
        break;
      case 4:
        retval = INT;
        break;
      case 8:
        retval = LONGLONG;
        break;
      case 16:
        retval = INT128;
        break;
      default:
        gcc_unreachable();
        break;
      }
    }
  else
    {
    switch( bytes )
      {
      case 1:
        retval = UCHAR;
        break;
      case 2:
        retval = USHORT;
        break;
      case 4:
        retval = UINT;
        break;
      case 8:
        retval = ULONGLONG;
        break;
      case 16:
        retval = UINT128;
        break;
      default:
        gcc_unreachable();
        break;
      }
    }
  return retval;
  }

static
bool
refer_has_depends(const cbl_refer_t &refer, refer_type_t refer_type)
  {
  if( suppress_dest_depends )
    {
    // This is set, for example, by parser_initialize, which needs to set a
    // variable's value regardless of the impact of a DEPENDING ON clause.
    return false;
    }

  if(    refer.field
      && (refer.field->attr & (intermediate_e)) )
    {
    // This field can't have a DEPENDING ON
    return false;
    }

  // Check if there there is an occurs with a depending_on in the hierarchy
  bool proceed = false;
  const cbl_field_t *odo = symbol_find_odo(refer.field);
  cbl_field_t *depending_on;
  if( odo && odo != refer.field )
    {
    // We have an ODO and refer.field is not the ODO, so we can keep looking
    depending_on = cbl_field_of(symbol_at(odo->occurs.depending_on));
    if( depending_on->var_decl_node )
      {
      // The depending_on has been initialized
      if( refer_type == refer_source )
        {
        proceed = true;
        }
      else
        {
        // In ISO/IEC 1989:2023, "OCCURS 13.18.38.4 General rules", talks about the
        // three situations we know how to deal with.

        // Rule 7)  We need to detect if depending_on is completely independent
        //          of refer.field
        cbl_field_t *p;
        cbl_field_t *parent1 = refer.field;
        while( (p = parent_of(parent1)) )
          {
          parent1 = p;
          }
        const cbl_field_t *parent2 = depending_on;
        while( (p = parent_of(parent2)) )
          {
          parent2 = p;
          }
        if( parent1 != parent2 )
          {
          // refer.field and depending_on have two different ultimate parents, so
          // Rule 7) applies, and we have to trim the destination according to
          // depending_on
          //gg_printf("Rule 7 applies\n", NULL_TREE);
          proceed = true;
          }
        else
          {
          // Rule 7) doesn't apply, so we have to check Rule 8)
          // In this case:
          //      01      digtab.
          //       05     depl        pic 9.
          //       05     digitgrp.
          //        10    digits      occurs 1 to 9 depending on depl pic x.
          //      MOVE ... TO digitgrp
          // The DEPENDING ON variable depl is not subordinate to digitgrp, and
          // consequently we have to trim according to depl:
          if( depending_on->offset < refer.field->offset )
            {
            // depending_on comes before refer.field, so rule 8a) applies
            //gg_printf("Rule 8a) applies\n", NULL_TREE);
            proceed = true;
            }
          else
            {
            // depending_on comes after refer.field, so Rule 8b) for receiving
            // items applies, and we will not trim according to depending_on
            //gg_printf("Rule 8b) applies\n", NULL_TREE);
            }
          }
        }
      }
    }
  return proceed;
  }

void
set_exception_code_func(ec_type_t ec, int /*line*/, int from_raise_statement)
  {
  if( ec )
    {
    gg_call(VOID,
            "__gg__set_exception_code",
            build_int_cst_type(INT, ec),
            build_int_cst_type(INT, from_raise_statement),
            NULL_TREE);
    }
  else
    {
    gg_printf("set_exception_code: set it to ZERO\n", NULL_TREE);
    gg_assign(var_decl_exception_code, integer_zero_node);
    }
  }

bool
process_this_exception(const ec_type_t ec)
  {
  const cbl_enabled_exceptions_t& enabled_exceptions( cdf_enabled_exceptions() );
  bool retval;
  if( enabled_exceptions.match(ec) || !skip_exception_processing )
    {
    retval = true;
    }
  else
    {
    retval = false;
    }
  return retval;
  }

void
rt_error(const char *msg)
  {
  // Come here with a fatal run-time error message
  char ach[256];
  snprintf( ach, sizeof(ach), "%s:%d: %s",
            current_filename.back().c_str(),
            CURRENT_LINE_NUMBER,
            msg);
  gg_printf("%s\n", gg_string_literal(ach), NULL_TREE);
  gg_abort();
  }

void
copy_little_endian_into_place(cbl_field_t *dest,
                              tree         dest_offset,
                              tree value,
                              int rhs_rdigits,
                              bool check_for_error,
                        const tree &size_error)
  {
  if( check_for_error )
    {
    // We need to see if value can fit into destref

    // We do this by comparing value to 10^(lhs.ldigits + rhs_rdigits)
    // Example:  rhs is 123.45, whichis 12345 with rdigits 2
    // lhs is 99.999.  So, lhs.digits is 5, and lhs.rdigits is 3.
    // 10^(5 - 3 + 2) is 10^4, which is 10000.  Because 12345 is >= 10000, the
    // source can't fit into the destination.

    // Note:  I am not trying to avoid the use of stack variables, because I am
    // not sure how to declare a file-static variable of unknown type.
    tree abs_value = gg_define_variable(TREE_TYPE(value));
    IF( value, lt_op, build_int_cst_type(TREE_TYPE(value), 0) )
      {
      gg_assign(abs_value, gg_negate(value));
      }
    ELSE
      {
      gg_assign(abs_value, value);
      }
    ENDIF

    FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(  dest->data.digits
                                                        - dest->data.rdigits
                                                        + rhs_rdigits );
    IF( gg_cast(INT128, abs_value),
        ge_op,
        wide_int_to_tree(INT128, power_of_ten) )
      {
      // Flag the size error
      gg_assign(size_error, integer_one_node);
      }
    ELSE
      ENDIF
    }
  scale_by_power_of_ten_N(value, dest->data.rdigits - rhs_rdigits);

  tree dest_type = tree_type_from_size( dest->data.capacity,
                                        dest->attr & signable_e);
  tree dest_pointer = gg_add(member(dest->var_decl_node, "data"),
                             dest_offset);
  gg_assign(gg_indirect(gg_cast(build_pointer_type(dest_type), dest_pointer)),
            gg_cast(dest_type, value));
  }

void
build_array_of_treeplets( int ngroup,
                          size_t N,
                          cbl_refer_t *refers)
  {
  if( N )
    {
    // At the present time the most this routine is called is four times, for
    // the implementation of the UNSTRING verb.

    if( N > MIN_FIELD_BLOCK_SIZE )
      {
      gg_call(VOID,
              "__gg__resize_treeplet",
              build_int_cst_type(INT,    ngroup),
              build_int_cst_type(SIZE_T, N),
              NULL_TREE
              );
      }
    switch(ngroup)
      {
      case 1:
        for(size_t i=0; i<N; i++)
          {
          gg_assign(gg_array_value(var_decl_treeplet_1f, i),
                    refers[i].field ? gg_get_address_of(refers[i].field->var_decl_node)
                                    : gg_cast(cblc_field_p_type_node, null_pointer_node));
          gg_assign(gg_array_value(var_decl_treeplet_1o, i),
                    refer_offset(refers[i]));
          gg_assign(gg_array_value(var_decl_treeplet_1s, i),
                    refer_size_source(refers[i]));
          }
        break;
      case 2:
        for(size_t i=0; i<N; i++)
          {
          gg_assign(gg_array_value(var_decl_treeplet_2f, i),
                    refers[i].field ? gg_get_address_of(refers[i].field->var_decl_node)
                                    : gg_cast(cblc_field_p_type_node, null_pointer_node));
          gg_assign(gg_array_value(var_decl_treeplet_2o, i),
                    refer_offset(refers[i]));
          gg_assign(gg_array_value(var_decl_treeplet_2s, i),
                    refer_size_source(refers[i]));
          }
        break;
      case 3:
        for(size_t i=0; i<N; i++)
          {
          gg_assign(gg_array_value(var_decl_treeplet_3f, i),
                    refers[i].field ? gg_get_address_of(refers[i].field->var_decl_node)
                                    : gg_cast(cblc_field_p_type_node, null_pointer_node));
          gg_assign(gg_array_value(var_decl_treeplet_3o, i),
                    refer_offset(refers[i]));
          gg_assign(gg_array_value(var_decl_treeplet_3s, i),
                    refer_size_source(refers[i]));
          }
        break;
      case 4:
        for(size_t i=0; i<N; i++)
          {
          gg_assign(gg_array_value(var_decl_treeplet_4f, i),
                    refers[i].field ? gg_get_address_of(refers[i].field->var_decl_node)
                                    : gg_cast(cblc_field_p_type_node, null_pointer_node));
          gg_assign(gg_array_value(var_decl_treeplet_4o, i),
                    refer_offset(refers[i]));
          gg_assign(gg_array_value(var_decl_treeplet_4s, i),
                    refer_size_source(refers[i]));
          }
        break;
      default:
        abort();
        break;
      }
    }
  else
    {
    // Just do nothing
    }
  }

void
build_array_of_fourplets( int ngroup,
                          size_t N,
                          cbl_refer_t *refers)
  {
  int flag_bits = 0;
  if( N )
    {
    if( N > MIN_FIELD_BLOCK_SIZE )
      {
      gg_call(VOID,
              "__gg__resize_treeplet",
              build_int_cst_type(INT, ngroup),
              build_int_cst_type(SIZE_T, N),
              NULL_TREE);

      gg_call(VOID,
              "__gg__resize_int_p",
              gg_get_address_of(var_decl_fourplet_flags_size),
              gg_get_address_of(var_decl_fourplet_flags),
              build_int_cst_type(SIZE_T, N),
              NULL_TREE);
      }

    for(size_t i=0; i<N; i++)
      {
      gg_assign(gg_array_value(var_decl_treeplet_1f, i),
                gg_get_address_of(refers[i].field->var_decl_node));
      gg_assign(gg_array_value(var_decl_treeplet_1o, i),
                refer_offset(refers[i], &flag_bits));
      gg_assign(gg_array_value(var_decl_treeplet_1s, i),
                refer_size_source(refers[i]));
      gg_assign(gg_array_value(var_decl_fourplet_flags, i),
                build_int_cst_type(INT, flag_bits));
      }
    }
  else
    {
    abort();
    }
  }

tree
build_array_of_size_t( size_t  N,
                       const size_t *values)
  {
  // We create and populate an array of size_t values

  // This only works because it is used in but one spot.  If this routine is
  // called twice, be careful about how the first one is used.  It's a static
  // variable, you see.
  static tree values_p = gg_define_variable(SIZE_T_P, "..baost_values_p", vs_file_static);
  if( N )
    {
    gg_assign(  values_p,
                gg_cast(build_pointer_type(SIZE_T),
                        gg_malloc(N*sizeof(SIZE_T))));

    for(size_t i=0; i<N; i++)
      {
      gg_assign(  gg_array_value(values_p, i),
                  build_int_cst_type(SIZE_T, values[i]));
      }
    }
  else
    {
    gg_assign(  values_p,
                gg_cast(build_pointer_type(SIZE_T), null_pointer_node ));
    }
  return values_p;
  }

void
parser_display_internal_field(tree file_descriptor,
                              cbl_field_t *field,
                              bool advance)
  {
  cbl_refer_t wrapper = {};
  wrapper.field = field;
  parser_display_internal(file_descriptor, wrapper, advance);
  }

char *
get_literal_string(cbl_field_t *field)
  {
  assert(field->type == FldLiteralA);
  size_t buffer_length = field->data.capacity+1;
  char *buffer = static_cast<char *>(xcalloc(1, buffer_length));

  for(size_t i=0; i<field->data.capacity; i++)
    {
    buffer[i] = ascii_to_internal(field->data.initial[i]);
    }

  return buffer;
  }

bool
refer_is_clean(const cbl_refer_t &refer)
  {
  if( !refer.field || refer.field->type == FldLiteralN )
    {
    // It is routine for a refer to have no field.  It happens when the parser
    // passes us a refer for an optional parameter that has been omitted, for
    // example.

    // It is also the case that a FldLiteralN will never have suscripts, or the
    // like.
    return true;
    }

  return     !refer.all
          && !refer.addr_of
          && !refer.nsubscript()
          && !refer.refmod.from
          && !refer.refmod.len
          && !refer_has_depends(refer, refer_source)
          ;
  }


/*  This routine returns the length portion of a refmod(start:length) reference.
    It extracts both the start and the length so that it can add them together
    to make sure that result falls within refer.capacity.

    This routine shouldn't be called unless there is refmod involved.
    */
static
tree  // size_t
refer_refmod_length(const cbl_refer_t &refer)
  {
  Analyze();
  REFER("refstart and reflen");
  static tree refstart = gg_define_variable(LONG, "..rrl_refstart", vs_file_static);
  static tree reflen   = gg_define_variable(LONG, "..rrl_reflen", vs_file_static);

  get_and_check_refstart_and_reflen( refstart, reflen, refer);

  // Arrive here with a valid value for reflen:

  return gg_cast(SIZE_T, reflen);
  }

static
tree // size_t
refer_fill_depends(const cbl_refer_t &refer)
  {
  REFER("");
  // This returns a positive number which is the amount a depends-limited
  // capacity needs to be reduced.
  Analyze();
  cbl_field_t *odo = symbol_find_odo(refer.field);

  static tree value64 = gg_define_variable(LONG, "..rfd_value64", vs_file_static);

  get_depending_on_value(value64, refer);

  // value64 is >= zero and < bounds.upper

  // We multiply the ODO value by the size of the data capacity to get the
  // shortened length:

  tree mult_expr = gg_multiply( build_int_cst_type(TREE_TYPE(value64), odo->data.capacity),
                                value64 );

  // And we add that to the distance from the requested variable to the odo
  // variable to get the modified length:
  tree add_expr = gg_add(mult_expr, build_int_cst_type(SIZE_T, odo->offset - refer.field->offset));
  return add_expr;
  }

tree  // size_t
refer_offset(const cbl_refer_t &refer,
                   int *pflags)
  {
  // This routine calculates the effect of a refer offset on the
  // refer.field->data location.  When there are subscripts, the data location
  // gets augmented by the (subscript-1)*element_size calculation.  And when
  // there is a refmod, the data location additionally gets augmented by
  // (refmod.from-1)

  if( !refer.field )
    {
    // It's common for the field to be missing.  It generally means that an
    // optional parameter wasn't supplied.
    return size_t_zero_node;
    }

  if( refer.field->type == FldLiteralN || refer.field->type == FldLiteralA )
    {
    // We know that literals have no offset
    return size_t_zero_node;
    }

  tree retval = get_data_offset(refer, pflags);
  return retval;
  }

static
tree   // size_t
refer_size(const cbl_refer_t &refer, refer_type_t refer_type)
  {
  Analyze();
  static tree retval = gg_define_variable(SIZE_T, "..rs_retval", vs_file_static);

  if( !refer.field )
    {
    return size_t_zero_node;
    }

  if( refer_is_clean(refer) )
    {
    return get_any_capacity(refer.field);
    }

  // Step the first:  Get the actual full length:

  if( refer_has_depends(refer, refer_type) )
    {
    // Because there is a depends, we might have to change the length:
    gg_assign(retval, refer_fill_depends(refer));
    }
  else
    {
    gg_assign(retval, get_any_capacity(refer.field));
    }

  if( refer.refmod.from || refer.refmod.len )
    {
    tree refmod = refer_refmod_length(refer);
    // retval is the ODO based total length.
    // refmod is the length resulting from refmod(from:len)
    // We have to reduce retval by the effect of refmod:
    tree diff = gg_subtract(get_any_capacity(refer.field),
                            refmod);
    gg_assign(retval, gg_subtract(retval, diff));
    }
  return retval;
  }

tree  // size_t
refer_size_dest(const cbl_refer_t &refer)
  {
  return refer_size(refer, refer_dest);
  }

tree  // size_t
refer_size_source(const cbl_refer_t &refer)
  {
  /*  There are oddities involved with refer_size_source and refer_size_dest.
      See the comments in refer_has_depends for some explanation.  There are
      other considerations, as well.  For example, consider a move, where you
      have both a source and a dest.  Given that refer_size returns a static,
      there are ways that the source and dest can trip over each other.

      The logic here avoids all known cases where they might trip over each
      other.  But there conceivably might be others,.

      You have been warned.

      */

  // This test has to be here, otherwise there are failures in regression
  // testing.
  if( !refer.field )
    {
    return size_t_zero_node;
    }

  // This test has to be here, otherwise there are failures in regression
  // testing.
  if( refer_is_clean(refer) )
    {
    return get_any_capacity(refer.field);
    }

  // This assignment has to be here. Simply returning refer_size() results
  // in regression testing errors.
  static tree retval = gg_define_variable(SIZE_T, "..rss_retval", vs_file_static);
  gg_assign(retval, refer_size(refer, refer_source));
  return retval;
  }

tree
qualified_data_location(const cbl_refer_t &refer)
  {
  return gg_add(member(refer.field->var_decl_node, "data"),
                refer_offset(refer));
  }

uint64_t
get_time_nanoseconds()
{
  // This code was unabashedly stolen from gcc/timevar.cc.
  // It returns the Unix epoch with nine decimal places.

  uint64_t retval = 0;

#ifdef HAVE_CLOCK_GETTIME
  struct timespec ts;
  clock_gettime (CLOCK_REALTIME, &ts);
  retval = ts.tv_sec * 1000000000 + ts.tv_nsec;
  return retval;
#endif
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday (&tv, NULL);
  retval = tv.tv_sec * 1000000000 + tv.tv_usec * 1000;
  return retval;
#endif
  return retval;
}
