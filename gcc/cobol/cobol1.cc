/* gcobol backend interface
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Robert J. Dubner and James K. Lowden

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "cobol-system.h"
#include <coretypes.h>
#include <tree.h>
#include <diagnostic.h>
#include <opts.h>
#include <debug.h>
#include <langhooks.h>
#include <langhooks-def.h>
#include <target.h>
#include <stringpool.h>
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "../../libgcobol/exceptl.h"
#include "exceptg.h"
#include "gengen.h"   // This has some GTY(()) markers
#include "structs.h"  // This has some GTY(()) markers

/*  Required language-dependent contents of a type.

    Without it, we get

    gt-cobol-cobol1.h:858: undefined reference to `gt_pch_nx_lang_type(void *)

    */

struct GTY (()) lang_type
    {
    char dummy;
    };

/* Language-dependent contents of a decl.
    Without it, we get

    gt-cobol-cobol1.h:674: more undefined references to `gt_pch_nx_lang_decl

    */

struct GTY (()) lang_decl
    {
    char dummy;
    };

/*
 * Language-dependent contents of an identifier.
 * This must include a tree_identifier.
 */
struct GTY (()) lang_identifier
    {
    struct tree_identifier common;
    };

/* The resulting tree type.  */

union GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
                chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
                            "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN "
                            "(&%h.generic)) : NULL"))) lang_tree_node
    {
    union tree_node GTY ((tag ("0"), desc ("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY ((tag ("1"))) identifier;
    };

/*  We don't use language_function.

    But without the placeholder:

    /usr/bin/ld: gtype-desc.o: in function `gt_ggc_mx_function(void*)':
    ../build/gcc/gtype-desc.cc:1763: undefined reference to `gt_ggc_mx_language_function(void*)'
    /usr/bin/ld: gtype-desc.o: in function `gt_pch_nx_function(void*)':
    ../build/gcc/gtype-desc.cc:5727: undefined reference to `gt_pch_nx_language_function(void*)'

    */

struct GTY (()) language_function
    {
    int dummy;
    };

/*
 *  Language hooks.
 */

#define ATTR_NULL     0
#define ATTR_LEAF_LIST      (ECF_LEAF)
#define ATTR_NOTHROW_LEAF_LIST    (ECF_NOTHROW | ECF_LEAF)
#define ATTR_NOTHROW_LEAF_MALLOC_LIST (ECF_NOTHROW | ECF_LEAF | ECF_MALLOC)
#define ATTR_CONST_NOTHROW_LEAF_LIST  (ECF_NOTHROW | ECF_LEAF | ECF_CONST)
#define ATTR_PURE_NOTHROW_LEAF_LIST (ECF_NOTHROW | ECF_LEAF | ECF_PURE)
#define ATTR_NOTHROW_LIST   (ECF_NOTHROW)
#define ATTR_CONST_NOTHROW_LIST   (ECF_NOTHROW | ECF_CONST)
#define ATTR_ALLOC_WARN_UNUSED_RESULT_SIZE_2_NOTHROW_LIST \
          (ECF_NOTHROW | ECF_LEAF | ECF_MALLOC)
#define ATTR_ALLOC_WARN_UNUSED_RESULT_SIZE_2_NOTHROW_LEAF_LIST \
          (ECF_NOTHROW | ECF_LEAF)
#define ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST \
          (ECF_COLD | ECF_NORETURN | \
          ECF_NOTHROW | ECF_LEAF)
#define ATTR_PURE_NOTHROW_NONNULL_LEAF (ECF_PURE|ECF_NOTHROW|ECF_LEAF)
#define ATTR_MALLOC_WARN_UNUSED_RESULT_NOTHROW_NONNULL_LEAF (ECF_MALLOC|ECF_NOTHROW|ECF_LEAF)
#define ATTR_TMPURE_NORETURN_NOTHROW_LEAF_COLD_LIST (ECF_TM_PURE|ECF_NORETURN|ECF_NOTHROW|ECF_LEAF|ECF_COLD)
#define ATTR_NORETURN_NOTHROW_LIST (ECF_NORETURN|ECF_NOTHROW)
#define ATTR_NOTHROW_NONNULL_LEAF (ECF_NOTHROW|ECF_LEAF)

static void
gfc_define_builtin (const char *name, tree type, enum built_in_function code,
        const char *library_name, int attr)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
             library_name, NULL_TREE);
  set_call_expr_flags (decl, attr);

  set_builtin_decl (code, decl, true);
}

static void
create_our_type_nodes_init()
  {
  for(int i=0; i<256; i++)
    {
    char_nodes[i] = build_int_cst_type(CHAR, i);
    }

  // Create some useful constants to avoid cluttering up the code
  // build_int_cst_type() calls
  pvoid_type_node    = build_pointer_type(void_type_node);
  integer_minusone_node = build_int_cst_type(INT, -1);
  integer_two_node    = build_int_cst_type(INT, 2);
  integer_eight_node  = build_int_cst_type(INT, 8);
  size_t_zero_node    = build_int_cst_type(SIZE_T,  0);
  int128_zero_node    = build_int_cst_type(INT128,  0);
  int128_five_node    = build_int_cst_type(INT128,  5);
  int128_ten_node     = build_int_cst_type(INT128, 10);
  char_ptr_type_node  = build_pointer_type(CHAR);
  uchar_ptr_type_node = build_pointer_type(UCHAR);
  wchar_ptr_type_node = build_pointer_type(WCHAR);
  long_double_ten_node = build_real_from_int_cst(
                           LONGDOUBLE,
                           build_int_cst_type(INT,10));
  sizeof_size_t  = build_int_cst_type(SIZE_T, int_size_in_bytes(SIZE_T));
  sizeof_pointer = build_int_cst_type(SIZE_T, int_size_in_bytes(VOID_P));

  bool_true_node = build2(EQ_EXPR,
                          integer_type_node,
                          integer_one_node,
                          integer_one_node);

  bool_false_node = build2(   EQ_EXPR,
                              integer_type_node,
                              integer_one_node,
                              integer_zero_node);
  }


static bool
cobol_langhook_init (void)
    {
    build_common_tree_nodes (true);

    create_our_type_nodes_init();

    tree char_pointer_type_node = build_pointer_type (char_type_node);
    tree const_char_pointer_type_node
      = build_pointer_type (build_type_variant (char_pointer_type_node, 1, 0));

    tree ftype;

    ftype = build_function_type_list (pvoid_type_node,
                                      size_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_malloc",
                        ftype,
                        BUILT_IN_MALLOC,
                        "malloc",
                        ATTR_NOTHROW_LEAF_MALLOC_LIST);

    ftype = build_function_type_list (pvoid_type_node, pvoid_type_node,
              size_type_node, NULL_TREE);
    gfc_define_builtin ("__builtin_realloc", ftype, BUILT_IN_REALLOC,
            "realloc", ATTR_NOTHROW_LEAF_LIST);

    ftype = build_function_type_list (void_type_node,
                                      pvoid_type_node, NULL_TREE);
    gfc_define_builtin ("__builtin_free", ftype, BUILT_IN_FREE,
            "free", ATTR_NOTHROW_LEAF_LIST);

    ftype = build_function_type_list (pvoid_type_node,
                                      const_ptr_type_node,
                                      integer_type_node,
                                      size_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_memchr", ftype, BUILT_IN_MEMCHR,
            "memchr", ATTR_PURE_NOTHROW_NONNULL_LEAF);


    ftype = build_function_type_list (size_type_node,
                                      const_char_pointer_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_strlen", ftype, BUILT_IN_STRLEN,
            "strlen", ATTR_PURE_NOTHROW_NONNULL_LEAF);


    ftype = build_function_type_list (char_pointer_type_node,
                                      const_char_pointer_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_strdup", ftype, BUILT_IN_STRDUP,
            "strdup", ATTR_MALLOC_WARN_UNUSED_RESULT_NOTHROW_NONNULL_LEAF);

    ftype = build_function_type_list (void_type_node, NULL_TREE);
    gfc_define_builtin ("__builtin_abort", ftype, BUILT_IN_ABORT,
            "abort", ATTR_TMPURE_NORETURN_NOTHROW_LEAF_COLD_LIST);

    ftype = build_function_type_list (void_type_node,
                                      integer_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_exit", ftype, BUILT_IN_EXIT,
            "exit", ATTR_TMPURE_NORETURN_NOTHROW_LEAF_COLD_LIST);

    ftype = build_function_type_list (integer_type_node,
                                      const_char_pointer_type_node,
                                      const_char_pointer_type_node,
                                      size_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_strncmp", ftype, BUILT_IN_STRNCMP,
            "strncmp", ATTR_PURE_NOTHROW_NONNULL_LEAF);

    ftype = build_function_type_list (integer_type_node,
                                      const_char_pointer_type_node,
                                      const_char_pointer_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_strcmp", ftype, BUILT_IN_STRCMP,
            "strcmp", ATTR_PURE_NOTHROW_NONNULL_LEAF);

    ftype = build_function_type_list (char_pointer_type_node,
                                      char_pointer_type_node,
                                      const_char_pointer_type_node,
                                      NULL_TREE);
    gfc_define_builtin ("__builtin_strcpy", ftype, BUILT_IN_STRCPY,
            "strcpy", ATTR_NOTHROW_NONNULL_LEAF);

    build_common_builtin_nodes ();

    // Make sure this is a supported configuration.
    if( !targetm.scalar_mode_supported_p (TImode) || !float128_type_node )
      {
      sorry ("COBOL requires a 64-bit configuration");
      }

    return true;
    }


void cobol_set_debugging( bool flex, bool yacc, bool parser );
void cobol_set_indicator_column( int column );
void copybook_directory_add( const char gcob_copybook[] );
void copybook_extension_add( const char ext[] );
bool defined_cmd( const char arg[] );
void lexer_echo( bool tf );

static void
cobol_langhook_init_options_struct (struct gcc_options *opts) {
  opts->x_yy_flex_debug = 0;
  opts->x_yy_debug = 0;
  opts->x_cobol_trace_debug = 0;

  cobol_set_debugging( false, false, false );

  copybook_directory_add( getenv("GCOBOL_COPYBOOK") );
}

static unsigned int
cobol_option_lang_mask (void) {
  return CL_Cobol;
}

bool use_static_call( bool yn );
void add_cobol_exception( ec_type_t type, bool );

bool include_file_add(const char input[]);
bool preprocess_filter_add( const char filter[] );

bool max_errors_exceeded( int nerr ) {
  return flag_max_errors != 0 && flag_max_errors <= nerr;
}

static void
enable_exceptions( bool enable ) {
  for( char * name = xstrdup(cobol_exceptions);
       NULL != (name = strtok(name, ",")); name = NULL ) {
    ec_type_t type = ec_type_of(name);
    if( type == ec_none_e ) {
      yywarn("unrecognized exception '%s' was ignored", name);
      continue;
    }
    ec_disposition_t disposition = ec_type_disposition(type);
    if( disposition != ec_implemented(disposition) ) {
      cbl_unimplemented("exception '%s'", name);
    }
    add_cobol_exception(type, enable );
  }
}

static bool
cobol_langhook_handle_option (size_t scode,
                              const char *arg ATTRIBUTE_UNUSED,
                              HOST_WIDE_INT value,
                              int kind ATTRIBUTE_UNUSED,
                              location_t loc ATTRIBUTE_UNUSED,
                              const struct
                              cl_option_handlers *handlers ATTRIBUTE_UNUSED)
    {
    // process_command (decoded_options_count, decoded_options);
    enum opt_code code = (enum opt_code) scode;

    switch(code)
        {
        case OPT_D:
            defined_cmd(arg);
            return true;
        case OPT_E:
            lexer_echo(true);
            return true;

        case OPT_I:
            copybook_directory_add(arg);
            return true;
        case OPT_copyext:
            copybook_extension_add(cobol_copyext);
            return true;

        case OPT_M:
            cobol_set_pp_option('M');
            return true;

        case OPT_fstatic_call:
            use_static_call( arg? true : false );
            return true;

        case OPT_fdefaultbyte:
            wsclear(cobol_default_byte);
            return true;

        case OPT_fflex_debug: // cppcheck-suppress syntaxError // The need for this is a mystery
            yy_flex_debug = 1;
            cobol_set_debugging( true, yy_debug == 1, cobol_trace_debug == 1 );
            return true;

        case OPT_fyacc_debug:
            yy_debug = 1;
            cobol_set_debugging(yy_flex_debug == 1,
                                true,
                                cobol_trace_debug == 1 );
            return true;

        case OPT_ftrace_debug:
            cobol_set_debugging( yy_flex_debug == 1, yy_debug == 1, true );
            return true;

        case OPT_fcobol_exceptions: {
            if( cobol_exceptions[0] == '=' ) cobol_exceptions++;
            enable_exceptions(value == 1);
            return true;
        }

        case OPT_ffixed_form:
            cobol_set_indicator_column(-7);
            return true;
        case OPT_ffree_form:
            cobol_set_indicator_column(0);
            return true;

        case OPT_findicator_column:
            cobol_set_indicator_column( indicator_column );
            return true;

        case OPT_dialect:
            cobol_dialect_set(cbl_dialect_t(cobol_dialect));
            return true;

        case OPT_fsyntax_only:
          mode_syntax_only(identification_div_e);
          break;

        case OPT_preprocess:
          if( ! preprocess_filter_add(arg) ) {
            cbl_errx( "could not execute preprocessor %s", arg);
          }
          return true;

        case OPT_include:
          if( ! include_file_add(arg) ) {
            cbl_errx( "could not include %s", arg);
          }
            return true;

        case OPT_main:
            // This isn't right.  All OPT_main should be replaced
            error("We should never see a non-equal dash-main in cobol1.c");
            exit(1);
            return true;

        case OPT_main_:
            register_main_switch(cobol_main_string);
            return true;

        case OPT_nomain:
            return true;

        case OPT_finternal_ebcdic:
            cobol_gcobol_feature_set(feature_internal_ebcdic_e);
            return true;

        default:
            break;
        }

    Cobol_handle_option_auto (&global_options, &global_options_set,
                              scode, arg, value,
                              cobol_option_lang_mask (), kind,
                              loc, handlers, global_dc);

    return true;
    }

void
cobol_parse_files (int nfile, const char **files);

static void
cobol_langhook_parse_file (void)
    {
    cobol_parse_files (num_in_fnames, in_fnames);
    }

static tree
cobol_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
    {
    if (mode == TYPE_MODE (float_type_node))
        return float_type_node;

    if (mode == TYPE_MODE (double_type_node))
        return double_type_node;

    if (mode == TYPE_MODE (float32_type_node))
        return float32_type_node;

    if (mode == TYPE_MODE (float64_type_node))
        return float64_type_node;

    if (mode == TYPE_MODE (float128_type_node))
        return float128_type_node;

    if (mode == TYPE_MODE (intQI_type_node))
        return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
    if (mode == TYPE_MODE (intHI_type_node))
        return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
    if (mode == TYPE_MODE (intSI_type_node))
        return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
    if (mode == TYPE_MODE (intDI_type_node))
        return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
    if (mode == TYPE_MODE (intTI_type_node))
        return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

    if (mode == TYPE_MODE (integer_type_node))
        return unsignedp ? unsigned_type_node : integer_type_node;

    if (mode == TYPE_MODE (long_integer_type_node))
        return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (mode == TYPE_MODE (long_long_integer_type_node))
        return unsignedp ? long_long_unsigned_type_node
               : long_long_integer_type_node;

    if (COMPLEX_MODE_P (mode))
        {
        if (mode == TYPE_MODE (complex_float_type_node))
            return complex_float_type_node;
        if (mode == TYPE_MODE (complex_double_type_node))
            return complex_double_type_node;
        if (mode == TYPE_MODE (complex_long_double_type_node))
            return complex_long_double_type_node;
        if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
            return complex_integer_type_node;
        }

    return NULL;
    }

////static tree
////cobol_langhook_type_for_size (unsigned int bits ATTRIBUTE_UNUSED,
////                              int unsignedp ATTRIBUTE_UNUSED)
////    {
////    gcc_unreachable ();
////    return NULL;
////    }

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
cobol_langhook_builtin_function (tree decl)
    {
    return decl;
    }

static bool
cobol_langhook_global_bindings_p (void)
    {
    return false;
    }

static tree
cobol_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
    {
    // This function is necessary, but is apparently never being called
    gcc_unreachable ();
    }

static tree
cobol_langhook_getdecls (void)
    {
    return NULL;
    }

char *
cobol_name_mangler(const char *cobol_name_)
    {
    // The caller should free the returned string.

    // This is a solution to the problem of hyphens and the fact that COBOL
    // names can start with digits.
    //
    // COBOL names can't start with underscore; GNU assembler names can.
    // Assembler names can't start with a digit 0-9; COBOL names can.
    //
    // We convert all COBOL names to lowercase, so uppercase characters aren't
    // seen.
    //
    // COBOL names can have hyphens; assembler names can't.
    //
    // So if a name starts with a digit, we prepend an underscore.
    // We convert the whole name to lowercase.
    // We replace hyphens with '$'
    //

    if( !cobol_name_ )
      {
      return nullptr;
      }

    // Allocate enough space for a prepended underscore and a final '\0'
    char *cobol_name = static_cast<char *>(xmalloc(strlen(cobol_name_)+2));
    size_t n = 0;
    if( cobol_name_[0] >= '0' && cobol_name_[0] <= '9' )
      {
      // The name starts with 0-9, so we are going to lead it
      // with an underscore
      cobol_name[n++] = '_';
      }
    for(size_t i=0; i<strlen(cobol_name_); i++)
      {
      // Convert to lowercase, replacing '-' with '$'
      int ch = cobol_name_[i];
      if( ch == '-' )
        {
        cobol_name[n++] = '$';
        }
      else
        {
        cobol_name[n++] = TOLOWER(ch);
        }
      }
    cobol_name[n++] = '\0';

    return cobol_name;
    }

cbl_call_convention_t parser_call_target_convention( tree func );

static
void
cobol_set_decl_assembler_name (tree decl)
    {
    tree id;

      /* set_decl_assembler_name may be called on TYPE_DECL to record ODR
         name for C++ types.  By default types have no ODR names.  */
    if (TREE_CODE (decl) == TYPE_DECL)
        {
        return;
        }

    /* The language-independent code should never use the
     DECL_ASSEMBLER_NAME for lots of DECLs.  Only FUNCTION_DECLs and
     VAR_DECLs for variables with static storage duration need a real
     DECL_ASSEMBLER_NAME.  */
    gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
        || (VAR_P (decl) && (TREE_STATIC (decl)
                            || DECL_EXTERNAL (decl)
                            || TREE_PUBLIC (decl))));

    const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
    char *mangled_name = cobol_name_mangler(name);

    // A verbatim CALL does not get mangled.
    if( cbl_call_verbatim_e == parser_call_target_convention(decl) )
      {
      strcpy(mangled_name, name);
      }

    id = get_identifier(mangled_name);
    free(mangled_name);

    SET_DECL_ASSEMBLER_NAME (decl, id);
    }

/* Get a value for the SARIF v2.1.0 "artifact.sourceLanguage" property,
   based on the list in SARIF v2.1.0 Appendix J.  */

const char *
cobol_get_sarif_source_language(const char *)
    {
    return "cobol";
    }

#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_TYPE_FOR_MODE
////#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_SET_DECL_ASSEMBLER_NAME
#undef LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE

// We use GCC in the name, not GNU, as others do,
// because "GnuCOBOL" refers to a different GNU project.
// https://www.gnu.org/software/software.html
#define LANG_HOOKS_NAME "GCC COBOL"

#define LANG_HOOKS_INIT                     cobol_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK         cobol_option_lang_mask

#define LANG_HOOKS_INIT_OPTIONS_STRUCT      cobol_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION            cobol_langhook_handle_option

#define LANG_HOOKS_BUILTIN_FUNCTION         cobol_langhook_builtin_function
#define LANG_HOOKS_GETDECLS                 cobol_langhook_getdecls
#define LANG_HOOKS_GLOBAL_BINDINGS_P        cobol_langhook_global_bindings_p
#define LANG_HOOKS_PARSE_FILE               cobol_langhook_parse_file
#define LANG_HOOKS_PUSHDECL                 cobol_langhook_pushdecl

#define LANG_HOOKS_TYPE_FOR_MODE            cobol_langhook_type_for_mode
////#define LANG_HOOKS_TYPE_FOR_SIZE            cobol_langhook_type_for_size

#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME cobol_set_decl_assembler_name

#define LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE cobol_get_sarif_source_language

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-cobol-cobol1.h"
#include "gtype-cobol.h"
