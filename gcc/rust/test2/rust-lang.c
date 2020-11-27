/* Rust-lang.c - Rust frontend GCC interface 

License, copyright stuff, etc, whatever. */

#include "common/common-target.h"
#include "config.h"
#include "convert.h"
#include "coretypes.h"
#include "debug.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "opts.h"
#include "stor-layout.h"
#include "system.h"
#include "target.h"
#include "tree.h"

#include <mpfr.h>

#include "rust-c.h"
//#include "rust-gcc.h"

/* Language-dependent contents of a type.  */
// seems to be a "gengtype" of some kind
struct GTY(()) lang_type {
    char dummy;
};

/* Language-dependent contents of a decl.  */
struct GTY(()) lang_decl {
    char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */
struct GTY(()) lang_identifier {
    struct tree_identifier common;
};

/* The resulting tree type.  */
union GTY((desc("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
           chain_next("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node {
    union tree_node GTY((tag("0"),
                         desc("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY((tag("1"))) identifier;
};

/* We don't use language_function.  */
struct GTY(()) language_function {
    int dummy;
};

/* Option information we need to pass to rust_create_rustrust.  */
static const char* rust_pkgpath = NULL;
static const char* rust_prefix = NULL;
static const char* rust_relative_import_path = NULL;
static const char* rust_c_header = NULL;

/* Language hooks.  */
static bool grs_langhook_init(void) {
    build_common_tree_nodes(false);

    /* I don't know why this has to be done explicitly.  */
    void_list_node = build_tree_list(NULL_TREE, void_type_node);

    /* We must create the rustrust IR after calling build_common_tree_nodes
     (because Gorust::define_builtin_function_trees refers indirectly
     to, e.g., unsigned_char_type_node) but before calling
     build_common_builtin_nodes (because it calls, indirectly,
     rust_type_for_size).  */
    struct rust_create_rustrust_args args;
    args.int_type_size = INT_TYPE_SIZE;
    args.pointer_size = POINTER_SIZE;
    args.pkgpath = rust_pkgpath;
    args.prefix = rust_prefix;
    args.relative_import_path = rust_relative_import_path;
    args.c_header = rust_c_header;
    args.check_divide_by_zero = rust_check_divide_zero;
    args.check_divide_overflow = rust_check_divide_overflow;
    args.compiling_runtime = rust_compiling_runtime;
    args.debug_escape_level = rust_debug_escape_level;
    args.debug_escape_hash = rust_debug_escape_hash;
    args.nil_check_size_threshold = TARGET_AIX ? -1 : 4096;
    args.debug_optimization = rust_debug_optimization;
    args.linemap = rust_get_linemap();
    args.backend = rust_get_backend();
    rust_create_rustrust(&args);

    build_common_builtin_nodes();

    /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
    mpfr_set_default_prec(256);

    /* Go uses exceptions.  */
    using_eh_for_cleanups();

    return true;
}

/* The option mask.  */
static unsigned int grs_langhook_option_lang_mask(void) {
    return CL_Go;
}

/* Initialize the options structure.  */
static void grs_langhook_init_options_struct(struct gcc_options* opts) {
    /* Go says that signed overflow is precisely defined.  */
    opts->x_flag_wrapv = 1;

    /* We default to using strict aliasing, since Go pointers are safe.
     This is turned off for code that imports the "unsafe" package,
     because using unsafe.pointer violates C style aliasing
     requirements.  */
    opts->x_flag_strict_aliasing = 1;

    /* Default to avoiding range issues for complex multiply and
     divide.  */
    opts->x_flag_complex_method = 2;

    /* The builtin math functions should not set errno.  */
    opts->x_flag_errno_math = 0;
    opts->frontend_set_flag_errno_math = true;

    /* Exceptions are used to handle recovering from panics.  */
    opts->x_flag_exceptions = 1;
    opts->x_flag_non_call_exceptions = 1;

    /* We need to keep pointers live for the garbage collector.  */
    opts->x_flag_keep_gc_roots_live = 1;

    /* Go programs expect runtime.Callers to work, and that uses
     libbacktrace that uses debug info.  Set the debug info level to 1
     by default.  In post_options we will set the debug type if the
     debug info level was not set back to 0 on the command line.  */
    opts->x_debug_info_level = DINFO_LEVEL_TERSE;
}

/* Infrastructure for a vector of char * pointers.  */
typedef const char* rust_char_p;

/* The list of directories to search after all the Go specific
   directories have been searched.  */
static vec<rust_char_p> rust_search_dirs;

/* Handle Go specific options.  Return 0 if we didn't do anything.  */
static bool grs_langhook_handle_option(
  size_t scode,
  const char* arg,
  HOST_WIDE_INT value,
  int kind ATTRIBUTE_UNUSED,
  location_t loc ATTRIBUTE_UNUSED,
  const struct cl_option_handlers* handlers ATTRIBUTE_UNUSED) {
    enum opt_code code = (enum opt_code)scode;
    bool ret = true;

    switch (code) {
        case OPT_I:
            rust_add_search_path(arg);
            break;

        case OPT_L:
            /* A -L option is assumed to come from the compiler driver.
	 This is a system directory.  We search the following
	 directories, if they exist, before this one:
	   dir/rust/VERSION
	   dir/rust/VERSION/MACHINE
	 This is like include/c++.  */
            {
                static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };
                size_t len;
                char* p;
                struct stat st;

                len = strlen(arg);
                p = XALLOCAVEC(char,
                               (len + sizeof "rust" + sizeof DEFAULT_TARGET_VERSION + 
                                sizeof DEFAULT_TARGET_MACHINE + 3));
                strcpy(p, arg);
                if (len > 0 && !IS_DIR_SEPARATOR(p[len - 1]))
                    strcat(p, dir_separator_str);
                strcat(p, "rust");
                strcat(p, dir_separator_str);
                strcat(p, DEFAULT_TARGET_VERSION);
                if (stat(p, &st) == 0 && S_ISDIR(st.st_mode)) {
                    rust_add_search_path(p);
                    strcat(p, dir_separator_str);
                    strcat(p, DEFAULT_TARGET_MACHINE);
                    if (stat(p, &st) == 0 && S_ISDIR(st.st_mode))
                        rust_add_search_path(p);
                }

                /* Search ARG too, but only after we've searched to Go
	   specific directories for all -L arguments.  */
                rust_search_dirs.safe_push(arg);
            }
            break;

        case OPT_frust_dump_:
            ret = rust_enable_dump(arg) ? true : false;
            break;

        case OPT_frust_optimize_:
            ret = rust_enable_optimize(arg, value) ? true : false;
            break;

        case OPT_frust_pkgpath_:
            rust_pkgpath = arg;
            break;

        case OPT_frust_prefix_:
            rust_prefix = arg;
            break;

        case OPT_frust_relative_import_path_:
            rust_relative_import_path = arg;
            break;

        case OPT_frust_c_header_:
            rust_c_header = arg;
            break;

        default:
            /* Just return 1 to indicate that the option is valid.  */
            break;
    }

    return ret;
}

/* Run after parsing options.  */
static bool grs_langhook_post_options(const char** pfilename ATTRIBUTE_UNUSED) {
    unsigned int ix;
    const char* dir;

    gcc_assert(num_in_fnames > 0);

    FOR_EACH_VEC_ELT(rust_search_dirs, ix, dir)
    rust_add_search_path(dir);
    rust_search_dirs.release();

    if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
        flag_excess_precision_cmdline = EXCESS_PRECISION_STANDARD;

    /* Tail call optimizations can confuse uses of runtime.Callers.  */
    if (!global_options_set.x_flag_optimize_sibling_calls)
        global_options.x_flag_optimize_sibling_calls = 0;

    /* If the debug info level is still 1, as set in init_options, make
     sure that some debugging type is selected.  */
    if (global_options.x_debug_info_level == DINFO_LEVEL_TERSE 
        && global_options.x_write_symbols == NO_DEBUG)
        global_options.x_write_symbols = PREFERRED_DEBUGGING_TYPE;

    /* We turn on stack splitting if we can.  */
    if (!global_options_set.x_flag_split_stack 
        && targetm_common.supports_split_stack(false, &global_options))
        global_options.x_flag_split_stack = 1;

    /* If stack splitting is turned on, and the user did not explicitly
     request function partitioning, turn off partitioning, as it
     confuses the linker when trying to handle partitioned split-stack
     code that calls a non-split-stack function.  */
    if (global_options.x_flag_split_stack && global_options.x_flag_reorder_blocks_and_partition 
        && !global_options_set.x_flag_reorder_blocks_and_partition)
        global_options.x_flag_reorder_blocks_and_partition = 0;

    /* Returning false means that the backend should be used.  */
    return false;
}

/* Main entry point for front-end, apparently. Finds input file names in global vars in_fnames and 
 * num_in_fnames. From this, frontend can take over and do actual parsing and initial compilation.
 * This function must create a complete parse tree in a global var, and then return. 
 * 
 * Some consider this the "start of compilation". */ 
static void grs_langhook_parse_file(void) {
    grs_parse_input_files(in_fnames, num_in_fnames, flag_syntax_only, rust_require_return_statement);

    /* Final processing of globals and early debug info generation.  */
    rust_write_globals();
}

static tree grs_langhook_type_for_size(unsigned int bits, int unsignedp) {
    tree type;
    if (unsignedp) {
        if (bits == INT_TYPE_SIZE)
            type = unsigned_type_node;
        else if (bits == CHAR_TYPE_SIZE)
            type = unsigned_char_type_node;
        else if (bits == SHORT_TYPE_SIZE)
            type = short_unsigned_type_node;
        else if (bits == LONG_TYPE_SIZE)
            type = long_unsigned_type_node;
        else if (bits == LONG_LONG_TYPE_SIZE)
            type = long_long_unsigned_type_node;
        else
            type = make_unsigned_type(bits);
    } else {
        if (bits == INT_TYPE_SIZE)
            type = integer_type_node;
        else if (bits == CHAR_TYPE_SIZE)
            type = signed_char_type_node;
        else if (bits == SHORT_TYPE_SIZE)
            type = short_integer_type_node;
        else if (bits == LONG_TYPE_SIZE)
            type = long_integer_type_node;
        else if (bits == LONG_LONG_TYPE_SIZE)
            type = long_long_integer_type_node;
        else
            type = make_signed_type(bits);
    }
    return type;
}

static tree grs_langhook_type_for_mode(machine_mode mode, int unsignedp) {
    tree type;
    /* Go has no vector types.  Build them here.  FIXME: It does not
     make sense for the middle-end to ask the frontend for a type
     which the frontend does not support.  However, at least for now
     it is required.  See PR 46805.  */
    if (GET_MODE_CLASS(mode) == MODE_VECTOR_BOOL && valid_vector_subparts_p(GET_MODE_NUNITS(mode))) {
        unsigned int elem_bits = vector_element_size(GET_MODE_BITSIZE(mode),
                                                     GET_MODE_NUNITS(mode));
        tree bool_type = build_nonstandard_boolean_type(elem_bits);
        return build_vector_type_for_mode(bool_type, mode);
    } else if (VECTOR_MODE_P(mode) && valid_vector_subparts_p(GET_MODE_NUNITS(mode))) {
        tree inner;

        inner = grs_langhook_type_for_mode(GET_MODE_INNER(mode), unsignedp);
        if (inner != NULL_TREE)
            return build_vector_type_for_mode(inner, mode);
        return NULL_TREE;
    }

    scalar_int_mode imode;
    scalar_float_mode fmode;
    complex_mode cmode;
    if (is_int_mode(mode, &imode))
        return grs_langhook_type_for_size(GET_MODE_BITSIZE(imode), unsignedp);
    else if (is_float_mode(mode, &fmode)) {
        switch (GET_MODE_BITSIZE(fmode)) {
            case 32:
                return float_type_node;
            case 64:
                return double_type_node;
            default:
                // We have to check for long double in order to support
                // i386 excess precision.
                if (fmode == TYPE_MODE(long_double_type_node))
                    return long_double_type_node;
        }
    } else if (is_complex_float_mode(mode, &cmode)) {
        switch (GET_MODE_BITSIZE(cmode)) {
            case 64:
                return complex_float_type_node;
            case 128:
                return complex_double_type_node;
            default:
                // We have to check for long double in order to support
                // i386 excess precision.
                if (cmode == TYPE_MODE(complex_long_double_type_node))
                    return complex_long_double_type_node;
        }
    }

#if HOST_BITS_PER_WIDE_INT >= 64
    /* The middle-end and some backends rely on TImode being supported
     for 64-bit HWI.  */
    if (mode == TImode) {
        type = build_nonstandard_integer_type(GET_MODE_BITSIZE(TImode),
                                              unsignedp);
        if (type && TYPE_MODE(type) == TImode)
            return type;
    }
#endif
    return NULL_TREE;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree grs_langhook_builtin_function(tree decl) {
    return decl;
}

/* Return true if we are in the global binding level.  */

static bool grs_langhook_global_bindings_p(void) {
    return current_function_decl == NULL_TREE;
}

/* Push a declaration into the current binding level.  We can't
   usefully implement this since we don't want to convert from tree
   back to one of our internal data structures.  I think the only way
   this is used is to record a decl which is to be returned by
   getdecls, and we could implement it for that purpose if
   necessary.  */

static tree grs_langhook_pushdecl(tree decl ATTRIBUTE_UNUSED) {
    gcc_unreachable();
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use the write_globals hook.  This
   can't simply crash because it is called by -gstabs.  */

static tree grs_langhook_getdecls(void) {
    return NULL;
}

/* Go specific gimplification.  We need to gimplify
   CALL_EXPR_STATIC_CHAIN, because the gimplifier doesn't handle
   it.  */

static int grs_langhook_gimplify_expr(tree* expr_p, gimple_seq* pre_p, gimple_seq* post_p) {
    if (TREE_CODE(*expr_p) == CALL_EXPR && CALL_EXPR_STATIC_CHAIN(*expr_p) != NULL_TREE)
        gimplify_expr(&CALL_EXPR_STATIC_CHAIN(*expr_p), pre_p, post_p, is_gimple_val, fb_rvalue);
    return GS_UNHANDLED;
}

/* Return a decl for the exception personality function.  The function
   itself is implemented in librust/runtime/rust-unwind.c.  */

static tree grs_langhook_eh_personality(void) {
    static tree personality_decl;
    if (personality_decl == NULL_TREE) {
        personality_decl = build_personality_function("gccrust");
        rust_preserve_from_gc(personality_decl);
    }
    return personality_decl;
}

/* Functions called directly by the generic backend.  */

tree convert(tree type, tree expr) {
    if (type == error_mark_node || expr == error_mark_node || TREE_TYPE(expr) == error_mark_node)
        return error_mark_node;

    if (type == TREE_TYPE(expr))
        return expr;

    if (TYPE_MAIN_VARIANT(type) == TYPE_MAIN_VARIANT(TREE_TYPE(expr)))
        return fold_convert(type, expr);

    switch (TREE_CODE(type)) {
        case VOID_TYPE:
        case BOOLEAN_TYPE:
            return fold_convert(type, expr);
        case INTEGER_TYPE:
            return fold(convert_to_integer(type, expr));
        case POINTER_TYPE:
            return fold(convert_to_pointer(type, expr));
        case REAL_TYPE:
            return fold(convert_to_real(type, expr));
        case COMPLEX_TYPE:
            return fold(convert_to_complex(type, expr));
        default:
            break;
    }

    gcc_unreachable();
}

/* Convert an identifier for use in an error message.  */

const char* rust_localize_identifier(const char* ident) {
    return identifier_to_locale(ident);
}

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME "GNU Rust"
#define LANG_HOOKS_INIT grs_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK grs_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT grs_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION grs_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS grs_langhook_post_options
#define LANG_HOOKS_PARSE_FILE grs_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE grs_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE grs_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION grs_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P grs_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL grs_langhook_pushdecl
#define LANG_HOOKS_GETDECLS grs_langhook_getdecls
#define LANG_HOOKS_GIMPLIFY_EXPR grs_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY grs_langhook_eh_personality

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

// These are for GCC's garbage collector to work properly or something
#include "gt-rust-rust-lang.h"
#include "gtype-rust.h"
