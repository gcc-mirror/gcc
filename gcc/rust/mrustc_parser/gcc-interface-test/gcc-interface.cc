// gcc interface main - do stuff here

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

#include "rust-c-interface.h"

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
           chain_next("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? 
           ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node {
    union tree_node GTY((tag("0"),
                         desc("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY((tag("1"))) identifier;
};

/* We don't use language_function.  */
struct GTY(()) language_function {
    int dummy;
};

/* Option information we need to pass to rust_create_grs.  */

static const char* rust_pkgpath = NULL;
static const char* rust_prefix = NULL;
static const char* rust_relative_import_path = NULL;
static const char* rust_c_header = NULL;

/* Language hooks.  */

static bool grs_langhook_init(void) {
    build_common_tree_nodes(false);

    /* I don't know why this has to be done explicitly.  */
    void_list_node = build_tree_list(NULL_TREE, void_type_node);

    // not currently relevant
    /* We must create the grs IR after calling build_common_tree_nodes
     (because Gorust::define_builtin_function_trees refers indirectly
     to, e.g., unsigned_char_type_node) but before calling
     build_common_builtin_nodes (because it calls, indirectly,
     rust_type_for_size).  */
    /* struct rust_create_grs_args args;
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
    rust_create_grs(&args);*/

    build_common_builtin_nodes();

    /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
    mpfr_set_default_prec(128);

    /* Rust uses exceptions.  */
    using_eh_for_cleanups();

    // maybe get rid of this?
    rdot_init();

    return true;
}

/* Initialize the options structure before parsing options.  */
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

    /* Turn on stack splitting if possible? */
    // no definition for that anymore, apparently
    /* if (targetm_common.supports_split_stack(false, opts))
        opts->x_flag_split_stack = 1; */

    /* Exceptions are used to handle recovering from panics.  */
    opts->x_flag_exceptions = 1;
    opts->x_flag_non_call_exceptions = 1;

    // nope, don't need this because no gc
    /* We need to keep pointers live for the garbage collector.  */
    //opts->x_flag_keep_gc_roots_live = 1;

    /* Go programs expect runtime.Callers to work, and that uses
     libbacktrace that uses debug info.  Set the debug info level to 1
     by default.  In post_options we will set the debug type if the
     debug info level was not set back to 0 on the command line.  */
    opts->x_debug_info_level = DINFO_LEVEL_TERSE;
}

// doesn't seem to be needed, currently
/*  Infrastructure for a vector of char * pointers.  */
/* typedef const char* rust_char_p;*/

/* The list of directories to search after all the Go specific
   directories have been searched.  */
/* static vec<rust_char_p> rust_search_dirs;*/

/* Handle grs specific options.  Return 0 if we didn't do anything.  */
static bool grs_langhook_handle_option(size_t scode, const char* arg ATTRIBUTE_UNUSED, 
    int value ATTRIBUTE_UNUSED, int kind ATTRIBUTE_UNUSED, location_t loc ATTRIBUTE_UNUSED, 
    const struct cl_option_handlers* handlers ATTRIBUTE_UNUSED) {
    enum opt_code code = (enum opt_code)scode;
    bool ret = true;

    // ignore options...
    switch (code) {

        default:
            /* Just return 1 to indicate that the option is valid.  */
            break;
    }

    return ret;
}

/* Run after parsing options.  */
static bool grs_langhook_post_options(const char** pfilename ATTRIBUTE_UNUSED) {

    if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
        flag_excess_precision_cmdline = EXCESS_PRECISION_STANDARD;

    /* We turn on stack splitting if we can.  */
    if (!global_options_set.x_flag_split_stack && targetm_common.supports_split_stack(false, 
        &global_options))
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

static void grs_langhook_parse_file(void) {
    //grs_parse_input_files(in_fnames, num_in_fnames, flag_syntax_only, rust_require_return_statement);

    /* Final processing of globals and early debug info generation.  */
    //rust_write_globals();

    // loop through all input file names
    for (size_t idx = 0; idx < num_in_fnames; idx++) {
        // get input file string for index
        const char* in = in_fnames[idx];
        GRS_current_infname = xstrdup(in);
        GRS_current_infile = basename(GRS_current_infname);
        grs_do_compile(in); // calls lexer?
    }
}

static tree grs_langhook_type_for_size(unsigned int bits ATTRIBUTE_UNUSED,
                                       int unsignedp ATTRIBUTE_UNUSED) {
    /* tree type;
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
    return type;*/

    gcc_unreachable();

    return NULL_TREE;
}

static tree grs_langhook_type_for_mode(enum machine_mode mode, int unsignedp) {
    if (mode == TYPE_MODE(float_type_node))
        return float_type_node;

    if (mode == TYPE_MODE(double_type_node))
        return double_type_node;

    if (mode == TYPE_MODE(integer_type_node))
        return unsignedp ? unsigned_type_node : integer_type_node;

    if (mode == TYPE_MODE(long_integer_type_node))
        return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (COMPLEX_MODE_P(mode)) {
        if (mode == TYPE_MODE(complex_float_type_node))
            return complex_float_type_node;

        if (mode == TYPE_MODE(complex_double_type_node))
            return complex_double_type_node;

        if (mode == TYPE_MODE(complex_long_double_type_node))
            return complex_long_double_type_node;

        if (mode == TYPE_MODE(complex_integer_type_node) && !unsignedp)
            return complex_integer_type_node;
    }

    /* gcc unreachable */
    return NULL;
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
    return NULL;
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use the write_globals hook.  This
   can't simply crash because it is called by -gstabs.  */
static tree grs_langhook_getdecls(void) {
    gcc_unreachable();
    return NULL;
}

/* Write out globals. */
static void grs_langhook_write_globals(void) {
    // pass off to middle-end function, basically
    dot_pass_WriteGlobals();
}

static unsigned int grs_langhook_option_lang_mask(void) {
    return CL_Rust;
}

/* Return a decl for the exception personality function.  The function
   itself is implemented in librust/runtime/rust-unwind.c.  */
static tree grs_langhook_eh_personality(void) {
    static tree personality_decl;

    if (personality_decl == NULL_TREE) {
        personality_decl = build_personality_function("gccrs");
        grs_preserve_from_gc(personality_decl);
    }

    return personality_decl;
}

/* Rust-specific gimplification.  We need to gimplify
   CALL_EXPR_STATIC_CHAIN, because the gimplifier doesn't handle
   it.  */
static int grs_langhook_gimplify_expr(tree* expr_p ATTRIBUTE_UNUSED, 
    gimple_seq* pre_p ATTRIBUTE_UNUSED, gimple_seq* post_p ATTRIBUTE_UNUSED) {
    if (TREE_CODE(*expr_p) == CALL_EXPR && CALL_EXPR_STATIC_CHAIN(*expr_p) != NULL_TREE)
        gimplify_expr(&CALL_EXPR_STATIC_CHAIN(*expr_p), pre_p, post_p, is_gimple_val, fb_rvalue);

    /* Often useful to use debug_tree here to see what's going on because every gimplification calls
       this. */
    //debug_tree(*expr_p)
    return GS_UNHANDLED;
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

static GTY(()) tree grs_gc_root;
void grs_preserve_from_gc(tree t) {
    grs_gc_root = tree_cons(NULL_TREE, t, grs_gc_root);
}

// seems to be unused
/* Convert an identifier for use in an error message.  */
/* const char* rust_localize_identifier(const char* ident) {
    return identifier_to_locale(ident);
}*/

/* The language hooks data structure. This is the main interface between the GCC front-end
 * and the GCC middle-end/back-end. A list of language hooks could be found in
 * <gcc>/langhooks.h
 */
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
#undef LANG_HOOKS_WRITE_GLOBALS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME                 "GNU Rust"
#define LANG_HOOKS_INIT                 grs_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK     grs_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT  grs_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION        grs_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS         grs_langhook_post_options
#define LANG_HOOKS_PARSE_FILE           grs_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE        grs_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE        grs_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION     grs_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P    grs_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL             grs_langhook_pushdecl
#define LANG_HOOKS_GETDECLS             grs_langhook_getdecls
#define LANG_HOOKS_WRITE_GLOBALS        grs_langhook_write_globals
#define LANG_HOOKS_GIMPLIFY_EXPR        grs_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY       grs_langhook_eh_personality

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-rust-rs-lang.h"
#include "gtype-rust.h"
// TODO: something to do with this, maybe
