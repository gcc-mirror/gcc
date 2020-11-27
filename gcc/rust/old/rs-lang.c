// rs-lang.cc - frontend interface or something
// NOTE: should be mentioned in gtfiles in config-lang.in
/* This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.
*/
#include "rust.h"

// May not need this?
char* GRS_current_infname;
char* GRS_current_infile;

/* Language-dependent contents of a type. GTY() mark used for garbage collector. */
struct GTY(()) lang_type {
    char dummy;
};

/* Language-dependent contents of a decl.  */
struct GTY(()) lang_decl {
    char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.
*/
struct GTY(()) lang_identifier {
    struct tree_identifier common;
};

/* The resulting tree type.  */
union GTY((desc("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
           chain_next("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
           "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node {
    union tree_node GTY((tag("0"),
                         desc("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY((tag("1"))) identifier;
};

/* We don't use language_function.  */
struct GTY(()) language_function {
    int dummy;
};

/* Language hooks.  */

/* Initial lang hook called (possibly), used for initialisation. 
 * Must call build_common_tree_nodes, set_sizetype, build_common_tree_nodes_2, and 
 * build_common_builtin_nodes, as well as set global variable void_list_node. */
static bool grs_langhook_init(void) {
    /* Something to do with this: 
     This allows the code in d-builtins.cc to not have to worry about
     converting (C signed char *) to (D char *) for string arguments of
     built-in functions. The parameter (signed_char = false) specifies
     whether char is signed.  */
    build_common_tree_nodes(false);

    // Creates a new TREE_LIST node with purpose NULL_TREE and value void_type_node
    void_list_node = build_tree_list(NULL_TREE, void_type_node);

    // Builds built-ins for middle-end after all front-end built-ins are already instantiated
    build_common_builtin_nodes();

    mpfr_set_default_prec(128);
    using_eh_for_cleanups();

    rdot_init();
    return true;
}

/* Initialize before parsing options.  */
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

    /* We turn on stack splitting if we can.  */
    if (targetm_common.supports_split_stack(false, opts))
        opts->x_flag_split_stack = 1;

    /* Exceptions are used to handle recovering from panics.  */
    opts->x_flag_exceptions = 1;
    opts->x_flag_non_call_exceptions = 1;
}

/* Handle grs specific options.  Return 0 if we didn't do anything.  */
static bool grs_langhook_handle_option(size_t scode, const char* arg ATTRIBUTE_UNUSED, 
    int value ATTRIBUTE_UNUSED, int kind ATTRIBUTE_UNUSED, location_t l ATTRIBUTE_UNUSED, 
    const struct cl_option_handlers* handlers ATTRIBUTE_UNUSED) {
    enum opt_code code = (enum opt_code)scode;
    int retval = 1;

    switch (code) {
            /* ignore options for now... */

        default:
            break;
    }

    return retval;
}

/* Run after parsing options.  */
static bool grs_langhook_post_options(const char** pfilename ATTRIBUTE_UNUSED) {
    if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
        flag_excess_precision_cmdline = EXCESS_PRECISION_STANDARD;

    /* Returning false means that the backend should be used.  */
    return false;
}

/* Main entry point for front-end, apparently. Finds input file names in global vars in_fnames and 
 * num_in_fnames. From this, frontend can take over and do actual parsing and initial compilation.
 * This function must create a complete parse tree in a global var, and then return. 
 * 
 * Some consider this the "start of compilation". */ 
static void grs_langhook_parse_file(void) {
    size_t idx;

    // loop through all files in in_fnames array
    for (idx = 0; idx < num_in_fnames; ++idx) {
        const char* in = in_fnames[idx];
        GRS_current_infname = xstrdup(in);
        GRS_current_infile = basename(GRS_current_infname);

        // parse file. seems to call flex-generated lexer, which then calls bison-generated parser
        grs_do_compile(in);
        // seems very coupled to old gccrs' lexer - e.g. rustc lexer handles parsing outside of the lexer
        // note that apparently bison generates the "yyparse" automatically - perhaps the rs-parser.cc was actually generated using bison
    }
}

static tree grs_langhook_type_for_mode(enum machine_mode mode, int unsignedp) {
    if (mode == TYPE_MODE(float_type_node))
        return float_type_node;

    if (mode == TYPE_MODE(double_type_node))
        return double_type_node;

    // More code about other type_nodes here? intQI, intHI, intSI, intDI, intTI?
    // Don't know what they mean, but one site has them here in a similar fashion of selections

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
    /* gcc_unreachable */
    return NULL;
}

static tree grs_langhook_type_for_size(unsigned int bits ATTRIBUTE_UNUSED, 
    int unsignedp ATTRIBUTE_UNUSED) {
    gcc_unreachable();
    return NULL_TREE;
}

/* Record a builtin function.  We just ignore builtin functions.  */
static tree grs_langhook_builtin_function(tree decl ATTRIBUTE_UNUSED) {
    return decl;
}

static bool grs_langhook_global_bindings_p(void) {
    return current_function_decl == NULL_TREE;
}

static tree grs_langhook_pushdecl(tree decl ATTRIBUTE_UNUSED) {
    gcc_unreachable();
    return NULL;
}

static tree grs_langhook_getdecls(void) {
    gcc_unreachable();
    return NULL;
}

/* Write out globals.  */
static void grs_langhook_write_globals(void) {
    // pass off to middle end function basically.
    dot_pass_WriteGlobals();
}

static unsigned int grs_langhook_option_lang_mask(void) {
    return CL_Rust;
}

/* Return a decl for the exception personality function.  The function
   itself is implemented in libgo/runtime/go-unwind.c.  */
static tree grs_langhook_eh_personality(void) {
    static tree personality_decl;
    if (personality_decl == NULL_TREE) {
        personality_decl = build_personality_function("gccrs");
        grs_preserve_from_gc(personality_decl);
    }
    return personality_decl;
}

static int
grs_langhook_gimplify_expr(tree* expr_p ATTRIBUTE_UNUSED,
                           gimple_seq* pre_p ATTRIBUTE_UNUSED,
                           gimple_seq* post_p ATTRIBUTE_UNUSED) {
    if (TREE_CODE(*expr_p) == CALL_EXPR && CALL_EXPR_STATIC_CHAIN(*expr_p) != NULL_TREE)
        gimplify_expr(&CALL_EXPR_STATIC_CHAIN(*expr_p), pre_p, post_p, is_gimple_val, fb_rvalue);
    /* Often useful to use debug_tree here to see whats going on because
     ever gimplication calls this. */
    // debug_tree (*expr_p)
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

#define LANG_HOOKS_NAME "GNU Rust"
#define LANG_HOOKS_INIT grs_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK grs_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT grs_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION grs_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS grs_langhook_post_options
/* Main lang-hook, apparently. Finds input file names in global vars in_fnames and num_in_fnames
 * From this, frontend can take over and do actual parsing and initial compilation.
 * This hook must create a complete parse tree in a global var, and then return. */ 
#define LANG_HOOKS_PARSE_FILE grs_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE grs_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE grs_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION grs_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P grs_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL grs_langhook_pushdecl
#define LANG_HOOKS_GETDECLS grs_langhook_getdecls
#define LANG_HOOKS_WRITE_GLOBALS grs_langhook_write_globals
#define LANG_HOOKS_GIMPLIFY_EXPR grs_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY grs_langhook_eh_personality

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

// These are for GCC's garbage collector to work properly or something
#include "gt-rust-rs-lang.h"
#include "gtype-rust.h"