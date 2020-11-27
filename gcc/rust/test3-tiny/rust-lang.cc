#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "gimple-expr.h"
#include "diagnostic.h"
#include "opts.h"
#include "fold-const.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "debug.h"
#include "convert.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "common/common-target.h"
// note: header files must be in this order or else forward declarations don't work properly. Kinda
// dumb system, but have to live with it. clang-format seems to mess it up
/* Order: config, system, coretypes, target, tree, gimple-expr, diagnostic, opts, fold-const, 
 * gimplify, stor-layout, debug, convert, langhooks, langhooks-def, common-target */

#include "rust-parse.h"

// Language-dependent contents of a type. GTY() mark used for garbage collector.
struct GTY(()) lang_type {
    char dummy;
};

// Language-dependent contents of a decl.
struct GTY(()) lang_decl {
    char dummy;
};

// Language-dependent contents of an identifier.  This must include a tree_identifier.
struct GTY(()) lang_identifier {
    struct tree_identifier common;
};

// The resulting tree type.
union GTY((desc("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
  chain_next(
    "CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
    "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL"))) lang_tree_node {
    union tree_node GTY((tag("0"), desc("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY((tag("1"))) identifier;
};

// We don't use language_function.
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

    // mpfr_set_default_prec(128);
    // using_eh_for_cleanups();

    // rdot_init();
    return true;
}

// Parses a single file with filename filename.
static void grs_parse_file(const char* filename) {
    FILE* file = fopen(filename, "r");

    if (file == NULL) {
        fatal_error(UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

    // parse file here
    // create lexer
    Rust::Lexer lex(filename, file);
    Rust::Parser parser(lex);

    parser.parse_program();

    /* Rust::const_TokenPtr tok = lex.peek_token();
    // do shit until EOF
    while (true) {
        bool has_text = tok->get_id() == Rust::IDENTIFIER || tok->get_id() == Rust::INT_LITERAL
                        || tok->get_id() == Rust::FLOAT_LITERAL
                        || tok->get_id() == Rust::STRING_LITERAL;

        location_t loc = tok->get_locus();

        fprintf(stderr, "<id=%s%s, %s, line=%d, col=%d>\n", tok->token_id_to_str(),
          has_text ? (std::string(", text=") + tok->get_str()).c_str() : "", LOCATION_FILE(loc),
          LOCATION_LINE(loc), LOCATION_COLUMN(loc));

        if (tok->get_id() == Rust::END_OF_FILE)
            break;

        lex.skip_token();
        tok = lex.peek_token();
    }*/

    fclose(file);
}

/* Actual main entry point for front-end. Called by langhook to parse files.
 * May move to a different compilation unit if frontend gets too big. */
static void grs_parse_files(int num_files, const char** files) {
    for (int i = 0; i < num_files; i++) {
        grs_parse_file(files[i]);
    }
}

/* Main entry point for front-end, apparently. Finds input file names in global vars in_fnames and
 * num_in_fnames. From this, frontend can take over and do actual parsing and initial compilation.
 * This function must create a complete parse tree in a global var, and then return.
 *
 * Some consider this the "start of compilation". */
static void grs_langhook_parse_file(void) {
    fprintf(stderr, "Nothing happens yet \n");

    grs_parse_files(num_in_fnames, in_fnames);
}

static tree grs_langhook_type_for_mode(machine_mode mode, int unsignedp) {
    if (mode == TYPE_MODE(float_type_node))
        return float_type_node;

    if (mode == TYPE_MODE(double_type_node))
        return double_type_node;

    // don't know what this means but assume it has something to do with weird precisions
    if (mode == TYPE_MODE(intQI_type_node))
        return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
    if (mode == TYPE_MODE(intHI_type_node))
        return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
    if (mode == TYPE_MODE(intSI_type_node))
        return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
    if (mode == TYPE_MODE(intDI_type_node))
        return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
    if (mode == TYPE_MODE(intTI_type_node))
        return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

    if (mode == TYPE_MODE(integer_type_node))
        return unsignedp ? unsigned_type_node : integer_type_node;

    if (mode == TYPE_MODE(long_integer_type_node))
        return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (mode == TYPE_MODE(long_long_integer_type_node))
        return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

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

static tree grs_langhook_type_for_size(
  unsigned int bits ATTRIBUTE_UNUSED, int unsignedp ATTRIBUTE_UNUSED) {
    gcc_unreachable();
    return NULL_TREE;
}

// Record a builtin function.  We just ignore builtin functions.
static tree grs_langhook_builtin_function(tree decl ATTRIBUTE_UNUSED) {
    return decl;
}

static bool grs_langhook_global_bindings_p(void) {
    // return current_function_decl == NULL_TREE;
    //gcc_unreachable();
    //return true;
    return false;
}

static tree grs_langhook_pushdecl(tree decl ATTRIBUTE_UNUSED) {
    gcc_unreachable();
    return NULL;
}

static tree grs_langhook_getdecls(void) {
    // gcc_unreachable();
    return NULL;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */
/* tree convert(tree type, tree expr) { // not implemented yet - seems to be needed for compilation
    return NULL;
}*/
// implemented in rust-misc-convert.cc

/* The language hooks data structure. This is the main interface between the GCC front-end
 * and the GCC middle-end/back-end. A list of language hooks could be found in
 * <gcc>/langhooks.h
 */
#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
//#undef LANG_HOOKS_OPTION_LANG_MASK
//#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
//#undef LANG_HOOKS_HANDLE_OPTION
//#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
//#undef LANG_HOOKS_WRITE_GLOBALS
//#undef LANG_HOOKS_GIMPLIFY_EXPR
//#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME "GNU Rust"
#define LANG_HOOKS_INIT grs_langhook_init
//#define LANG_HOOKS_OPTION_LANG_MASK grs_langhook_option_lang_mask
//#define LANG_HOOKS_INIT_OPTIONS_STRUCT grs_langhook_init_options_struct
//#define LANG_HOOKS_HANDLE_OPTION grs_langhook_handle_option
//#define LANG_HOOKS_POST_OPTIONS grs_langhook_post_options
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
//#define LANG_HOOKS_WRITE_GLOBALS grs_langhook_write_globals
//#define LANG_HOOKS_GIMPLIFY_EXPR grs_langhook_gimplify_expr
//#define LANG_HOOKS_EH_PERSONALITY grs_langhook_eh_personality

// Expands all LANG_HOOKS_x of GCC
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

// These are for GCC's garbage collector to work properly or something
#include "gt-rust-rust-lang.h"
#include "gtype-rust.h"