/* Process declarations and variables for GNU CHILL compiler.
   Copyright (C) 1992, 93, 1994, 1998, 1999 Free Software Foundation, Inc. 
   
   This file is part of GNU CC.
   
   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Process declarations and symbol lookup for CHILL front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* NOTES on Chill name resolution
   
   Chill allows one to refer to an identifier that is declared later in
   the same Group.  Hence, a single pass over the code (as in C) is
   insufficient.
   
   This implementation uses two complete passes over the source code,
   plus some extra passes over internal data structures.
   
   Loosely, during pass 1, a 'scope' object is created for each Chill
   reach.  Each scope object contains a list of 'decl' objects,
   one for each 'defining occurrence' in the reach.  (This list
   is in the 'remembered_decls' field of each scope.)
   The scopes and their decls are replayed in pass 2:  As each reach
   is entered, the decls saved from pass 1 are made visible.
   
   There are some exceptions.  Declarations that cannot be referenced
   before their declaration (i.e. whose defining occurrence precede
   their reach), can be deferred to pass 2.  These include formal
   parameter declarations, and names defined in a DO action.
   
   During pass 2, as each scope is entered, we must make visible all
   the declarations defined in the scope, before we generate any code.
   We must also simplify the declarations from pass 1:  For example
   a VAR_DECL may have a array type whose bounds are expressions;
   these need to be folded.  But of course the expressions may contain
   identifiers that may be defined later in the scope - or even in
   a different module.
   
   The "satisfy" process has two main phases:
   
   1: Binding. Each identifier *referenced* in a declaration (i.e. in
   a mode or the RHS of a synonum declaration) must be bound to its
   defining occurrence.  This may need to be linking via
   grants and/or seizes (which are represented by ALIAS_DECLs).
   A further complication is handling implied name strings.
   
   2: Layout. Each CONST_DECL or TYPE_DECL *referenced* in a declaration
   must than be replaced by its value (or type).  Constants must be
   folded.  Types and declarstions must be laid out.  DECL_RTL must be set.
   While doing this, we must watch out for circular dependencies.
   
   If a scope contains nested modulions, then the Binding phase must be
   done for each nested module (recursively) before the Layout phase
   can start for that scope.  As an example of why this is needed, consider:
   
   M1: MODULE
     DCL a ARRAY [1:y] int; -- This should have 7 elements.
     SYN x = 5;
     SEIZE y;
   END M1;
   M2: MODULE
     SYN x = 2;
     SYN y = x + 5;
     GRANT y;
   END M2;

   Here, the 'x' in "x + 5" must be Bound to the 'x' in module M2.
   This must be done before we can Layout a.
   The reason this is an issue is that we do *not* have a lookup
   (or hash) table per scope (or module).  Instead we have a single
   global table we we keep adding and removing bindings from.
   (This is both for speed, and because of gcc history.)

   Note that a SEIZE generates a declaration in the current scope,
   linked to something in the surrounding scope.  Determining (binding)
   the link must be done in pass 2.  On the other hand, a GRANT
   generates a declaration in the surrounding scope, linked to
   something in the current scope.  This linkage is Bound in pass 1.

   The sequence for the above example is:
   - Enter the declarations of M1 (i.e. {a, x, y}) into the hash table.
   - For each of {a, x, y}, examine dependent expression (the
     rhs of x, the bounds of a), and Bind any identifiers to
     the current declarations (as found in the hash table).  Specifically,
     the 'y' in the array bounds of 'a' is bound to the 'y' declared by
     the SEIZE declaration.  Also, 'y' is Bound to the implicit
     declaration in the global scope (generated from the GRANT in M2).
   - Remove the bindings for M1 (i.e. {a, x, y}) from the hash table.
   - Enter the declarations of M2 (i.e. {x, y}) into the hash table.
   - For each of {x, y} examine the dependent expressions (the rhs of
     x and y), and Bind any identifiers to their current declarartions
     (in this case the 'x' in "x + 5" is bound to the 'x' that is 2.
   - Remove the bindings for M2 (i.e. {x, y}) from the hash table.
   - Perform Layout for M1:  This requires the size of a, which
     requires the value of y.  The 'y'  is Bound to the implicit
     declaration in the global scope, which is Bound to the declaration
     of y in M2.  We now require the value of this 'y', which is "x + 5"
     where x is bound to the x in M2 (thanks to our previous Binding
     phase).  So we get that the value of y is 7.
   - Perform layout of M2.  This implies calculating (constant folding)
   the value of y - but we already did that, so we're done.   

   An example illustating the problem with implied names:

   M1: MODULE
     SEIZE y;
     use(e);  -- e is implied by y.
   END M1;
   M2: MODULE
     GRANT y;
     SYNMODE y = x;
     SEIZE x;
   END M2;
   M3: MODULE
     GRANT x;
     SYNMODE x = SET (e);
   END M3;

   This implies that determining the implied name e in M1
   must be done after Binding of y to x in M2.

   Yet another nasty:
   M1: MODULE
     SEIZE v;
     DCL a ARRAY(v:v) int;
   END M1;
   M2: MODULE
     GRANT v;
     SEIZE x;
     SYN v x = e;
   END M2;
   M3: MODULE
     GRANT x;
     SYNMODE x = SET(e);
   END M3;

   This one implies that determining the implied name e in M2,
   must be done before Layout of a in M1.

   These two examples togother indicate the determining implieed
   names requries yet another phase.
   - Bind strong names in M1.
   - Bind strong names in M2.
   - Bind strong names in M3.
   - Determine weak names implied by SEIZEs in M1.
   - Bind the weak names in M1.
   - Determine weak names implied by SEIZEs in M2.
   - Bind the weak names in M2.
   - Determine weak names implied by SEIZEs in M3.
   - Bind the weak names in M3.
   - Layout M1.
   - Layout M2.
   - Layout M3.

   We must bind the strong names in every module before we can determine
   weak names in any module (because of seized/granted synmode/newmodes).
   We must bind the weak names in every module before we can do Layout
   in any module.

   Sigh.

   */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "ch-tree.h"
#include "lex.h"
#include "obstack.h"
#include "input.h"
#include "rtl.h"
#include "toplev.h"

#define IS_UNKNOWN_TYPE(type) (TYPE_SIZE(type)==0)
#define BUILTIN_NESTING_LEVEL (-1)

/* For backward compatibility, we define Chill INT to be the same
   as SHORT (i.e. 16 bits), at least if C INT is the same as LONG.
   This is a lose. */
#define CHILL_INT_IS_SHORT (INT_TYPE_SIZE==LONG_TYPE_SIZE)

extern int  ignore_case;
extern tree process_type;
extern struct obstack *saveable_obstack;
extern tree signal_code;
extern int special_UC;

static tree get_next_decl             PROTO((void));
static tree lookup_name_for_seizing   PROTO((tree));
#if 0
static tree lookup_name_current_level PROTO((tree));
#endif
static void save_decl                 PROTO((tree));

extern struct obstack permanent_obstack;
extern int in_pseudo_module;

struct module *current_module = NULL;
struct module *first_module = NULL;
struct module **next_module = &first_module;

extern int  in_pseudo_module;

int module_number = 0;

/* This is only used internally (by signed_type). */

tree signed_boolean_type_node;

tree global_function_decl = NULL_TREE;

/* This is a temportary used by RESULT to store its value.
   Note we cannot directly use DECL_RESULT for two reasons:
   a) If DECL_RESULT is a register, it may get clobbered by a
   subsequent function call; and
   b) if the function returns a struct, we might (visibly) modify the
   destination before we're supposed to. */
tree chill_result_decl;

int result_never_set;

/* forward declarations */
static void pushdecllist                     PROTO((tree, int));
static int  init_nonvalue_struct             PROTO((tree));
static int  init_nonvalue_array              PROTO((tree));

int current_nesting_level = BUILTIN_NESTING_LEVEL;
int current_module_nesting_level = 0;

/* Lots of declarations copied from c-decl.c. */
/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#if 0
/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
    FUNCDEF,			/* Function definition */
    PARM,			/* Declaration of parm before function body */
    FIELD,			/* Declaration inside struct or union */
    BITFIELD,			/* Likewise but with specified width */
    TYPENAME};			/* Typename (inside cast or sizeof)  */
#endif

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

#ifndef WCHAR_TYPE
#define WCHAR_TYPE "int"
#endif

/* a node which has tree code ERROR_MARK, and whose type is itself.
   All erroneous expressions are replaced with this node.  All functions
   that accept nodes as arguments should avoid generating error messages
   if this node is one of the arguments, since it is undesirable to get
   multiple error messages from one error in the input.  */

tree error_mark_node;

/* INTEGER_TYPE and REAL_TYPE nodes for the standard data types */

tree short_integer_type_node;
tree integer_type_node;
tree long_integer_type_node;
tree long_long_integer_type_node;

tree short_unsigned_type_node;
tree unsigned_type_node;
tree long_unsigned_type_node;
tree long_long_unsigned_type_node;

tree ptrdiff_type_node;

tree unsigned_char_type_node;
tree signed_char_type_node;
tree char_type_node;
tree wchar_type_node;
tree signed_wchar_type_node;
tree unsigned_wchar_type_node;

tree float_type_node;
tree double_type_node;
tree long_double_type_node;

tree complex_integer_type_node;
tree complex_float_type_node;
tree complex_double_type_node;
tree complex_long_double_type_node;

tree intQI_type_node;
tree intHI_type_node;
tree intSI_type_node;
tree intDI_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
tree intTI_type_node;
#endif

tree unsigned_intQI_type_node;
tree unsigned_intHI_type_node;
tree unsigned_intSI_type_node;
tree unsigned_intDI_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
tree unsigned_intTI_type_node;
#endif

/* a VOID_TYPE node.  */

tree void_type_node;
tree void_list_node;

/* Nodes for types `void *' and `const void *'.  */
tree ptr_type_node, const_ptr_type_node;

/* type of initializer structure, which points to
   a module's module-level code, and to the next
   such structure. */
tree initializer_type;

/* type of a CHILL predefined value builtin routine */
tree chill_predefined_function_type;

/* type `int ()' -- used for implicit declaration of functions.  */

tree default_function_type;

#if 0
/* function types `double (double)' and `double (double, double)', etc.  */

tree double_ftype_double, double_ftype_double_double;
tree int_ftype_int, long_ftype_long;

/* Function type `void (void *, void *, int)' and similar ones */

tree void_ftype_ptr_ptr_int, int_ftype_ptr_ptr_int, void_ftype_ptr_int_int;

/* Function type `char *(char *, char *)' and similar ones */
tree string_ftype_ptr_ptr, int_ftype_string_string;

/* Function type `int (const void *, const void *, size_t)' */
tree int_ftype_cptr_cptr_sizet;
#endif

char **boolean_code_name;

/* Two expressions that are constants with value zero.
   The first is of type `int', the second of type `void *'.  */

tree integer_zero_node;
tree null_pointer_node;

/* A node for the integer constant 1.  */
tree integer_one_node;

/* A node for the integer constant -1.  */
tree integer_minus_one_node;

/* Nodes for boolean constants TRUE and FALSE. */
tree boolean_true_node, boolean_false_node;

tree string_one_type_node;  /* The type of CHARS(1). */
tree bitstring_one_type_node;  /* The type of BOOLS(1). */
tree bit_zero_node; /* B'0' */
tree bit_one_node; /* B'1' */

/* Nonzero if we have seen an invalid cross reference
   to a struct, union, or enum, but not yet printed the message.  */

tree pending_invalid_xref;
/* File and line to appear in the eventual error message.  */
char *pending_invalid_xref_file;
int pending_invalid_xref_line;

/* After parsing the declarator that starts a function definition,
   `start_function' puts here the list of parameter names or chain of decls.
   `store_parm_decls' finds it here.  */

static tree current_function_parms;

/* Nonzero when store_parm_decls is called indicates a varargs function.
   Value not meaningful after store_parm_decls.  */

static int c_function_varargs;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* These are irrelevant for Chill, but are referenced from from c-typeck.c. */
int warn_format;
int warn_traditional;
int warn_bad_function_cast;

/* Identifiers that hold VAR_LENGTH and VAR_DATA.  */
tree var_length_id, var_data_id;

tree case_else_node;

/* For each binding contour we allocate a scope structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *  2) one for each compound statement,
 *     to record its declarations.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 */

/* To communicate between pass 1 and 2, we maintain a list of "scopes".
   Each scope corrresponds to a nested source scope/block that contain 
   that can contain declarations.  The TREE_VALUE of the scope points
   to the list of declarations declared in that scope.
   The TREE_PURPOSE of the scope points to the surrounding scope.
   (We may need to handle nested modules later.  FIXME)
   The TREE_CHAIN field contains a list of scope as they are seen
   in chronological order.  (Reverse order during first pass,
   but it is reverse before pass 2.) */

struct scope
{
  /* The enclosing scope. */
  struct scope *enclosing;
  
  /* The next scope, in chronlogical order. */
  struct scope *next;
  
  /* A chain of DECLs constructed using save_decl during pass 1. */
  tree remembered_decls;
  
  /* A chain of _DECL nodes for all variables, constants, functions,
     and typedef types belong to this scope. */
  tree decls;
  
  /* List of declarations that have been granted into this scope. */
  tree granted_decls;

  /* List of implied (weak) names. */
  tree weak_decls;
  
  /* For each level, a list of shadowed outer-level local definitions
     to be restored when this level is popped.
     Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
     whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
  tree shadowed;
  
  /* For each level (except not the global one),
     a chain of BLOCK nodes for all the levels
     that were entered and exited one level down.  */
  tree blocks;
  
  /* The BLOCK node for this level, if one has been preallocated.
     If 0, the BLOCK is allocated (if needed) when the level is popped.  */
  tree this_block;
  
  /* The binding level which this one is contained in (inherits from).  */
  struct scope *level_chain;
  
  /* Nonzero for a level that corresponds to a module. */
  char module_flag;
  
  /* Zero means called from backend code. */
  char two_pass;
  
  /* The modules that are directly enclosed by this scope
     are chained together. */
  struct scope* first_child_module;
  struct scope** tail_child_module;
  struct scope* next_sibling_module;
};

/* The outermost binding level, for pre-defined (builtin) names. */

static struct scope builtin_scope = {
  NULL, NULL, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE,
  NULL_TREE, NULL_TREE, NULL, 0, 0, NULL, NULL, NULL};

struct scope *global_scope;

/* The binding level currently in effect.  */

static struct scope *current_scope = &builtin_scope;

/* The most recently seen scope. */
struct scope *last_scope = &builtin_scope;

/* Binding level structures are initialized by copying this one.  */

static struct scope clear_scope = {
  NULL, NULL, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE,
  NULL_TREE, NULL_TREE, NULL, 0, 0, NULL, NULL, NULL};

/* Chain of decls accessible through IDENTIFIER_OUTER_VALUE.
   Decls with the same DECL_NAME are adjacent in the chain. */

static tree outer_decls = NULL_TREE;

/* C-specific option variables.  */

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

int flag_cond_mismatch;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double;

/* Nonzero means don't recognize the keyword `asm'.  */

int flag_no_asm;

/* Nonzero means don't recognize any builtin functions.  */

int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

int flag_no_nonansi_builtin;

/* Nonzero means do some things the same way PCC does.  */

int flag_traditional;

/* Nonzero means to allow single precision math even if we're generally
   being traditional. */
int flag_allow_single_precision = 0;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */

int flag_signed_bitfields = 1;
int explicit_flag_signed_bitfields = 0;

/* Nonzero means warn about implicit declarations.  */

int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

int warn_write_strings;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

int warn_cast_qual;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

int warn_pointer_arith;

/* Nonzero means warn for non-prototype function decls
   or non-prototyped defs without previous prototype.  */

int warn_strict_prototypes;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */

int warn_missing_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

int warn_redundant_decls = 0;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */

int warn_nested_externs = 0;

/* Warn about a subscript that has type char.  */

int warn_char_subscripts = 0;

/* Warn if a type conversion is done that might have confusing results.  */

int warn_conversion;

/* Warn if adding () is suggested.  */

int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */

int warn_missing_braces;

/* Define the special tree codes that we use.  */

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See ch-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,
  
  char chill_tree_code_type[] = {
    'x',
#include "ch-tree.def"
  };
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,
  
int chill_tree_code_length[] = {
    0,
#include "ch-tree.def"
  };
#undef DEFTREECODE


/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,
  
char *chill_tree_code_name[] = {
    "@@dummy",
#include "ch-tree.def"
  };
#undef DEFTREECODE

/* Nonzero means `$' can be in an identifier.
   See cccp.c for reasons why this breaks some obscure ANSI C programs.  */

#ifndef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 0
#endif
int dollars_in_ident = DOLLARS_IN_IDENTIFIERS > 1;

/* An identifier that is used internally to indicate
   an "ALL" prefix for granting or seizing.
   We use "*" rather than the external name "ALL", partly for convenience,
   and partly to avoid case senstivity problems. */

tree ALL_POSTFIX;

void
allocate_lang_decl (t)
     tree t ATTRIBUTE_UNUSED;
{
  /* Nothing needed */
}

void
copy_lang_decl (node)
     tree node ATTRIBUTE_UNUSED;
{
  /* Nothing needed */
}

tree
build_lang_decl (code, name, type)
     enum chill_tree_code code;
     tree name;
     tree type;
{
  return build_decl (code, name, type);
}

/* Decode the string P as a language-specific option for C.
   Return the number of strings consumed for a valid option.
   Return 0 for an invalid option.  */

int
c_decode_option (argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv;
{
  char *p = argv[0];
  if (!strcmp (p, "-ftraditional") || !strcmp (p, "-traditional"))
    {
      flag_traditional = 1;
      flag_writable_strings = 1;
#if DOLLARS_IN_IDENTIFIERS > 0
      dollars_in_ident = 1;
#endif
    }
  else if (!strcmp (p, "-fnotraditional") || !strcmp (p, "-fno-traditional"))
    {
      flag_traditional = 0;
      flag_writable_strings = 0;
      dollars_in_ident = DOLLARS_IN_IDENTIFIERS > 1;
    }
  else if (!strcmp (p, "-fsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-funsigned-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-signed-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-unsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-fsigned-bitfields")
	   || !strcmp (p, "-fno-unsigned-bitfields"))
    {
      flag_signed_bitfields = 1;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-funsigned-bitfields")
	   || !strcmp (p, "-fno-signed-bitfields"))
    {
      flag_signed_bitfields = 0;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-fshort-enums"))
    flag_short_enums = 1;
  else if (!strcmp (p, "-fno-short-enums"))
    flag_short_enums = 0;
  else if (!strcmp (p, "-fcond-mismatch"))
    flag_cond_mismatch = 1;
  else if (!strcmp (p, "-fno-cond-mismatch"))
    flag_cond_mismatch = 0;
  else if (!strcmp (p, "-fshort-double"))
    flag_short_double = 1;
  else if (!strcmp (p, "-fno-short-double"))
    flag_short_double = 0;
  else if (!strcmp (p, "-fasm"))
    flag_no_asm = 0;
  else if (!strcmp (p, "-fno-asm"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-fbuiltin"))
    flag_no_builtin = 0;
  else if (!strcmp (p, "-fno-builtin"))
    flag_no_builtin = 1;
  else if (!strcmp (p, "-ansi"))
    flag_no_asm = 1, flag_no_nonansi_builtin = 1, dollars_in_ident = 0;
  else if (!strcmp (p, "-Wimplicit"))
    warn_implicit = 1;
  else if (!strcmp (p, "-Wno-implicit"))
    warn_implicit = 0;
  else if (!strcmp (p, "-Wwrite-strings"))
    warn_write_strings = 1;
  else if (!strcmp (p, "-Wno-write-strings"))
    warn_write_strings = 0;
  else if (!strcmp (p, "-Wcast-qual"))
    warn_cast_qual = 1;
  else if (!strcmp (p, "-Wno-cast-qual"))
    warn_cast_qual = 0;
  else if (!strcmp (p, "-Wpointer-arith"))
    warn_pointer_arith = 1;
  else if (!strcmp (p, "-Wno-pointer-arith"))
    warn_pointer_arith = 0;
  else if (!strcmp (p, "-Wstrict-prototypes"))
    warn_strict_prototypes = 1;
  else if (!strcmp (p, "-Wno-strict-prototypes"))
    warn_strict_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-prototypes"))
    warn_missing_prototypes = 1;
  else if (!strcmp (p, "-Wno-missing-prototypes"))
    warn_missing_prototypes = 0;
  else if (!strcmp (p, "-Wredundant-decls"))
    warn_redundant_decls = 1;
  else if (!strcmp (p, "-Wno-redundant-decls"))
    warn_redundant_decls = 0;
  else if (!strcmp (p, "-Wnested-externs"))
    warn_nested_externs = 1;
  else if (!strcmp (p, "-Wno-nested-externs"))
    warn_nested_externs = 0;
  else if (!strcmp (p, "-Wchar-subscripts"))
    warn_char_subscripts = 1;
  else if (!strcmp (p, "-Wno-char-subscripts"))
    warn_char_subscripts = 0;
  else if (!strcmp (p, "-Wconversion"))
    warn_conversion = 1;
  else if (!strcmp (p, "-Wno-conversion"))
    warn_conversion = 0;
  else if (!strcmp (p, "-Wparentheses"))
    warn_parentheses = 1;
  else if (!strcmp (p, "-Wno-parentheses"))
    warn_parentheses = 0;
  else if (!strcmp (p, "-Wreturn-type"))
    warn_return_type = 1;
  else if (!strcmp (p, "-Wno-return-type"))
    warn_return_type = 0;
  else if (!strcmp (p, "-Wcomment"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-comment"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wcomments"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-comments"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wtrigraphs"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-trigraphs"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wimport"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-import"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wmissing-braces"))
    warn_missing_braces = 1;
  else if (!strcmp (p, "-Wno-missing-braces"))
    warn_missing_braces = 0;
  else if (!strcmp (p, "-Wall"))
    {
      extra_warnings = 1;
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = 2;
      warn_implicit = 1;
      warn_return_type = 1;
      warn_unused = 1;
      warn_char_subscripts = 1;
      warn_parentheses = 1;
      warn_missing_braces = 1;
    }
  else
    return 0;
  
  return 1;
}

/* Hooks for print_node.  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int  indent;
{
  indent_to (file, indent + 3);
  fputs ("nesting_level ", file);
  fprintf (file, HOST_WIDE_INT_PRINT_DEC, DECL_NESTING_LEVEL (node));
  fputs (" ", file);
  if (DECL_WEAK_NAME (node))
    fprintf (file, "weak_name ");
  if (CH_DECL_SIGNAL (node))
    fprintf (file, "decl_signal ");
  print_node (file, "tasking_code",
	      (tree)DECL_TASKING_CODE_DECL (node), indent + 4);
}


void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int  indent;
{
  tree temp;

  indent_to (file, indent + 3);
  if (CH_IS_BUFFER_MODE (node))
    fprintf (file, "buffer_mode ");
  if (CH_IS_EVENT_MODE (node))
    fprintf (file, "event_mode ");

  if (CH_IS_EVENT_MODE (node) || CH_IS_BUFFER_MODE (node))
    {
      temp = max_queue_size (node);
      if (temp)
	print_node_brief (file, "qsize", temp, indent + 4);
    }
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int  indent;
{
  print_node (file, "local",       IDENTIFIER_LOCAL_VALUE (node),   indent +  4);
  print_node (file, "outer",       IDENTIFIER_OUTER_VALUE (node),   indent +  4);
  print_node (file, "implicit",    IDENTIFIER_IMPLICIT_DECL (node), indent + 4);
  print_node (file, "error locus", IDENTIFIER_ERROR_LOCUS (node),   indent + 4);
  print_node (file, "signal_dest", IDENTIFIER_SIGNAL_DEST (node),   indent + 4);
  indent_to  (file, indent + 3);
  if (IDENTIFIER_SIGNAL_DATA(node))
    fprintf (file, "signal_data ");
}

/* initialise non-value struct */

static int
init_nonvalue_struct (expr)
     tree expr;
{
  tree type = TREE_TYPE (expr);
  tree field;
  int res = 0;

  if (CH_IS_BUFFER_MODE (type))
    {
      expand_expr_stmt (
        build_chill_modify_expr (
          build_component_ref (expr, get_identifier ("__buffer_data")),
            null_pointer_node));
      return 1;
    }
  else if (CH_IS_EVENT_MODE (type))
    {
      expand_expr_stmt (
        build_chill_modify_expr (
          build_component_ref (expr, get_identifier ("__event_data")),
            null_pointer_node));
      return 1;
    }
  else if (CH_IS_ASSOCIATION_MODE (type))
    {
      expand_expr_stmt (
        build_chill_modify_expr (expr,
          chill_convert_for_assignment (type, association_init_value,
					"association")));
      return 1;
    }
  else if (CH_IS_ACCESS_MODE (type))
    {
      init_access_location (expr, type);
      return 1;
    }
  else if (CH_IS_TEXT_MODE (type))
    {
      init_text_location (expr, type);
      return 1;
    }

  for (field = TYPE_FIELDS (type); field != NULL_TREE; field = TREE_CHAIN (field))
    {
      type = TREE_TYPE (field);
      if (CH_TYPE_NONVALUE_P (type))
	{
	  tree exp = build_component_ref (expr, DECL_NAME (field));
	  if (TREE_CODE (type) == RECORD_TYPE)
	    res |= init_nonvalue_struct (exp);
	  else if (TREE_CODE (type) == ARRAY_TYPE)
	    res |= init_nonvalue_array (exp);
	}
    }
  return res;
}

/* initialize non-value array */
/* do it with DO FOR unique-id IN expr; ... OD; */
static int
init_nonvalue_array (expr)
     tree expr;
{
  tree tmpvar = get_unique_identifier ("NONVALINIT");
  tree type;
  int res = 0;

  push_loop_block ();
  build_loop_iterator (tmpvar, expr, NULL_TREE, NULL_TREE, 0, 1, 0);
  nonvalue_begin_loop_scope ();
  build_loop_start (NULL_TREE);
  tmpvar = lookup_name (tmpvar);
  type = TREE_TYPE (tmpvar);
  if (CH_TYPE_NONVALUE_P (type))
    {
      if (TREE_CODE (type) == RECORD_TYPE)
	res |= init_nonvalue_struct (tmpvar);
      else if (TREE_CODE (type) == ARRAY_TYPE)
	res |= init_nonvalue_array (tmpvar);
    }
  build_loop_end ();
  nonvalue_end_loop_scope ();
  pop_loop_block ();
  return res;
}

/* This excessive piece of code sets DECL_NESTING_LEVEL (DECL) to LEVEL. */

void
set_nesting_level (decl, level)
     tree decl;
     int level;
{
  static tree *small_ints = NULL;
  static int max_small_ints = 0;
  
  if (level < 0)
    decl->decl.vindex = NULL_TREE;
  else
    {
      if (level >= max_small_ints)
	{
	  int new_max = level + 20;
	  if (small_ints == NULL)
	    small_ints = (tree*)xmalloc (new_max * sizeof(tree));
	  else
	    small_ints = (tree*)xrealloc (small_ints, new_max * sizeof(tree));
	  while (max_small_ints < new_max)
	    small_ints[max_small_ints++] = NULL_TREE;
	}
      if (small_ints[level] == NULL_TREE)
	{
	  push_obstacks (&permanent_obstack, &permanent_obstack);
	  small_ints[level] = build_int_2 (level, 0);
	  pop_obstacks ();
	}
      /* set DECL_NESTING_LEVEL */
      decl->decl.vindex = small_ints[level];
    }
}

/* OPT_EXTERNAL is non-zero when the declaration is at module level.
 * OPT_EXTERNAL == 2 means implicitly grant it.
 */
void
do_decls (names, type, opt_static, lifetime_bound, opt_init, opt_external)
     tree names;
     tree type;
     int  opt_static;
     int  lifetime_bound;
     tree opt_init;
     int  opt_external;
{
  if (names == NULL_TREE || TREE_CODE (names) == TREE_LIST)
    {
      for (; names != NULL_TREE; names = TREE_CHAIN (names))
	do_decl (TREE_VALUE (names), type, opt_static, lifetime_bound,
		 opt_init, opt_external);
    }
  else if (TREE_CODE (names) != ERROR_MARK)
    do_decl (names, type, opt_static, lifetime_bound, opt_init, opt_external);
}

tree
do_decl (name, type, is_static, lifetime_bound, opt_init, opt_external)
     tree name, type;
     int  is_static;
     int  lifetime_bound;
     tree opt_init;
     int  opt_external;
{
  tree decl;

  if (current_function_decl == global_function_decl
      && ! lifetime_bound /*&& opt_init != NULL_TREE*/)
    seen_action = 1;

  if (pass < 2)
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      decl = make_node (VAR_DECL);
      DECL_NAME (decl) = name;
      TREE_TYPE (decl) = type;
      DECL_ASSEMBLER_NAME (decl) = name;

      /* Try to put things in common when possible.
         Tasking variables must go into common.  */
      DECL_COMMON (decl) = 1;
      DECL_EXTERNAL (decl) = opt_external > 0;
      TREE_PUBLIC (decl)   = opt_external > 0;
      TREE_STATIC (decl)   = is_static;

      if (pass == 0)
	{
	  /* We have to set this here, since we build the decl w/o
	     calling `build_decl'.  */
	  DECL_INITIAL (decl) = opt_init;
	  pushdecl (decl);
	  finish_decl (decl);
	}
      else
	{
	  save_decl (decl);
	  pop_obstacks ();
	}
      DECL_INITIAL (decl) = opt_init;
      if (opt_external > 1 || in_pseudo_module)
	push_granted (DECL_NAME (decl), decl);
    }
  else /* pass == 2 */
    {
      tree temp = NULL_TREE;
      int init_it = 0;

      decl = get_next_decl ();
      
      if (name != DECL_NAME (decl))
	abort ();
      
      type = TREE_TYPE (decl);
      
      push_obstacks_nochange ();
      if (TYPE_READONLY_PROPERTY (type))
	{
	  if (CH_TYPE_NONVALUE_P (type))
	    {
	      error_with_decl (decl, "`%s' must not be declared readonly");
	      opt_init = NULL_TREE; /* prevent subsequent errors */
	    }
	  else if (opt_init == NULL_TREE && !opt_external)
	    error("declaration of readonly variable without initialization");
	}
      TREE_READONLY (decl) = TYPE_READONLY (type);
      
      if (!opt_init && chill_varying_type_p (type))
	{
	  tree fixed_part_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));
	  if (fixed_part_type != NULL_TREE && TREE_CODE (fixed_part_type) != ERROR_MARK)
	    {
	      if (CH_CHARS_TYPE_P (fixed_part_type))
		opt_init = build_chill_string (0, "");
	      else
		opt_init = build_nt (CONSTRUCTOR, NULL_TREE, NULL_TREE);
	      lifetime_bound = 1;
	    }
	}

      if (opt_init)
	{
	  if (CH_TYPE_NONVALUE_P (type))
	    {
	      error_with_decl (decl,
			       "no initialisation allowed for `%s'");
	      temp = NULL_TREE;
	    }
	  else if (TREE_CODE (type) == REFERENCE_TYPE)
	    { /* A loc-identity declaration */
	      if (! CH_LOCATION_P (opt_init))
		{
		  error_with_decl (decl,
			"value for loc-identity `%s' is not a location");
		  temp = NULL_TREE;
		}
	      else if (! CH_READ_COMPATIBLE (TREE_TYPE (type),
					     TREE_TYPE (opt_init)))
		{
		  error_with_decl (decl,
				   "location for `%s' not read-compatible");
		  temp = NULL_TREE;
		}
	      else
		temp = convert (type, opt_init);
	    }
	  else
	    { /* Normal location declaration */
	      char place[80];
	      sprintf (place, "`%.60s' initializer",
		       IDENTIFIER_POINTER (DECL_NAME (decl)));
	      temp = chill_convert_for_assignment (type, opt_init, place);
	    }
	}
      else if (CH_TYPE_NONVALUE_P (type))
	{
	  temp = NULL_TREE;
	  init_it = 1;
	}
      DECL_INITIAL (decl) = NULL_TREE;

      if (temp != NULL_TREE && TREE_CODE (temp) != ERROR_MARK)
        {
	  /* The same for stack variables (assuming no nested modules). */
	  if (lifetime_bound || !is_static)
	    {
	      if (is_static && ! TREE_CONSTANT (temp))
		error_with_decl (decl, "nonconstant initializer for `%s'");
	      else
		DECL_INITIAL (decl) = temp;
	    }
        }
      finish_decl (decl);
      /* Initialize the variable unless initialized statically. */
      if ((!is_static || ! lifetime_bound) &&
	  temp != NULL_TREE && TREE_CODE (temp) != ERROR_MARK)
	{
	  int was_used = TREE_USED (decl);
	  emit_line_note (input_filename, lineno);
	  expand_expr_stmt (build_chill_modify_expr (decl, temp));
	  /* Don't let the initialization count as "using" the variable.  */
	  TREE_USED (decl) = was_used;
	  if (current_function_decl == global_function_decl)
	    build_constructor = 1;
	}
      else if (init_it && TREE_CODE (type) != ERROR_MARK)
	{
	  /* Initialize variables with non-value type */
	  int was_used = TREE_USED (decl);
	  int something_initialised = 0;

	  emit_line_note (input_filename, lineno);
	  if (TREE_CODE (type) == RECORD_TYPE)
	    something_initialised = init_nonvalue_struct (decl);
	  else if (TREE_CODE (type) == ARRAY_TYPE)
	    something_initialised = init_nonvalue_array (decl);
	  if (! something_initialised)
	    {
	      error ("do_decl: internal error: don't know what to initialize");
	      abort ();
	    }
	  /* Don't let the initialization count as "using" the variable.  */
	  TREE_USED (decl) = was_used;
	  if (current_function_decl == global_function_decl)
	    build_constructor = 1;
	}
    }
  return decl;
}

/*
 * ARGTYPES is a tree_list of formal argument types.  TREE_VALUE
 * is the type tree for each argument, while the attribute is in
 * TREE_PURPOSE.
 */
tree
build_chill_function_type (return_type, argtypes, exceptions, recurse_p)
     tree return_type, argtypes, exceptions, recurse_p;
{
  tree ftype, arg;

  if (exceptions != NULL_TREE)
    {
      /* if we have exceptions we add 2 arguments, callers filename
	 and linenumber. These arguments will be added automatically
	 when calling a function which may raise exceptions. */
      argtypes = chainon (argtypes,
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_PTR]));
      argtypes = chainon (argtypes,
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]));
}

  /* Indicate the argument list is complete. */
  argtypes = chainon (argtypes,
		      build_tree_list (NULL_TREE, void_type_node));
  
  /* INOUT and OUT parameters must be a REFERENCE_TYPE since
     we'll be passing a temporary's address at call time. */
  for (arg = argtypes; arg; arg = TREE_CHAIN (arg))
    if (TREE_PURPOSE (arg) == ridpointers[(int) RID_LOC]
	|| TREE_PURPOSE (arg) == ridpointers[(int) RID_OUT]
	|| TREE_PURPOSE (arg) == ridpointers[(int) RID_INOUT]
	)
      TREE_VALUE (arg) = 
	build_chill_reference_type (TREE_VALUE (arg));
  
  /* Cannot use build_function_type, because if does hash-canonlicalization. */
  ftype = make_node (FUNCTION_TYPE);
  TREE_TYPE (ftype) = return_type ? return_type : void_type_node ;
  TYPE_ARG_TYPES (ftype) = argtypes;
  
  if (exceptions)
    ftype = build_exception_variant (ftype, exceptions);
  
  if (recurse_p)
    sorry ("RECURSIVE PROCs");
  
  return ftype;
}

/*
 * ARGTYPES is a tree_list of formal argument types.
 */
tree
push_extern_function (name, typespec, argtypes, exceptions, granting)
  tree name, typespec, argtypes, exceptions;
  int granting ATTRIBUTE_UNUSED;/*If 0 do pushdecl(); if 1 do push_granted()*/
{
  tree ftype, fndecl;
  
  push_obstacks_nochange ();
  end_temporary_allocation ();
  
  if (pass < 2)
    {
      ftype = build_chill_function_type (typespec, argtypes,
					 exceptions, NULL_TREE);
      
      fndecl = build_decl (FUNCTION_DECL, name, ftype);
      
      DECL_EXTERNAL(fndecl) = 1;
      TREE_STATIC (fndecl) = 1;
      TREE_PUBLIC (fndecl) = 1;
      if (pass == 0)
	{
	  pushdecl (fndecl);
	  finish_decl (fndecl);
	}
      else
	{
	  save_decl (fndecl);
	  pop_obstacks ();
	}
      make_function_rtl (fndecl);
    }
  else
    {
      fndecl = get_next_decl (); 
      finish_decl (fndecl);
    }
#if 0
  
  if (granting)
    push_granted (name, decl);
  else
    pushdecl(decl);
#endif
  return fndecl;
}



void
push_extern_process (name, argtypes, exceptions, granting)
     tree name, argtypes, exceptions;
     int  granting;
{
  tree decl, func, arglist;
  
  push_obstacks_nochange ();
  end_temporary_allocation ();
  
  if (pass < 2)
    {
      tree proc_struct = make_process_struct (name, argtypes);
      arglist = (argtypes == NULL_TREE) ? NULL_TREE :
	tree_cons (NULL_TREE,
		   build_chill_pointer_type (proc_struct), NULL_TREE);
    }
  else
    arglist = NULL_TREE;

  func = push_extern_function (name, NULL_TREE, arglist,
			       exceptions, granting);

  /* declare the code variable */
  decl = generate_tasking_code_variable (name, &process_type, 1);
  CH_DECL_PROCESS (func) = 1;
  /* remember the code variable in the function decl */
  DECL_TASKING_CODE_DECL (func) = (struct lang_decl *)decl;

  add_taskstuff_to_list (decl, "_TT_Process", NULL_TREE, func, NULL_TREE);
}

void
push_extern_signal (signame, sigmodelist, optsigdest)
     tree signame, sigmodelist, optsigdest;
{
  tree decl, sigtype;

  push_obstacks_nochange ();
  end_temporary_allocation ();
  
  sigtype = 
    build_signal_struct_type (signame, sigmodelist, optsigdest);
  
  /* declare the code variable outside the process */
  decl = generate_tasking_code_variable (signame, &signal_code, 1);
  add_taskstuff_to_list (decl, "_TT_Signal", NULL_TREE, sigtype, NULL_TREE);
}

void
print_mode (mode)
     tree mode;
{
  while (mode != NULL_TREE)
    {
      switch (TREE_CODE (mode))
	{
	case POINTER_TYPE:
	  printf (" REF ");
	  mode = TREE_TYPE (mode);
	  break;
	case INTEGER_TYPE:
	case REAL_TYPE:
	  printf (" %s ", IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (mode))));
	  mode = NULL_TREE;
	  break;
	case ARRAY_TYPE:
	  {
	    tree itype = TYPE_DOMAIN (mode);
	    if (CH_STRING_TYPE_P (mode))
	      {
		fputs (" STRING (", stdout);
		printf (HOST_WIDE_INT_PRINT_DEC,
			TREE_INT_CST_LOW (TYPE_MAX_VALUE (itype)));
		fputs (") OF ", stdout);
	      }
	    else
	      {
		fputs (" ARRAY (", stdout);
		printf (HOST_WIDE_INT_PRINT_DEC,
			TREE_INT_CST_LOW (TYPE_MIN_VALUE (itype)));
		fputs (":", stdout);
		printf (HOST_WIDE_INT_PRINT_DEC,
			TREE_INT_CST_LOW (TYPE_MAX_VALUE (itype)));
		fputs (") OF ", stdout);
	      }
	    mode = TREE_TYPE (mode);
	    break;
	  }
	case RECORD_TYPE:
	  {
	    tree fields = TYPE_FIELDS (mode);
	    printf (" RECORD (");
	    while (fields != NULL_TREE)
	      {
		printf (" %s:", IDENTIFIER_POINTER (DECL_NAME (fields)));
		print_mode (TREE_TYPE (fields));
		if (TREE_CHAIN (fields))
		  printf (",");
		fields = TREE_CHAIN (fields);
	      }
	    printf (")");
	    mode = NULL_TREE;
	    break;
	  }
	default:
	  abort ();
	}
    }
}

tree
chill_munge_params (nodes, type, attr)
     tree nodes, type, attr;
{
  tree node;
  if (pass == 1)
    {
      /* Convert the list of identifiers to a list of types. */
      for (node = nodes; node != NULL_TREE; node = TREE_CHAIN (node))
	{
	  TREE_VALUE (node) = type;  /* this was the identifier node */
	  TREE_PURPOSE (node) = attr;
	}
    }
  return nodes;
}

/* Push the declarations described by SYN_DEFS into the current scope.  */
void
push_syndecl (name, mode, value)
     tree name, mode, value;
{
  if (pass == 1)
    {
      tree decl = make_node (CONST_DECL);
      DECL_NAME (decl) = name;
      DECL_ASSEMBLER_NAME (decl) = name;
      TREE_TYPE (decl) = mode;
      DECL_INITIAL (decl) = value;
      TREE_READONLY (decl) = 1;
      save_decl (decl);
      if (in_pseudo_module)
	push_granted (DECL_NAME (decl), decl);
    }
  else /* pass == 2 */
    get_next_decl ();
}



/* Push the declarations described by (MODENAME,MODE) into the current scope.
   MAKE_NEWMODE is 1 for NEWMODE, 0 for SYNMODE, and
   -1 for internal use (in which case the mode does not need to be copied). */

tree
push_modedef (modename, mode, make_newmode)
     tree modename;
     tree mode;  /* ignored if pass==2. */
     int make_newmode;
{
  tree newdecl, newmode;
  
  if (pass == 1)
    {
      /* FIXME: need to check here for SYNMODE fred fred; */
      push_obstacks (&permanent_obstack, &permanent_obstack);

      newdecl = build_lang_decl (TYPE_DECL, modename, mode);

      if (make_newmode >= 0)
	{
	  newmode = make_node (LANG_TYPE);
	  TREE_TYPE (newmode) = mode;
	  TREE_TYPE (newdecl) = newmode;
	  TYPE_NAME (newmode) = newdecl;
	  if (make_newmode > 0)
	    CH_NOVELTY (newmode) = newdecl;
	}

      save_decl (newdecl);
      pop_obstacks ();
	  
    }
  else /* pass == 2 */
    {
      /* FIXME: need to check here for SYNMODE fred fred; */
      newdecl = get_next_decl ();
      if (DECL_NAME (newdecl) != modename)
	abort ();
      if (TREE_CODE (TREE_TYPE (newdecl)) != ERROR_MARK)
	{
	  /* ASSOCIATION, ACCESS, TEXT, BUFFER, and EVENT must not be READOnly */
	  if (TREE_READONLY (TREE_TYPE (newdecl)) &&
	      (CH_IS_ASSOCIATION_MODE (TREE_TYPE (newdecl)) ||
	       CH_IS_ACCESS_MODE (TREE_TYPE (newdecl)) ||
	       CH_IS_TEXT_MODE (TREE_TYPE (newdecl)) ||
	       CH_IS_BUFFER_MODE (TREE_TYPE (newdecl)) ||
	       CH_IS_EVENT_MODE (TREE_TYPE (newdecl))))
	    error_with_decl (newdecl, "`%s' must not be READonly");
	  rest_of_decl_compilation (newdecl, NULL_PTR,
				    global_bindings_p (), 0);
	}
    }
  return newdecl;
}

/* Return a chain of FIELD_DECLs for the names in NAMELIST.  All of
   of type TYPE.  When NAMELIST is passed in from the parser, it is
   in reverse order.
   LAYOUT is (NULL_TREE, integer_one_node, integer_zero_node, tree_list),
   meaning (default, pack, nopack, POS (...) ).  */

tree
grok_chill_fixedfields (namelist, type, layout)
     tree namelist, type;
     tree layout;
{
  tree decls = NULL_TREE;
  
  if (layout != NULL_TREE && TREE_CHAIN (namelist) != NULL_TREE)
    {
      if (layout != integer_one_node && layout != integer_zero_node)
	{
	  layout = NULL_TREE;
	  error ("POS may not be specified for a list of field declarations");
	}
    }

  /* we build the chain of FIELD_DECLs backwards, effectively
     unreversing the reversed names in NAMELIST.  */
  for (; namelist; namelist = TREE_CHAIN (namelist))
    {
      tree decl = build_decl (FIELD_DECL, 
			      TREE_VALUE (namelist), type);
      DECL_INITIAL (decl) = layout;
      TREE_CHAIN (decl) = decls;
      decls = decl;
    }
  
  return decls;
}

struct tree_pair
{
  tree value;
  tree decl;
};


/* Function to help qsort sort variant labels by value order.  */
static int
label_value_cmp (x, y)
     struct tree_pair *x, *y;
{
  return TREE_INT_CST_LOW (x->value) - TREE_INT_CST_LOW (y->value);
}

tree
make_chill_variants (tagfields, body, variantelse)
     tree tagfields;
     tree body;
     tree variantelse;
{
  tree utype;
  tree first = NULL_TREE;
  for (; body; body = TREE_CHAIN (body))
    {
      tree decls = TREE_VALUE (body);
      tree labellist = TREE_PURPOSE (body);

      if (labellist != NULL_TREE
	  && TREE_CODE (TREE_VALUE (labellist)) == TREE_LIST
	  && TREE_VALUE (TREE_VALUE (labellist)) == case_else_node
	  && TREE_CHAIN (labellist) == NULL_TREE)
	{
	  if (variantelse)
	    error ("(ELSE) case label as well as ELSE variant");
	  variantelse = decls;
	}
      else
	{
	  tree rtype = start_struct (RECORD_TYPE, NULL_TREE);
	  rtype = finish_struct (rtype, decls);

	  first = chainon (first, build_decl (FIELD_DECL, NULL_TREE, rtype));
      
	  TYPE_TAG_VALUES (rtype) = labellist;
	}
    }
  
  if (variantelse != NULL_TREE)
    {
      tree rtype = start_struct (RECORD_TYPE, NULL_TREE);
      rtype = finish_struct (rtype, variantelse);
      first = chainon (first,
		       build_decl (FIELD_DECL,
				   ELSE_VARIANT_NAME, rtype));
    }
  
  utype = start_struct (UNION_TYPE, NULL_TREE);
  utype = finish_struct (utype, first);
  TYPE_TAGFIELDS (utype) = tagfields;
  return utype;
}

tree
layout_chill_variants (utype)
     tree utype;
{
  tree first = TYPE_FIELDS (utype);
  int nlabels, label_index = 0;
  struct tree_pair *label_value_array;
  tree decl;
  extern int errorcount;
  
  if (TYPE_SIZE (utype))
    return utype;
  
  for (decl = first; decl; decl = TREE_CHAIN (decl))
    {
      tree tagfields = TYPE_TAGFIELDS (utype);
      tree t = TREE_TYPE (decl);
      tree taglist = TYPE_TAG_VALUES (t);
      if (DECL_NAME (decl) == ELSE_VARIANT_NAME)
	continue;
      if (tagfields == NULL_TREE)
	continue;
      for ( ; tagfields != NULL_TREE && taglist != NULL_TREE;
	   tagfields = TREE_CHAIN (tagfields), taglist = TREE_CHAIN (taglist))
	{
	  tree labellist = TREE_VALUE (taglist);
	  for (; labellist; labellist = TREE_CHAIN (labellist))
	    {
	      int compat_error = 0;
	      tree label_value = TREE_VALUE (labellist);
	      if (TREE_CODE (label_value) == RANGE_EXPR)
		{
		  if (TREE_OPERAND (label_value, 0) != NULL_TREE)
		    {
		      if (!CH_COMPATIBLE (TREE_OPERAND (label_value, 0),
					  TREE_TYPE (TREE_VALUE (tagfields)))
			  || !CH_COMPATIBLE (TREE_OPERAND (label_value, 1),
					     TREE_TYPE (TREE_VALUE (tagfields))))
			compat_error = 1;
		    }
		}
	      else if (TREE_CODE (label_value) == TYPE_DECL)
		{
		  if (!CH_COMPATIBLE (label_value,
				      TREE_TYPE (TREE_VALUE (tagfields))))
		    compat_error = 1;
		}
	      else if (TREE_CODE (label_value) == INTEGER_CST)
		{
		  if (!CH_COMPATIBLE (label_value,
				      TREE_TYPE (TREE_VALUE (tagfields))))
		    compat_error = 1;
		}
	      if (compat_error)
		{
		  if (TYPE_FIELDS (t) == NULL_TREE)
		    error ("inconsistent modes between labels and tag field");
		  else 
		    error_with_decl (TYPE_FIELDS (t),
				     "inconsistent modes between labels and tag field");
		}
	    }
	}
      if (tagfields != NULL_TREE)
	error ("too few tag labels");
      if (taglist != NULL_TREE)
	error ("too many tag labels");
    }

  /* Compute the number of labels to be checked for duplicates.  */
  nlabels = 0;
  for (decl = first; decl; decl = TREE_CHAIN (decl))
    {
      tree t = TREE_TYPE (decl);
       /* Only one tag (first case_label_list) supported, for now. */
      tree labellist = TYPE_TAG_VALUES (t);
      if (labellist)
	labellist = TREE_VALUE (labellist);
      
      for (; labellist != NULL_TREE; labellist = TREE_CHAIN (labellist))
	if (TREE_CODE (TREE_VALUE (labellist)) == INTEGER_CST)
	  nlabels++;
    }

  /* Check for duplicate label values.  */
  label_value_array = (struct tree_pair *)alloca (nlabels * sizeof (struct tree_pair));
  for (decl = first; decl; decl = TREE_CHAIN (decl))
    {
      tree t = TREE_TYPE (decl);
       /* Only one tag (first case_label_list) supported, for now. */
      tree labellist = TYPE_TAG_VALUES (t);
      if (labellist)
	labellist = TREE_VALUE (labellist);
      
      for (; labellist != NULL_TREE; labellist = TREE_CHAIN (labellist))
	{
	  struct tree_pair p;
	  
	  tree x = TREE_VALUE (labellist);
	  if (TREE_CODE (x) == RANGE_EXPR)
	    {
	      if (TREE_OPERAND (x, 0) != NULL_TREE)
		{
		  if (TREE_CODE (TREE_OPERAND (x, 0)) != INTEGER_CST)
		    error ("case label lower limit is not a discrete constant expression");
		  if (TREE_CODE (TREE_OPERAND (x, 1)) != INTEGER_CST)
		    error ("case label upper limit is not a discrete constant expression");
		}
	      continue;
	    }
	  else if (TREE_CODE (x) == TYPE_DECL)
	    continue;
	  else if (TREE_CODE (x) == ERROR_MARK)
	    continue;
	  else if (TREE_CODE (x) != INTEGER_CST) /* <-- FIXME: what about CONST_DECLs? */
	    {
	      error ("case label must be a discrete constant expression");
	      continue;
	    }
	  
	  if (TREE_CODE (x) == CONST_DECL)
	    x = DECL_INITIAL (x);
	  if (TREE_CODE (x) != INTEGER_CST) abort ();
	  p.value = x;
	  p.decl = decl;
	  if (p.decl == NULL_TREE)
	    p.decl = TREE_VALUE (labellist);
	  label_value_array[label_index++] = p;
	}
    }
  if (errorcount == 0)
    {
      int limit;
      qsort (label_value_array,
	     label_index, sizeof (struct tree_pair), label_value_cmp);
      limit = label_index - 1;
      for (label_index = 0; label_index < limit; label_index++)
	{
	  if (tree_int_cst_equal (label_value_array[label_index].value, 
				  label_value_array[label_index+1].value))
	    {
	      error_with_decl (label_value_array[label_index].decl,
			       "variant label declared here...");
	      error_with_decl (label_value_array[label_index+1].decl,
			       "...is duplicated here");
	    }
	}
    }
  layout_type (utype);
  return utype;
}

/* Convert a TREE_LIST of tag field names into a list of
   field decls, found from FIXED_FIELDS, re-using the input list. */

tree
lookup_tag_fields (tag_field_names, fixed_fields)
     tree tag_field_names;
     tree fixed_fields;
{
  tree list;
  for (list = tag_field_names; list != NULL_TREE; list = TREE_CHAIN (list))
    {
      tree decl = fixed_fields;
      for ( ; decl != NULL_TREE; decl = TREE_CHAIN (decl))
	{
	  if (DECL_NAME (decl) == TREE_VALUE (list))
	    {
	      TREE_VALUE (list) = decl;
	      break;
	    }
	}
      if (decl == NULL_TREE)
	{
	  error ("no field (yet) for tag %s",
		 IDENTIFIER_POINTER (TREE_VALUE (list)));
	  TREE_VALUE (list) = error_mark_node;
	}
    }
  return tag_field_names;
}

/* If non-NULL, TAGFIELDS is the tag fields for this variant record.
   BODY is a TREE_LIST of (optlabels, fixed fields).
   If non-null, VARIANTELSE is a fixed field for the else part of the
   variant record.  */

tree
grok_chill_variantdefs (tagfields, body, variantelse)
     tree tagfields, body, variantelse;
{
  tree t;
  
  t = make_chill_variants (tagfields, body, variantelse);
  if (pass != 1)
    t = layout_chill_variants (t);
  return build_decl (FIELD_DECL, NULL_TREE, t);
}

/*
  In pass 1, PARMS is a list of types (with attributes).
  In pass 2, PARMS is a chain of PARM_DECLs.
  */

int
start_chill_function (label, rtype, parms, exceptlist, attrs)
     tree label, rtype, parms, exceptlist, attrs;
{
  tree decl, fndecl, type, result_type, func_type;
  int nested = current_function_decl != 0;
  if (pass == 1)
    {
      func_type
	= build_chill_function_type (rtype, parms, exceptlist, 0);
      fndecl = build_decl (FUNCTION_DECL, label, func_type);

      save_decl (fndecl);
      
      /* Make the init_value nonzero so pushdecl knows this is not tentative.
	 error_mark_node is replaced below (in poplevel) with the BLOCK.  */
      DECL_INITIAL (fndecl) = error_mark_node;
      
      DECL_EXTERNAL (fndecl) = 0;
      
      /* This function exists in static storage.
	 (This does not mean `static' in the C sense!)  */
      TREE_STATIC (fndecl) = 1;

      for (; attrs != NULL_TREE; attrs = TREE_CHAIN (attrs))
	{
	  if (TREE_VALUE (attrs) == ridpointers[RID_GENERAL])
	    CH_DECL_GENERAL (fndecl) = 1;
	  else if (TREE_VALUE (attrs) == ridpointers[RID_SIMPLE])
	    CH_DECL_SIMPLE (fndecl) = 1;
	  else if (TREE_VALUE (attrs) == ridpointers[RID_RECURSIVE])
	    CH_DECL_RECURSIVE (fndecl) = 1;
	  else if (TREE_VALUE (attrs) == ridpointers[RID_INLINE])
	    DECL_INLINE (fndecl) = 1;
	  else
	    abort ();
	}
    }
  else /* pass == 2 */
    {
      fndecl = get_next_decl (); 
      if (DECL_NAME (fndecl) != label)
	abort ();           /* outta sync - got wrong decl */
      func_type = TREE_TYPE (fndecl);
      if (TYPE_RAISES_EXCEPTIONS (func_type) != NULL_TREE)
	{
	  /* In this case we have to add 2 parameters. 
	     See build_chill_function_type (pass == 1). */
	  tree arg;
        
	  arg = make_node (PARM_DECL);
	  DECL_ASSEMBLER_NAME (arg) = DECL_NAME (arg) = get_identifier (CALLER_FILE);
	  DECL_IGNORED_P (arg) = 1;
	  parms = chainon (parms, arg);
        
	  arg = make_node (PARM_DECL);
	  DECL_ASSEMBLER_NAME (arg) = DECL_NAME (arg) = get_identifier (CALLER_LINE);
	  DECL_IGNORED_P (arg) = 1;
	  parms = chainon (parms, arg);
	}
    }

  current_function_decl = fndecl;
  result_type = TREE_TYPE (func_type);
  if (CH_TYPE_NONVALUE_P (result_type))
    error ("non-value mode may only returned by LOC");

  pushlevel (1); /* Push parameters. */

  if (pass == 2)
    {
      DECL_ARGUMENTS (fndecl) = parms;
      for (decl = DECL_ARGUMENTS (fndecl), type = TYPE_ARG_TYPES (func_type);
	   decl != NULL_TREE;
	   decl = TREE_CHAIN (decl), type = TREE_CHAIN (type))
	{
	  /* check here that modes with the non-value property (like
	     BUFFER's, EVENT's, ASSOCIATION's, ACCESS's, or TEXT's) only
	     gets passed by LOC */
	  tree argtype = TREE_VALUE (type);
	  tree argattr = TREE_PURPOSE (type);

	  if (TREE_CODE (argtype) == REFERENCE_TYPE)
	    argtype = TREE_TYPE (argtype);

	  if (TREE_CODE (argtype) != ERROR_MARK &&
	      TREE_CODE_CLASS (TREE_CODE (argtype)) != 't')
	    {
	      error_with_decl (decl, "mode of `%s' is not a mode");
	      TREE_VALUE (type) = error_mark_node;
	    }

	  if (CH_TYPE_NONVALUE_P (argtype) &&
	      argattr != ridpointers[(int) RID_LOC])
	    error_with_decl (decl, "`%s' may only be passed by LOC");
	  TREE_TYPE (decl) = TREE_VALUE (type);
	  DECL_ARG_TYPE (decl) = TREE_TYPE (decl);
	  DECL_CONTEXT (decl) = fndecl;
	  TREE_READONLY (decl) = TYPE_READONLY (argtype);
	  layout_decl (decl, 0);
	}

      pushdecllist (DECL_ARGUMENTS (fndecl), 0);

      DECL_RESULT (current_function_decl)
	= build_decl (RESULT_DECL, NULL_TREE, result_type);

#if 0
      /* Write a record describing this function definition to the prototypes
	 file (if requested).  */
      gen_aux_info_record (fndecl, 1, 0, prototype);
#endif

      if (fndecl != global_function_decl || seen_action)
	{
	  /* Initialize the RTL code for the function.  */
	  init_function_start (fndecl, input_filename, lineno);

	  /* Set up parameters and prepare for return, for the function.  */
	  expand_function_start (fndecl, 0);
	}

      if (!nested)
	/* Allocate further tree nodes temporarily during compilation
	   of this function only.  */
	temporary_allocation ();

      /* If this fcn was already referenced via a block-scope `extern' decl (or
	 an implicit decl), propagate certain information about the usage. */
      if (TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (current_function_decl)))
	TREE_ADDRESSABLE (current_function_decl) = 1;
    }
      
  /* Z.200 requires that formal parameter names be defined in
     the same block as the procedure body.
     We could do this by keeping boths sets of DECLs in the same
     scope, but we would have to be careful to not merge the
     two chains (e.g. DECL_ARGUEMENTS musr not contains locals).
     Instead, we just make sure they have the same nesting_level. */
  current_nesting_level--;
  pushlevel (1); /* Push local variables. */

  if (pass == 2 && (fndecl != global_function_decl || seen_action))
    {
      /* generate label for possible 'exit' */
      expand_start_bindings (1);

      result_never_set = 1;
    }

  if (TREE_CODE (result_type) == VOID_TYPE)
    chill_result_decl = NULL_TREE;
  else
    {
      /* We use the same name as the keyword.
	 This makes it easy to print and change the RESULT from gdb. */
      char *result_str = (ignore_case || ! special_UC) ? "result" : "RESULT";
      if (pass == 2 && TREE_CODE (result_type) == ERROR_MARK)
	TREE_TYPE (current_scope->remembered_decls) = result_type;
      chill_result_decl = do_decl (get_identifier (result_str),
				   result_type, 0, 0, 0, 0);
      DECL_CONTEXT (chill_result_decl) = fndecl;
    }

  return 1;
}

/* For checking purpose added pname as new argument
   MW Wed Oct 14 14:22:10 1992 */
void
finish_chill_function ()
{
  register tree fndecl = current_function_decl;
  tree outer_function = decl_function_context (fndecl);
  int nested;
  if (outer_function == NULL_TREE && fndecl != global_function_decl)
    outer_function = global_function_decl;
  nested = current_function_decl != global_function_decl;
  if (pass == 2 && (fndecl != global_function_decl || seen_action))
    expand_end_bindings (getdecls (), 1, 0);
    
  /* pop out of function */
  poplevel (1, 1, 0);
  current_nesting_level++;
  /* pop out of its parameters */
  poplevel (1, 0, 1);

  if (pass == 2)
    {
      /*  TREE_READONLY (fndecl) = 1;
	  This caused &foo to be of type ptr-to-const-function which
	  then got a warning when stored in a ptr-to-function variable. */

      BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

      /* Must mark the RESULT_DECL as being in this function.  */

      DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

      if (fndecl != global_function_decl || seen_action)
	{
	  /* Generate rtl for function exit.  */
	  expand_function_end (input_filename, lineno, 0);

	  /* So we can tell if jump_optimize sets it to 1.  */
	  can_reach_end = 0;

	  /* Run the optimizers and output assembler code for this function. */
	  rest_of_compilation (fndecl);
	}

      if (DECL_SAVED_INSNS (fndecl) == 0 && ! nested)
	{
	  /* Stop pointing to the local nodes about to be freed.  */
	  /* But DECL_INITIAL must remain nonzero so we know this
	     was an actual function definition.  */
	  /* For a nested function, this is done in pop_chill_function_context.  */
	  DECL_INITIAL (fndecl) = error_mark_node;
	  DECL_ARGUMENTS (fndecl) = 0;
	}
    }
  current_function_decl = outer_function;
}

/* process SEIZE */

/* Points to the head of the _DECLs read from seize files.  */
#if 0
static tree seized_decls;

static tree processed_seize_files = 0;
#endif

void
chill_seize (old_prefix, new_prefix, postfix)
     tree old_prefix, new_prefix, postfix;
{
  if (pass == 1)
    {
      tree decl = build_alias_decl (old_prefix, new_prefix, postfix);
      DECL_SEIZEFILE(decl) = use_seizefile_name;
      save_decl (decl);
    }
  else /* pass == 2 */
    {
      /* Do nothing - get_next_decl automatically ignores ALIAS_DECLs */
    }
}
#if 0

/*
 * output a debug dump of a scope structure
 */
void
debug_scope (sp)
     struct scope *sp;
{
  if (sp == (struct scope *)NULL)
    {
      fprintf (stderr, "null scope ptr\n");
      return;
    }
  fprintf (stderr, "enclosing 0x%x ",           sp->enclosing);
  fprintf (stderr, "next 0x%x ",                sp->next); 
  fprintf (stderr, "remembered_decls 0x%x ",    sp->remembered_decls);
  fprintf (stderr, "decls 0x%x\n",              sp->decls); 
  fprintf (stderr, "shadowed 0x%x ",            sp->shadowed); 
  fprintf (stderr, "blocks 0x%x ",              sp->blocks); 
  fprintf (stderr, "this_block 0x%x ",          sp->this_block); 
  fprintf (stderr, "level_chain 0x%x\n",        sp->level_chain);
  fprintf (stderr, "module_flag %c ",           sp->module_flag ? 'T' : 'F');
  fprintf (stderr, "first_child_module 0x%x ",  sp->first_child_module);
  fprintf (stderr, "next_sibling_module 0x%x\n", sp->next_sibling_module);
  if (sp->remembered_decls != NULL_TREE)
    {
      tree temp;
      fprintf (stderr, "remembered_decl chain:\n");
      for (temp = sp->remembered_decls; temp; temp = TREE_CHAIN (temp))
	debug_tree (temp);
    }
}
#endif

static void
save_decl (decl)
     tree decl;
{
  if (current_function_decl != global_function_decl)
    DECL_CONTEXT (decl) = current_function_decl;

  TREE_CHAIN (decl) = current_scope->remembered_decls;
  current_scope->remembered_decls = decl;
#if 0
  fprintf (stderr, "\n\nsave_decl 0x%x\n", decl);
  debug_scope (current_scope);  /* ************* */
#endif
  set_nesting_level (decl, current_nesting_level);
}

static tree
get_next_decl ()
{
  tree decl;
  do
    {
      decl = current_scope->remembered_decls;
      current_scope->remembered_decls = TREE_CHAIN (decl);
      /* We ignore ALIAS_DECLs, because push_scope_decls
	 can convert a single ALIAS_DECL representing 'SEIZE ALL'
	 into one ALIAS_DECL for each seizeable name.
	 This means we lose the nice one-to-one mapping
         between pass 1 decls and pass 2 decls.
	 (Perhaps ALIAS_DECLs should not be on the remembered_decls list.) */
    } while (decl && TREE_CODE (decl) == ALIAS_DECL);
  return decl;
}

/* At the end of pass 1, we reverse the chronological chain of scopes. */

void
switch_to_pass_2 ()
{
#if 0
  extern int errorcount, sorrycount;
#endif
  if (current_scope != &builtin_scope)
    abort ();
  last_scope = &builtin_scope;
  builtin_scope.remembered_decls = nreverse (builtin_scope.remembered_decls);
  write_grant_file ();

#if 0
  if (errorcount || sorrycount)
    exit (FATAL_EXIT_CODE);
  else
#endif
  if (grant_only_flag)
    exit (SUCCESS_EXIT_CODE);

  pass = 2;
  module_number = 0;
  next_module = &first_module;
}

/*
 * Called during pass 2, when we're processing actions, to
 * generate a temporary variable.  These don't need satisfying
 * because they're compiler-generated and always declared
 * before they're used.
 */
tree
decl_temp1 (name, type, opt_static, opt_init, 
	    opt_external, opt_public)
     tree name, type;
     int  opt_static;
     tree opt_init;
     int  opt_external, opt_public;
{
  int orig_pass = pass;           /* be cautious */
  tree mydecl;

  pass = 1;
  mydecl = do_decl (name, type, opt_static, opt_static,
		    opt_init, opt_external);

  if (opt_public)
    TREE_PUBLIC (mydecl) = 1;
  pass = 2;
  do_decl (name, type, opt_static, opt_static, opt_init, opt_external);

  pass = orig_pass;
  return mydecl;
}

/* True if we're reading a seizefile, but we haven't seen a SPEC MODULE yet.
   For backwards compatibility, we treat declarations in such a context
   as implicity granted. */

tree
set_module_name (name)
     tree name;
{
  module_number++;
  if (name == NULL_TREE)
    {
      /* NOTE: build_prefix_clause assumes a generated
	 module starts with a '_'. */
      char buf[20];
      sprintf (buf, "_MODULE_%d", module_number);
      name = get_identifier (buf);
    }
  return name;
}

tree
push_module (name, is_spec_module)
     tree name;
     int is_spec_module;
{ 
  struct module *new_module;
  if (pass == 1)
    {
      new_module = (struct module*) permalloc (sizeof (struct module));
      new_module->prev_module = current_module;

      *next_module = new_module;
    }
  else
    {
      new_module = *next_module;
    }
  next_module = &new_module->next_module;

  new_module->procedure_seen = 0;
  new_module->is_spec_module = is_spec_module;
  new_module->name = name;
  if (current_module)
    new_module->prefix_name
      = get_identifier3 (IDENTIFIER_POINTER (current_module->prefix_name),
			 "__", IDENTIFIER_POINTER (name));
  else
    new_module->prefix_name = name;

  new_module->granted_decls = NULL_TREE;
  new_module->nesting_level = current_nesting_level + 1;

  current_module = new_module;
  current_module_nesting_level = new_module->nesting_level;
  in_pseudo_module = name ? 0 : 1;

  pushlevel (1);

  current_scope->module_flag = 1;

  *current_scope->enclosing->tail_child_module = current_scope;
  current_scope->enclosing->tail_child_module
    = &current_scope->next_sibling_module;

  /* Rename the global function to have the same name as
     the first named non-spec module. */
  if (!is_spec_module
      && IDENTIFIER_POINTER (name)[0] != '_'
      && IDENTIFIER_POINTER (DECL_NAME (global_function_decl))[0] == '_')
    {
      tree fname = get_identifier3 ("", IDENTIFIER_POINTER (name), "_");
      DECL_NAME (global_function_decl) = fname;
      DECL_ASSEMBLER_NAME (global_function_decl) = fname;
    }

  return name;   /* may have generated a name */
}
/* Make a copy of the identifier NAME, replacing each '!' by '__'. */
tree
fix_identifier (name)
     tree name;
{
  char *buf = (char*)alloca (2 * IDENTIFIER_LENGTH (name) + 1);
  int fixed = 0;
  register char *dptr = buf;
  register char *sptr = IDENTIFIER_POINTER (name);
  for (; *sptr; sptr++)
    {
      if (*sptr == '!')
	{
	  *dptr++ = '_';
	  *dptr++ = '_';
	  fixed++;
	}
      else
	*dptr++ = *sptr;
    }
  *dptr = '\0';
  return fixed ? get_identifier (buf) : name;
}

void
find_granted_decls ()
{
  if (pass == 1)
    {
      /* Match each granted name to a granted decl. */

      tree alias = current_module->granted_decls;
      tree next_alias, decl;
      /* This is an O(M*N) algorithm.  FIXME! */
      for (; alias; alias = next_alias)
	{
	  int found = 0;
	  next_alias = TREE_CHAIN (alias);
	  for (decl = current_scope->remembered_decls;
	       decl; decl = TREE_CHAIN (decl))
	    {
	      tree new_name = (! DECL_NAME (decl)) ? NULL_TREE :
		              decl_check_rename (alias, 
						 DECL_NAME (decl));

	      if (!new_name)
		continue;
	      /* A Seized declaration is not grantable. */
	      if (TREE_CODE (decl) == ALIAS_DECL && !CH_DECL_GRANTED (decl))
		continue;
	      found = 1;
	      if (global_bindings_p ())
		TREE_PUBLIC (decl) = 1;
	      if (DECL_ASSEMBLER_NAME (decl) == NULL_TREE)
		DECL_ASSEMBLER_NAME (decl) = fix_identifier (new_name);
	      if (DECL_POSTFIX_ALL (alias))
		{
		  tree new_alias
		    = build_alias_decl (NULL_TREE, NULL_TREE, new_name);
		  TREE_CHAIN (new_alias) = TREE_CHAIN (alias);
		  TREE_CHAIN (alias) = new_alias;
		  DECL_ABSTRACT_ORIGIN (new_alias) = decl;
		  DECL_SOURCE_LINE (new_alias) = 0;
		  DECL_SEIZEFILE (new_alias) = DECL_SEIZEFILE (alias);
		}
	      else
		{
		  DECL_ABSTRACT_ORIGIN (alias) = decl;
		  break;
		}
	    }
	  if (!found)
	    {
	      error_with_decl (alias, "Nothing named `%s' to grant.");
	      DECL_ABSTRACT_ORIGIN (alias) = error_mark_node;
	    }
	}
    }
}

void
pop_module ()
{
  tree decl;
  struct scope *module_scope = current_scope;

  poplevel (0, 0, 0);

  if (pass == 1)
    {
      /* Write out the grant file. */
      if (!current_module->is_spec_module)
	{
	  /* After reversal, TREE_CHAIN (last_old_decl) is the oldest
	     decl of the current module. */
	  write_spec_module (module_scope->remembered_decls,
			     current_module->granted_decls);
	}

      /* Move the granted decls into the enclosing scope. */
      if (current_scope == global_scope)
	{
	  tree next_decl;
	  for (decl = current_module->granted_decls; decl; decl = next_decl)
	    {
	      tree name = DECL_NAME (decl);
	      next_decl = TREE_CHAIN (decl);
	      if (name != NULL_TREE)
		{
		  tree old_decl = IDENTIFIER_OUTER_VALUE (name);
		  set_nesting_level (decl, current_nesting_level);
		  if (old_decl != NULL_TREE)
		    {
		      pedwarn_with_decl (decl, "duplicate grant for `%s'");
		      pedwarn_with_decl (old_decl, "previous grant for `%s'");
		      TREE_CHAIN (decl) = TREE_CHAIN (old_decl);
		      TREE_CHAIN (old_decl) = decl;
		    }
		  else
		    {
		      TREE_CHAIN (decl) = outer_decls;
		      outer_decls = decl;
		      IDENTIFIER_OUTER_VALUE (name) = decl;
		    }
		}
	    }
	}
      else
	current_scope->granted_decls = chainon (current_module->granted_decls,
						current_scope->granted_decls);
    }

  chill_check_no_handlers (); /* Sanity test */
  current_module = current_module->prev_module;
  current_module_nesting_level = current_module ?
    current_module->nesting_level : 0;
  in_pseudo_module = 0;
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  /* We return -1 here for the sake of variable_size() in ../stor-layout.c. */
  return (current_function_decl == NULL_TREE 
	  || current_function_decl == global_function_decl) ? -1 : 0;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return current_scope->decls != 0;
}

/* Make DECL visible.
   Save any existing definition.
   Check redefinitions at the same level.
   Suppress error messages if QUIET is true. */

void
proclaim_decl (decl, quiet)
     tree decl;
     int quiet;
{
  tree name = DECL_NAME (decl);
  if (name)
    {
      tree old_decl = IDENTIFIER_LOCAL_VALUE (name);
      if (old_decl == NULL) ; /* No duplication */
      else if (DECL_NESTING_LEVEL (old_decl) != current_nesting_level)
	{
	  /* Record for restoration when this binding level ends.  */
	  current_scope->shadowed
	    = tree_cons (name, old_decl, current_scope->shadowed);
	}
      else if (DECL_WEAK_NAME (decl))
	return;
      else if (!DECL_WEAK_NAME (old_decl))
	{
	  tree base_decl = decl, base_old_decl = old_decl;
	  while (TREE_CODE (base_decl) == ALIAS_DECL)
	    base_decl = DECL_ABSTRACT_ORIGIN (base_decl);
	  while (TREE_CODE (base_old_decl) == ALIAS_DECL)
	    base_old_decl = DECL_ABSTRACT_ORIGIN (base_old_decl);
	  /* Note that duplicate definitions are allowed for set elements
	     of similar set modes.  See Z200 (1988) 12.2.2.
	     However, if the types are identical, we are defining the
	     same name multiple times in the same SET, which is naughty. */
	  if (!quiet && base_decl != base_old_decl)
	    {
	      if (TREE_CODE (base_decl) != CONST_DECL
		  || TREE_CODE (base_old_decl) != CONST_DECL
		  || !CH_DECL_ENUM (base_decl)
		  || !CH_DECL_ENUM (base_old_decl)
		  || TREE_TYPE (base_decl) == TREE_TYPE (base_old_decl)
		  || !CH_SIMILAR (TREE_TYPE (base_decl),
				  TREE_TYPE(base_old_decl)))
		{
		  error_with_decl (decl, "duplicate definition `%s'");
		  error_with_decl (old_decl, "previous definition of `%s'");
		}
	    }
	}
      IDENTIFIER_LOCAL_VALUE (name) = decl;
    }
  /* Should be redundant most of the time ... */
  set_nesting_level (decl, current_nesting_level);
}

/* Return tree_cons (NULL_TREE, ELEMENT, LIST) unless ELEMENT
   is already in LIST, in which case return LIST. */

static tree
maybe_acons (element, list)
     tree element, list;
{
  tree pair;
  for (pair = list; pair; pair = TREE_CHAIN (pair))
    if (element == TREE_VALUE (pair))
      return list;
  return tree_cons (NULL_TREE, element, list);
}

struct path
{
  struct path *prev;
  tree node;
};

/* Look for implied types (enumeral types) implied by TYPE (a decl or type).
   Add these to list.
   Use old_path to guard against cycles. */

tree
find_implied_types (type, old_path, list)
     tree type;
     struct path *old_path;
     tree list;
{
  struct path path[1], *link;
  if (type == NULL_TREE)
    return list;
  path[0].prev = old_path;
  path[0].node = type;

  /* Check for a cycle.  Something more clever might be appropriate.  FIXME? */
  for (link = old_path; link; link = link->prev)
    if (link->node == type)
      return list;

  switch (TREE_CODE (type))
    {
    case ENUMERAL_TYPE:
      return maybe_acons (type, list);
    case LANG_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case INTEGER_TYPE:
      return find_implied_types (TREE_TYPE (type), path, list);
    case SET_TYPE:
      return find_implied_types (TYPE_DOMAIN (type), path, list);
    case FUNCTION_TYPE:
#if 0
    case PROCESS_TYPE:
#endif
      { tree t;
	list = find_implied_types (TREE_TYPE (type), path, list);
	for (t = TYPE_ARG_TYPES (type); t != NULL_TREE; t = TREE_CHAIN (t))
	  list = find_implied_types (TREE_VALUE (t), path, list);
	return list;
      }
    case ARRAY_TYPE:
      list = find_implied_types (TYPE_DOMAIN (type), path, list);
      return find_implied_types (TREE_TYPE (type), path, list);
    case RECORD_TYPE:
    case UNION_TYPE:
      { tree fields;
	for (fields = TYPE_FIELDS (type); fields != NULL_TREE;
	     fields = TREE_CHAIN (fields))
	  list = find_implied_types (TREE_TYPE (fields), path, list);
	return list;
      }

    case IDENTIFIER_NODE:
      return find_implied_types (lookup_name (type), path, list);
      break;
    case ALIAS_DECL:
      return find_implied_types (DECL_ABSTRACT_ORIGIN (type), path, list);
    case VAR_DECL:
    case FUNCTION_DECL:
    case TYPE_DECL:
      return find_implied_types (TREE_TYPE (type), path, list);
    default:
      return list;
    }
}

/* Make declarations in current scope visible.
   Also, expand SEIZEs, and make correspondong ALIAS_DECLs visible. */

static void
push_scope_decls (quiet)
     int quiet;  /* If 1, we're pre-scanning, so suppress errors. */
{
  tree decl;

  /* First make everything except 'SEIZE ALL' names visible, before
     handling 'SEIZE ALL'.  (This makes it easier to check 'seizable'). */
  for (decl = current_scope->remembered_decls; decl; decl = TREE_CHAIN (decl))
    {
      if (TREE_CODE (decl) == ALIAS_DECL)
	{
	  if (DECL_POSTFIX_ALL (decl))
	    continue;
	  if (DECL_ABSTRACT_ORIGIN (decl) == NULL_TREE)
	    {
	      tree val = lookup_name_for_seizing (decl);
	      if (val == NULL_TREE)
		{
		  error_with_file_and_line
		    (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl),
		     "cannot SEIZE `%s'",
		     IDENTIFIER_POINTER (DECL_OLD_NAME (decl)));
		  val = error_mark_node;
		}
	      DECL_ABSTRACT_ORIGIN (decl) = val;
	    }
	}
      proclaim_decl (decl, quiet);
    }

  pushdecllist (current_scope->granted_decls, quiet);

  /* Now handle SEIZE ALLs. */
  for (decl = current_scope->remembered_decls; decl; )
    {
      tree next_decl = TREE_CHAIN (decl);
      if (TREE_CODE (decl) == ALIAS_DECL
	  && DECL_ABSTRACT_ORIGIN (decl) == NULL_TREE
	  && DECL_POSTFIX_ALL (decl))
	{
	  /* We saw a "SEIZE ALL".  Replace it be a SEIZE for each
	     declaration visible in the surrounding scope.
	     Note that this complicates get_next_decl(). */
	  tree candidate;
	  tree last_new_alias = decl;
	  DECL_ABSTRACT_ORIGIN (decl) = error_mark_node;
	  if (current_scope->enclosing == global_scope)
	    candidate = outer_decls;
	  else
	    candidate = current_scope->enclosing->decls;
	  for ( ; candidate; candidate = TREE_CHAIN (candidate))
	    {
	      tree seizename = DECL_NAME (candidate);
	      tree new_name;
	      tree new_alias;
	      if (!seizename)
		continue;
	      new_name = decl_check_rename (decl, seizename);
	      if (!new_name)
		continue;

	      /* Check if candidate is seizable. */
	      if (lookup_name (new_name) != NULL_TREE)
		continue;

	      new_alias = build_alias_decl (NULL_TREE,NULL_TREE, new_name);
	      TREE_CHAIN (new_alias) = TREE_CHAIN (last_new_alias);
	      TREE_CHAIN (last_new_alias) = new_alias;
	      last_new_alias = new_alias;
	      DECL_ABSTRACT_ORIGIN (new_alias) = candidate;
	      DECL_SOURCE_LINE (new_alias) = 0;

	      proclaim_decl (new_alias, quiet);
	    }
	}
      decl = next_decl;
    }

  /* Link current_scope->remembered_decls at the head of the
     current_scope->decls list (just like pushdecllist, but
     without calling proclaim_decl, since we've already done that). */
  if ((decl = current_scope->remembered_decls) != NULL_TREE)
    {
      while (TREE_CHAIN (decl) != NULL_TREE)
	decl = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = current_scope->decls;
      current_scope->decls = current_scope->remembered_decls;
    }
}

static void
pop_scope_decls (decls_limit, shadowed_limit)
     tree decls_limit, shadowed_limit;
{
  /* Remove the temporary bindings we made. */
  tree link = current_scope->shadowed;
  tree decl = current_scope->decls;
  if (decl != decls_limit)
    {
      while (decl != decls_limit)
	{
	  tree next = TREE_CHAIN (decl);
	  if (DECL_NAME (decl))
	    {
	      /* If the ident. was used or addressed via a local extern decl,
		 don't forget that fact.  */
	      if (DECL_EXTERNAL (decl))
		{
		  if (TREE_USED (decl))
		    TREE_USED (DECL_NAME (decl)) = 1;
		  if (TREE_ADDRESSABLE (decl))
		    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (decl)) = 1;
		}
	      IDENTIFIER_LOCAL_VALUE (DECL_NAME (decl)) = 0;
	    }
	  if (next == decls_limit)
	    {
	      TREE_CHAIN (decl) = NULL_TREE;
	      break;
	    }
	  decl = next;
	}
      current_scope->decls = decls_limit;
    }
  
  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */
  for ( ; link != shadowed_limit; link = TREE_CHAIN (link))
    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);
  current_scope->shadowed = shadowed_limit;
}

/* Return list of weak names (as ALIAS_DECLs) implied by IMPLIED_TYPES. */

static tree
build_implied_names (implied_types)
     tree implied_types;
{
  tree aliases = NULL_TREE;

  for ( ; implied_types; implied_types = TREE_CHAIN (implied_types))
    {
      tree enum_type = TREE_VALUE (implied_types);
      tree link = TYPE_VALUES (enum_type);
      if (TREE_CODE (enum_type) != ENUMERAL_TYPE)
	abort ();
      
      for ( ; link; link = TREE_CHAIN (link))
	{
	  /* We don't handle renaming/prefixes (Blue Book p 163) FIXME */
	  /* Note that before enum_type is laid out, TREE_VALUE (link)
	     is a CONST_DECL, while after it is laid out,
	     TREE_VALUE (link) is an INTEGER_CST.  Either works. */
	  tree alias
	    = build_alias_decl (NULL_TREE, NULL_TREE, TREE_PURPOSE (link));
	  DECL_ABSTRACT_ORIGIN (alias) = TREE_VALUE (link);
	  DECL_WEAK_NAME (alias) = 1;
	  TREE_CHAIN (alias) = aliases;
	  aliases = alias;
	  /* Strictlt speaking, we should have a pointer from the alias
	     to the decl, so we can make sure that the alias is only
	     visible when the decl is.  FIXME */
	}
    }
  return aliases;
}

static void
bind_sub_modules (do_weak)
     int do_weak;
{
  tree decl;
  int save_module_nesting_level = current_module_nesting_level;
  struct scope *saved_scope = current_scope;
  struct scope *nested_module = current_scope->first_child_module;

  while (nested_module != NULL)
    {
      tree saved_shadowed = nested_module->shadowed;
      tree saved_decls = nested_module->decls;
      current_nesting_level++;
      current_scope = nested_module;
      current_module_nesting_level = current_nesting_level;
      if (do_weak == 0)
	push_scope_decls (1);
      else
	{
	  tree implied_types = NULL_TREE;
	  /* Push weak names implied by decls in current_scope. */
	  for (decl = current_scope->remembered_decls;
	       decl; decl = TREE_CHAIN (decl))
	    if (TREE_CODE (decl) == ALIAS_DECL)
	      implied_types = find_implied_types (decl, NULL, implied_types);
	  for (decl = current_scope->granted_decls;
	       decl; decl = TREE_CHAIN (decl))
	    implied_types = find_implied_types (decl, NULL, implied_types);
	  current_scope->weak_decls = build_implied_names (implied_types);
	  pushdecllist (current_scope->weak_decls, 1);
	}

      bind_sub_modules (do_weak);
      for (decl = current_scope->remembered_decls;
	   decl; decl = TREE_CHAIN (decl))
	satisfy_decl (decl, 1);
      pop_scope_decls (saved_decls, saved_shadowed);
      current_nesting_level--;
      nested_module = nested_module->next_sibling_module;
    }

  current_scope = saved_scope;
  current_module_nesting_level = save_module_nesting_level;
}

/* Enter a new binding level.
   If two_pass==0, assume we are called from non-Chill-specific parts
   of the compiler.  These parts assume a single pass.
   If two_pass==1,  we're called from Chill parts of the compiler.
*/

void
pushlevel (two_pass)
     int two_pass;
{
  register struct scope *newlevel;

  current_nesting_level++;
  if (!two_pass)
    {
      newlevel = (struct scope *)xmalloc (sizeof(struct scope));
      *newlevel = clear_scope;
      newlevel->enclosing = current_scope;
      current_scope = newlevel;
    }
  else if (pass < 2)
    {
      newlevel = (struct scope *)permalloc (sizeof(struct scope));
      *newlevel = clear_scope;
      newlevel->tail_child_module = &newlevel->first_child_module;
      newlevel->enclosing = current_scope;
      current_scope = newlevel;
      last_scope->next = newlevel;
      last_scope = newlevel;
    }
  else /* pass == 2 */
    {
      tree decl;
      newlevel = current_scope = last_scope = last_scope->next;

      push_scope_decls (0);
      pushdecllist (current_scope->weak_decls, 0);

      /* If this is not a module scope, scan ahead for locally nested
	 modules.  (If this is a module, that's already done.) */
      if (!current_scope->module_flag)
	{
	  bind_sub_modules (0);
	  bind_sub_modules (1);
	}

      for (decl = current_scope->remembered_decls;
	   decl; decl = TREE_CHAIN (decl))
	satisfy_decl (decl, 0);
    }

  /* Add this level to the front of the chain (stack) of levels that
     are active.  */

  newlevel->level_chain = current_scope;
  current_scope = newlevel;

  newlevel->two_pass = two_pass;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  register tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  tree subblocks;
  tree block = 0;
  tree decl;
  int block_previously_created = 0;

  if (current_scope == NULL)
    return error_mark_node;

  subblocks = current_scope->blocks;

  /* Get the decls in the order they were written.
     Usually current_scope->decls is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_scope->decls
      = decls = nreverse (current_scope->decls);
  else
    decls = current_scope->decls;

  if (pass == 2)
    {
      /* Output any nested inline functions within this block
	 if they weren't already output.  */

      for (decl = decls; decl; decl = TREE_CHAIN (decl))
	if (TREE_CODE (decl) == FUNCTION_DECL
	    && ! TREE_ASM_WRITTEN (decl)
	    && DECL_INITIAL (decl) != 0
	    && TREE_ADDRESSABLE (decl))
	  {
	    /* If this decl was copied from a file-scope decl
	       on account of a block-scope extern decl,
	       propagate TREE_ADDRESSABLE to the file-scope decl.  */
	    if (DECL_ABSTRACT_ORIGIN (decl) != 0)
	      TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
	    else
	      {
		push_function_context ();
		output_inline_function (decl);
		pop_function_context ();
	      }
	  }

      /* Clear out the meanings of the local variables of this level.  */
      pop_scope_decls (NULL_TREE, NULL_TREE);

      /* If there were any declarations or structure tags in that level,
	 or if this level is a function body,
	 create a BLOCK to record them for the life of this function.  */

      block = 0;
      block_previously_created = (current_scope->this_block != 0);
      if (block_previously_created)
	block = current_scope->this_block;
      else if (keep || functionbody)
	block = make_node (BLOCK);
      if (block != 0)
	{
	  tree *ptr;
	  BLOCK_VARS (block) = decls;

	  /* Splice out ALIAS_DECL and LABEL_DECLs,
	     since instantiate_decls can't handle them. */
	  for (ptr = &BLOCK_VARS (block); *ptr; )
	    {
	      decl = *ptr;
	      if (TREE_CODE (decl) == ALIAS_DECL
		  || TREE_CODE (decl) == LABEL_DECL)
		*ptr = TREE_CHAIN (decl);
	      else
		ptr = &TREE_CHAIN(*ptr);
	    }

	  BLOCK_SUBBLOCKS (block) = subblocks;
	  remember_end_note (block);
	}

      /* In each subblock, record that this is its superior.  */

      for (link = subblocks; link; link = TREE_CHAIN (link))
	BLOCK_SUPERCONTEXT (link) = block;

    }

  /* If the level being exited is the top level of a function,
     check over all the labels, and clear out the current
     (function local) meanings of their names.  */

  if (pass == 2 && functionbody)
    {
      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the BLOCK because they are
	 found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;

#if 0
      /* Clear out the definitions of all label names,
	 since their scopes end here,
	 and add them to BLOCK_VARS.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  register tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == 0)
	    {
	      error_with_decl (label, "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (label));
	    }
	  else if (warn_unused && !TREE_USED (label))
	    warning_with_decl (label, "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (label)) = 0;

	  /* Put the labels into the "variables" of the
	     top-level block, so debugger can see them.  */
	  TREE_CHAIN (label) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = label;
	}
#endif
    }

  if (pass < 2)
    {
      current_scope->remembered_decls
	= nreverse (current_scope->remembered_decls);
      current_scope->granted_decls = nreverse (current_scope->granted_decls);
    }

  current_scope = current_scope->enclosing;
  current_nesting_level--;

  if (pass < 2)
    {
      return NULL_TREE;
    }

  /* Dispose of the block that we just made inside some higher level.  */
  if (functionbody)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    {
      if (!block_previously_created)
        current_scope->blocks
          = chainon (current_scope->blocks, block);
    }
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_scope->blocks
      = chainon (current_scope->blocks, subblocks);

  if (block)
    TREE_USED (block) = 1;
  return block;
}

/* Delete the node BLOCK from the current binding level.
   This is used for the block inside a stmt expr ({...})
   so that the block can be reinserted where appropriate.  */

void
delete_block (block)
     tree block;
{
  tree t;
  if (current_scope->blocks == block)
    current_scope->blocks = TREE_CHAIN (block);
  for (t = current_scope->blocks; t;)
    {
      if (TREE_CHAIN (t) == block)
	TREE_CHAIN (t) = TREE_CHAIN (block);
      else
	t = TREE_CHAIN (t);
    }
  TREE_CHAIN (block) = NULL;
  /* Clear TREE_USED which is always set by poplevel.
     The flag is set again if insert_block is called.  */
  TREE_USED (block) = 0;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside teh BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_scope->blocks
    = chainon (current_scope->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     register tree block;
{
  current_scope->this_block = block;
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says. */

tree
pushdecl (x)
     tree x;
{
  register tree name = DECL_NAME (x);
  register struct scope *b = current_scope;

  DECL_CONTEXT (x) = current_function_decl;
  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if (TREE_CODE (x) == FUNCTION_DECL && DECL_INITIAL (x) == 0
      && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = 0;

  if (name)
    proclaim_decl (x, 0);

  if (TREE_CODE (x) == TYPE_DECL && DECL_SOURCE_LINE (x) == 0
      && TYPE_NAME (TREE_TYPE (x)) == 0)
    TYPE_NAME (TREE_TYPE (x)) = x;

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->decls;
  b->decls = x;

  return x;
}

/* Make DECLS (a chain of decls) visible in the current_scope. */

static void
pushdecllist (decls, quiet)
     tree decls;
     int quiet;
{
  tree last = NULL_TREE, decl;

  for (decl = decls; decl != NULL_TREE; 
       last = decl, decl = TREE_CHAIN (decl))
    {
      proclaim_decl (decl, quiet);
    }

  if (last)
    {
      TREE_CHAIN (last) = current_scope->decls;
      current_scope->decls = decls;
    }
}

/* Like pushdecl, only it places X in GLOBAL_SCOPE, if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  register tree t;
  register struct scope *b = current_scope;

  current_scope = global_scope;
  t = pushdecl (x);
  current_scope = b;
  return t;
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     char *filename;
     int line;
     tree name;
{
  tree decl;

  if (pass == 1)
    {
      decl = build_decl (LABEL_DECL, name, void_type_node);

      /* A label not explicitly declared must be local to where it's ref'd.  */
      DECL_CONTEXT (decl) = current_function_decl;

      DECL_MODE (decl) = VOIDmode;

      /* Say where one reference is to the label,
	 for the sake of the error if it is not defined.  */
      DECL_SOURCE_LINE (decl) = line;
      DECL_SOURCE_FILE (decl) = filename;

      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;

      DECL_ACTION_NESTING_LEVEL (decl) = action_nesting_level;

      save_decl (decl);
    }
  else
    {
      decl = get_next_decl ();
      /* Make sure every label has an rtx.  */

      label_rtx (decl);
      expand_label (decl);
    }
  return decl;
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  /* This is a kludge, so that dbxout_init can get the predefined types,
     which are in the builtin_scope, though when it is called,
     the current_scope is the global_scope.. */
  if (current_scope == global_scope)
    return builtin_scope.decls;
  return current_scope->decls;
}

#if 0
/* Store the list of declarations of the current level.
   This is done for the parameter declarations of a function being defined,
   after they are modified in the light of any missing parameters.  */

static void
storedecls (decls)
     tree decls;
{
  current_scope->decls = decls;
}
#endif

/* Look up NAME in the current binding level and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (name)
     tree name;
{
  register tree val = IDENTIFIER_LOCAL_VALUE (name);

  if (val == NULL_TREE)
    return NULL_TREE;
  if (TREE_CODE_CLASS (TREE_CODE (val)) == 'c')
    return val;
  if (DECL_NESTING_LEVEL (val) > BUILTIN_NESTING_LEVEL
      && DECL_NESTING_LEVEL (val) < current_module_nesting_level)
    {
      return NULL_TREE;
    }
  while (TREE_CODE (val) == ALIAS_DECL)
    {
      val = DECL_ABSTRACT_ORIGIN (val);
      if (TREE_CODE (val) == ERROR_MARK)
	return NULL_TREE;
    }
  if (TREE_CODE (val) == BASED_DECL)
    {
      return build_chill_indirect_ref (DECL_ABSTRACT_ORIGIN (val),
				       TREE_TYPE (val), 1);
    }
  if (TREE_CODE (val) == WITH_DECL)
    return build_component_ref (DECL_ABSTRACT_ORIGIN (val), DECL_NAME (val));
  return val;
}

#if 0
/* Similar to `lookup_name' but look only at current binding level.  */

static tree
lookup_name_current_level (name)
     tree name;
{
  register tree val = IDENTIFIER_LOCAL_VALUE (name);
  if (val && DECL_NESTING_LEVEL (val) == current_nesting_level)
    return val;
  return NULL_TREE;
}
#endif

static tree
lookup_name_for_seizing (seize_decl)
     tree seize_decl;
{
  tree name = DECL_OLD_NAME (seize_decl);
  register tree val;
  val = IDENTIFIER_LOCAL_VALUE (name);
  if (val == NULL_TREE || DECL_NESTING_LEVEL (val) == BUILTIN_NESTING_LEVEL)
    {
      val = IDENTIFIER_OUTER_VALUE (name);
      if (val == NULL_TREE)
	return NULL_TREE;
      if (TREE_CHAIN (val) && DECL_NAME (TREE_CHAIN (val)) == name)
	{ /* More than one decl with the same name has been granted
	     into the same global scope.  Pick the one (we hope) that
	     came from a seizefile the matches the most recent
	     seizefile (as given by DECL_SEIZEFILE (seize_decl).) */
	  tree d, best = NULL_TREE;
	  for (d = val; d != NULL_TREE && DECL_NAME (d) == name;
	       d = TREE_CHAIN (d))
	    if (DECL_SEIZEFILE (d) == DECL_SEIZEFILE (seize_decl))
	      {
		if (best)
		  {
		    error_with_decl (seize_decl,
				     "ambiguous choice for seize `%s' -");
		    error_with_decl (best, " - can seize this `%s' -");
		    error_with_decl (d, " - or this granted decl `%s'");
		    return NULL_TREE;
		  }
		best = d;
	      }
	  if (best == NULL_TREE)
	    {
	      error_with_decl (seize_decl,
			       "ambiguous choice for seize `%s' -");
	      error_with_decl (val, " - can seize this `%s' -");
	      error_with_decl (TREE_CHAIN (val),
			       " - or this granted decl `%s'");
	      return NULL_TREE;
	    }
	  val = best;
	}
    }
#if 0
  /* We don't need to handle this, as long as we
     resolve the seize targets before pushing them. */
  if (DECL_NESTING_LEVEL (val) >= current_module_nesting_level)
    {
      /* VAL was declared inside current module.  We need something
	 from the scope *enclosing* the current module, so search
	 through the shadowed declarations. */
      /* TODO - FIXME */
    }
#endif
  if (current_module && current_module->prev_module
      && DECL_NESTING_LEVEL (val)
      < current_module->prev_module->nesting_level)
    {

      /* It's declared in a scope enclosing the module enclosing
	 the current module.  Hence it's not visible. */
      return NULL_TREE;
    }
  while (TREE_CODE (val) == ALIAS_DECL)
    {
      val = DECL_ABSTRACT_ORIGIN (val);
      if (TREE_CODE (val) == ERROR_MARK)
	return NULL_TREE;
    }
  return val;
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *)0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
init_decl_processing ()
{
  int  wchar_type_size;
  tree bool_ftype_int_ptr_int;
  tree bool_ftype_int_ptr_int_int;
  tree bool_ftype_luns_ptr_luns_long;
  tree bool_ftype_luns_ptr_luns_long_ptr_int;
  tree bool_ftype_ptr_int_ptr_int;
  tree bool_ftype_ptr_int_ptr_int_int;
  tree find_bit_ftype;
  tree bool_ftype_ptr_ptr_int;
  tree bool_ftype_ptr_ptr_luns;
  tree bool_ftype_ptr_ptr_ptr_luns;
  tree endlink;
  tree int_ftype_int;
  tree int_ftype_int_int;
  tree int_ftype_int_ptr_int;
  tree int_ftype_ptr;
  tree int_ftype_ptr_int;
  tree int_ftype_ptr_int_int_ptr_int;
  tree int_ftype_ptr_luns_long_ptr_int;
  tree int_ftype_ptr_ptr_int;
  tree int_ftype_ptr_ptr_luns;
  tree long_ftype_ptr_luns;
  tree memcpy_ftype;
  tree memcmp_ftype;
  tree ptr_ftype_ptr_int_int;
  tree ptr_ftype_ptr_ptr_int;
  tree ptr_ftype_ptr_ptr_int_ptr_int;
  tree real_ftype_real;
  tree temp;
  tree void_ftype_cptr_cptr_int;
  tree void_ftype_long_int_ptr_int_ptr_int;
  tree void_ftype_ptr;
  tree void_ftype_ptr_int_int_int_int;
  tree void_ftype_ptr_int_ptr_int_int_int;
  tree void_ftype_ptr_int_ptr_int_ptr_int;
  tree void_ftype_ptr_luns_long_long_bool_ptr_int;
  tree void_ftype_ptr_luns_ptr_luns_luns_luns;
  tree void_ftype_ptr_ptr_ptr_int;
  tree void_ftype_ptr_ptr_ptr_luns;
  tree void_ftype_refptr_int_ptr_int;
  tree void_ftype_void;
  tree void_ftype_ptr_ptr_int;
  tree void_ftype_ptr_luns_luns_cptr_luns_luns_luns;
  tree ptr_ftype_luns_ptr_int;
  tree double_ftype_double;

  extern int set_alignment;

  /* allow 0-255 enums to occupy only a byte */
  flag_short_enums = 1;

  current_function_decl = NULL;

  set_alignment = BITS_PER_UNIT;

  ALL_POSTFIX = get_identifier ("*");
  string_index_type_dummy = get_identifier("%string-index%");

  var_length_id = get_identifier (VAR_LENGTH);
  var_data_id = get_identifier (VAR_DATA);

  /* This is the *C* int type. */
  integer_type_node = make_signed_type (INT_TYPE_SIZE);

  if (CHILL_INT_IS_SHORT)
    long_integer_type_node = integer_type_node;
  else
    long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);

  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);

  /* `unsigned long' is the standard type for sizeof.
     Note that stddef.h uses `unsigned long',
     and this must agree, even of long and int are the same size.  */
#ifndef SIZE_TYPE
    sizetype = long_unsigned_type_node;
#else
  {
    char *size_type_c_name = SIZE_TYPE;
    if (strncmp (size_type_c_name, "long long ", 10) == 0)
      sizetype = long_long_unsigned_type_node;
    else if (strncmp (size_type_c_name, "long ", 5) == 0)
      sizetype = long_unsigned_type_node;
    else
      sizetype = unsigned_type_node;
  }
#endif

  TREE_TYPE (TYPE_SIZE (integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_unsigned_type_node)) = sizetype;

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  intQI_type_node = make_signed_type (GET_MODE_BITSIZE (QImode));
  intHI_type_node = make_signed_type (GET_MODE_BITSIZE (HImode));
  intSI_type_node = make_signed_type (GET_MODE_BITSIZE (SImode));
  intDI_type_node = make_signed_type (GET_MODE_BITSIZE (DImode));
#if HOST_BITS_PER_WIDE_INT >= 64
  intTI_type_node = make_signed_type (GET_MODE_BITSIZE (TImode));
#endif
  unsigned_intQI_type_node = make_unsigned_type (GET_MODE_BITSIZE (QImode));
  unsigned_intHI_type_node = make_unsigned_type (GET_MODE_BITSIZE (HImode));
  unsigned_intSI_type_node = make_unsigned_type (GET_MODE_BITSIZE (SImode));
  unsigned_intDI_type_node = make_unsigned_type (GET_MODE_BITSIZE (DImode));
#if HOST_BITS_PER_WIDE_INT >= 64
  unsigned_intTI_type_node = make_unsigned_type (GET_MODE_BITSIZE (TImode));
#endif

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_FLOAT],
			float_type_node));
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  if (flag_short_double)
    TYPE_PRECISION (double_type_node) = FLOAT_TYPE_SIZE;
  else
    TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_DOUBLE],
			double_type_node));
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (long_double_type_node);

  complex_integer_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_integer_type_node) = integer_type_node;
  layout_type (complex_integer_type_node);

  complex_float_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_float_type_node) = float_type_node;
  layout_type (complex_float_type_node);

  complex_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_double_type_node) = double_type_node;
  layout_type (complex_double_type_node);

  complex_long_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_long_double_type_node) = long_double_type_node;
  layout_type (complex_long_double_type_node);

  integer_zero_node = build_int_2 (0, 0);
  TREE_TYPE (integer_zero_node) = integer_type_node;
  integer_one_node = build_int_2 (1, 0);
  TREE_TYPE (integer_one_node) = integer_type_node;
  integer_minus_one_node = build_int_2 (-1, -1);
  TREE_TYPE (integer_minus_one_node) = integer_type_node;

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  void_type_node = make_node (VOID_TYPE);
  pushdecl (build_decl (TYPE_DECL,
			ridpointers[(int) RID_VOID], void_type_node));
  layout_type (void_type_node);	/* Uses integer_zero_node */
  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));

  /* This is for wide string constants.  */
  wchar_type_node = short_unsigned_type_node;
  wchar_type_size = TYPE_PRECISION (wchar_type_node);
  signed_wchar_type_node = type_for_size (wchar_type_size, 0);
  unsigned_wchar_type_node = type_for_size (wchar_type_size, 1);

  default_function_type
    = build_function_type (integer_type_node, NULL_TREE);

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));

  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  boolean_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (boolean_type_node) = 1;
  fixup_unsigned_type (boolean_type_node);
  boolean_false_node = TYPE_MIN_VALUE (boolean_type_node);
  boolean_true_node = TYPE_MAX_VALUE (boolean_type_node);
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_BOOL],
                        boolean_type_node));

  /* TRUE and FALSE have the BOOL derived class */
  CH_DERIVED_FLAG (boolean_true_node) = 1;
  CH_DERIVED_FLAG (boolean_false_node) = 1;

  signed_boolean_type_node = make_node (BOOLEAN_TYPE);
  temp = build_int_2 (-1, -1);
  TREE_TYPE (temp) = signed_boolean_type_node;
  TYPE_MIN_VALUE (signed_boolean_type_node) = temp;
  temp = build_int_2 (0, 0);
  TREE_TYPE (temp) = signed_boolean_type_node;
  TYPE_MAX_VALUE (signed_boolean_type_node) = temp;
  layout_type (signed_boolean_type_node);

 
  bitstring_one_type_node = build_bitstring_type (integer_one_node);
  bit_zero_node = build (CONSTRUCTOR, bitstring_one_type_node, NULL_TREE,
			 NULL_TREE);
  bit_one_node = build (CONSTRUCTOR, bitstring_one_type_node, NULL_TREE,
			build_tree_list (NULL_TREE, integer_zero_node));

  char_type_node = make_node (CHAR_TYPE);
  TYPE_PRECISION (char_type_node) = CHAR_TYPE_SIZE;
  fixup_unsigned_type (char_type_node);
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_CHAR],
			char_type_node));

  if (CHILL_INT_IS_SHORT)
    {
      chill_integer_type_node = short_integer_type_node;
      chill_unsigned_type_node = short_unsigned_type_node;
    }
  else
    {
      chill_integer_type_node = integer_type_node;
      chill_unsigned_type_node = unsigned_type_node;
    }

  string_one_type_node = build_string_type (char_type_node, integer_one_node);

  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_BYTE],
                        signed_char_type_node));
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_UBYTE],
                        unsigned_char_type_node));

  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_INT],
                        chill_integer_type_node));

  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_UINT],
                        chill_unsigned_type_node));

  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_LONG],
                        long_integer_type_node));

  sizetype = long_integer_type_node;
#if 0
  ptrdiff_type_node
    = TREE_TYPE (IDENTIFIER_LOCAL_VALUE (get_identifier (PTRDIFF_TYPE)));
#endif
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_ULONG],
                        long_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_REAL],
                        float_type_node));
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_LONG_REAL],
                        double_type_node));
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int)RID_PTR],
                        ptr_type_node));

  IDENTIFIER_LOCAL_VALUE (ridpointers[(int)RID_TRUE]) =
    boolean_true_node;    
  IDENTIFIER_LOCAL_VALUE (ridpointers[(int)RID_FALSE]) =
    boolean_false_node;    
  IDENTIFIER_LOCAL_VALUE (ridpointers[(int)RID_NULL]) =
    null_pointer_node;    

  /* The second operand is set to non-NULL to distinguish
     (ELSE) from (*).  Used when writing grant files.  */
  case_else_node = build (RANGE_EXPR,
			  NULL_TREE, NULL_TREE, boolean_false_node);

  pushdecl (temp = build_decl (TYPE_DECL,
		     get_identifier ("__tmp_initializer"),
		       build_init_struct ()));
  DECL_SOURCE_LINE (temp) = 0;
  initializer_type = TREE_TYPE (temp);

  bcopy (chill_tree_code_type,
         tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE,
         (((int) LAST_CHILL_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
          * sizeof (char)));
  bcopy ((char *) chill_tree_code_length,
         (char *) (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE),
         (((int) LAST_CHILL_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
          * sizeof (int)));
  bcopy ((char *) chill_tree_code_name,
         (char *) (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE),
         (((int) LAST_CHILL_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
          * sizeof (char *)));
  boolean_code_name = (char **) xmalloc (sizeof (char *) * (int) LAST_CHILL_TREE_CODE);
  bzero ((char *) boolean_code_name, sizeof (char *) * (int) LAST_CHILL_TREE_CODE);

  boolean_code_name[EQ_EXPR] = "=";
  boolean_code_name[NE_EXPR] = "/=";
  boolean_code_name[LT_EXPR] = "<";
  boolean_code_name[GT_EXPR] = ">";
  boolean_code_name[LE_EXPR] = "<=";
  boolean_code_name[GE_EXPR] = ">=";
  boolean_code_name[SET_IN_EXPR] = "in";
  boolean_code_name[TRUTH_ANDIF_EXPR] = "andif";
  boolean_code_name[TRUTH_ORIF_EXPR] = "orif";
  boolean_code_name[TRUTH_AND_EXPR] = "and";
  boolean_code_name[TRUTH_OR_EXPR] = "or";
  boolean_code_name[BIT_AND_EXPR] = "and";
  boolean_code_name[BIT_IOR_EXPR] = "or";
  boolean_code_name[BIT_XOR_EXPR] = "xor";

  endlink = void_list_node;

  chill_predefined_function_type
    = build_function_type (integer_type_node,
       tree_cons (NULL_TREE, integer_type_node,
         endlink));

  bool_ftype_int_ptr_int
    = build_function_type (boolean_type_node,
          tree_cons (NULL_TREE, integer_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
 	          tree_cons (NULL_TREE, integer_type_node,
		      endlink))));
  bool_ftype_int_ptr_int
    = build_function_type (boolean_type_node,
          tree_cons (NULL_TREE, integer_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
 	          tree_cons (NULL_TREE, integer_type_node,
 	              tree_cons (NULL_TREE, integer_type_node,
			  endlink)))));
  bool_ftype_int_ptr_int_int
    = build_function_type (boolean_type_node,
          tree_cons (NULL_TREE, integer_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
 	              tree_cons (NULL_TREE, integer_type_node,
 	                  tree_cons (NULL_TREE, integer_type_node,
			      endlink)))));
  bool_ftype_luns_ptr_luns_long
    = build_function_type (boolean_type_node,
          tree_cons (NULL_TREE, long_unsigned_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
 	              tree_cons (NULL_TREE, long_unsigned_type_node,
 	                  tree_cons (NULL_TREE, long_integer_type_node,
			      endlink)))));
  bool_ftype_luns_ptr_luns_long_ptr_int
    = build_function_type (boolean_type_node,
          tree_cons (NULL_TREE, long_unsigned_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
 	              tree_cons (NULL_TREE, long_unsigned_type_node,
 	                  tree_cons (NULL_TREE, long_integer_type_node,
                              tree_cons (NULL_TREE, ptr_type_node,
                                  tree_cons (NULL_TREE, integer_type_node,
			              endlink)))))));
  bool_ftype_ptr_ptr_int
    = build_function_type (boolean_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
	          tree_cons (NULL_TREE, integer_type_node, 
		      endlink))));
  bool_ftype_ptr_ptr_luns
    = build_function_type (boolean_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, long_unsigned_type_node, 
		      endlink))));
  bool_ftype_ptr_ptr_ptr_luns
    = build_function_type (boolean_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
	              tree_cons (NULL_TREE, long_unsigned_type_node, 
		          endlink)))));
  bool_ftype_ptr_int_ptr_int
    = build_function_type (boolean_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
	          tree_cons (NULL_TREE, ptr_type_node, 
		      tree_cons (NULL_TREE, integer_type_node, 
			  endlink)))));
  bool_ftype_ptr_int_ptr_int_int
    = build_function_type (boolean_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
	          tree_cons (NULL_TREE, ptr_type_node, 
		      tree_cons (NULL_TREE, integer_type_node, 
		          tree_cons (NULL_TREE, integer_type_node, 
			             endlink))))));
  find_bit_ftype
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, long_unsigned_type_node,
		  tree_cons (NULL_TREE, integer_type_node,
			             endlink))));
  int_ftype_int
    = build_function_type (integer_type_node,
         tree_cons (NULL_TREE, integer_type_node, 
	     endlink));
  int_ftype_int_int
    = build_function_type (integer_type_node,
          tree_cons (NULL_TREE, integer_type_node,
	      tree_cons (NULL_TREE, integer_type_node, 
                  endlink)));
  int_ftype_int_ptr_int
    = build_function_type (integer_type_node,
	   tree_cons (NULL_TREE, integer_type_node,
 	       tree_cons (NULL_TREE, ptr_type_node,
 		   tree_cons (NULL_TREE, integer_type_node,
		       endlink))));
  int_ftype_ptr
    = build_function_type (integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node, 
              endlink));
  int_ftype_ptr_int
    = build_function_type (integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node, 
	      tree_cons (NULL_TREE, integer_type_node,
		  endlink)));

  long_ftype_ptr_luns
    = build_function_type (long_integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node, 
	      tree_cons (NULL_TREE, long_unsigned_type_node,
		  endlink)));

  int_ftype_ptr_int_int_ptr_int
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
 	      tree_cons (NULL_TREE, integer_type_node,
 		  tree_cons (NULL_TREE, integer_type_node,
		      tree_cons (NULL_TREE, ptr_type_node,
			  tree_cons (NULL_TREE, integer_type_node,
			      endlink))))));

  int_ftype_ptr_luns_long_ptr_int
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
 	      tree_cons (NULL_TREE, long_unsigned_type_node,
 		  tree_cons (NULL_TREE, long_integer_type_node,
		      tree_cons (NULL_TREE, ptr_type_node,
			  tree_cons (NULL_TREE, integer_type_node,
			      endlink))))));

  int_ftype_ptr_ptr_int
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
 	      tree_cons (NULL_TREE, ptr_type_node,
 		  tree_cons (NULL_TREE, integer_type_node,
		      endlink))));
  int_ftype_ptr_ptr_luns
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
 	      tree_cons (NULL_TREE, ptr_type_node,
 		  tree_cons (NULL_TREE, long_unsigned_type_node,
		      endlink))));
  memcpy_ftype	/* memcpy/memmove prototype */
    = build_function_type (ptr_type_node,
	tree_cons (NULL_TREE, ptr_type_node,
	  tree_cons (NULL_TREE, const_ptr_type_node,
	    tree_cons (NULL_TREE, sizetype,
	      endlink))));
  memcmp_ftype  /* memcmp prototype */
    = build_function_type (integer_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, sizetype,
              endlink)))); 

  ptr_ftype_ptr_int_int
    = build_function_type (ptr_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, integer_type_node, 
		      endlink))));
  ptr_ftype_ptr_ptr_int
    = build_function_type (ptr_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, integer_type_node, 
		      endlink))));
  ptr_ftype_ptr_ptr_int_ptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, integer_type_node,
		      tree_cons (NULL_TREE, ptr_type_node,
		          tree_cons (NULL_TREE, integer_type_node,
			      endlink))))));
  real_ftype_real
    = build_function_type (float_type_node,
	  tree_cons (NULL_TREE, float_type_node, 
              endlink));

  void_ftype_ptr
     = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, ptr_type_node, endlink));

  void_ftype_cptr_cptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, const_ptr_type_node,
	      tree_cons (NULL_TREE, const_ptr_type_node,
		  tree_cons (NULL_TREE, integer_type_node,
		      endlink))));

  void_ftype_refptr_int_ptr_int
    = build_function_type (void_type_node,
	      tree_cons (NULL_TREE, build_reference_type(ptr_type_node),
		tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		    tree_cons (NULL_TREE, integer_type_node,
		      endlink)))));

  void_ftype_ptr_ptr_ptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		          endlink)))));
  void_ftype_ptr_ptr_ptr_luns
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, ptr_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, long_unsigned_type_node,
		          endlink)))));
  void_ftype_ptr_int_int_int_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, integer_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		        tree_cons (NULL_TREE, integer_type_node,
		          endlink))))));
  void_ftype_ptr_luns_long_long_bool_ptr_int
    = build_function_type (void_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
	  tree_cons (NULL_TREE, long_unsigned_type_node,
	    tree_cons (NULL_TREE, long_integer_type_node,
	      tree_cons (NULL_TREE, long_integer_type_node,
		tree_cons (NULL_TREE, boolean_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		    tree_cons (NULL_TREE, integer_type_node,
		      endlink))))))));
  void_ftype_ptr_int_ptr_int_int_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		        tree_cons (NULL_TREE, integer_type_node,
		          tree_cons (NULL_TREE, integer_type_node,
		            endlink)))))));
  void_ftype_ptr_luns_ptr_luns_luns_luns
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, long_unsigned_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, long_unsigned_type_node,
		          tree_cons (NULL_TREE, long_unsigned_type_node,
		              tree_cons (NULL_TREE, long_unsigned_type_node,
		                  endlink)))))));
  void_ftype_ptr_int_ptr_int_ptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		        tree_cons (NULL_TREE, ptr_type_node,
		          tree_cons (NULL_TREE, integer_type_node,
		            endlink)))))));
  void_ftype_long_int_ptr_int_ptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, long_integer_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		  tree_cons (NULL_TREE, ptr_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		        tree_cons (NULL_TREE, ptr_type_node,
		          tree_cons (NULL_TREE, integer_type_node,
		            endlink)))))));
   void_ftype_void
     = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, void_type_node,
	       endlink));

  void_ftype_ptr_ptr_int
     = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, ptr_type_node,
	       tree_cons (NULL_TREE, ptr_type_node,
		   tree_cons (NULL_TREE, integer_type_node,
		       endlink))));

  void_ftype_ptr_luns_luns_cptr_luns_luns_luns
    = build_function_type (void_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
	  tree_cons (NULL_TREE, long_unsigned_type_node,
	    tree_cons (NULL_TREE, long_unsigned_type_node,
	      tree_cons (NULL_TREE, const_ptr_type_node,
	        tree_cons (NULL_TREE, long_unsigned_type_node,
	          tree_cons (NULL_TREE, long_unsigned_type_node,
	            tree_cons (NULL_TREE, long_unsigned_type_node,
			       endlink))))))));

  ptr_ftype_luns_ptr_int
    = build_function_type (ptr_type_node,
        tree_cons (NULL_TREE, long_unsigned_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, integer_type_node,
		       endlink))));

  double_ftype_double
    = build_function_type (double_type_node,
        tree_cons (NULL_TREE, double_type_node,
		   endlink));

/* These are compiler-internal function calls, not intended
   to be directly called by user code */
  builtin_function ("__allocate", ptr_ftype_luns_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("_allocate_global_memory", void_ftype_refptr_int_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("_allocate_memory", void_ftype_refptr_int_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__andpowerset", bool_ftype_ptr_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__bitsetpowerset", void_ftype_ptr_int_int_int_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__cardpowerset", long_ftype_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__cause_ex1", void_ftype_cptr_cptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__concatstring", ptr_ftype_ptr_ptr_int_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__continue", void_ftype_ptr_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__diffpowerset", void_ftype_ptr_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__eqpowerset", bool_ftype_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__ffsetclrpowerset", find_bit_ftype,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__flsetclrpowerset", find_bit_ftype,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__flsetpowerset", int_ftype_ptr_luns_long_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__ffsetpowerset", int_ftype_ptr_luns_long_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__inbitstring", bool_ftype_luns_ptr_luns_long_ptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__inpowerset", bool_ftype_luns_ptr_luns_long, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__lepowerset", bool_ftype_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__ltpowerset", bool_ftype_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  /* Currently under experimentation.  */
  builtin_function ("memmove", memcpy_ftype,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("memcmp", memcmp_ftype,
                    NOT_BUILT_IN, NULL_PTR);

  /* this comes from c-decl.c (init_decl_processing) */
  builtin_function ("__builtin_alloca",
		    build_function_type (ptr_type_node,
					 tree_cons (NULL_TREE,
						    sizetype,
						    endlink)),
		    BUILT_IN_ALLOCA, "alloca");

  builtin_function ("memset", ptr_ftype_ptr_int_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__notpowerset", bool_ftype_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__orpowerset", bool_ftype_ptr_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__psslice", void_ftype_ptr_int_ptr_int_int_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__pscpy", void_ftype_ptr_luns_luns_cptr_luns_luns_luns,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("_return_memory", void_ftype_ptr_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__setbitpowerset", void_ftype_ptr_luns_long_long_bool_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__terminate", void_ftype_ptr_ptr_int,
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__unhandled_ex", void_ftype_cptr_cptr_int, 
		    NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__xorpowerset", bool_ftype_ptr_ptr_ptr_luns, 
		    NOT_BUILT_IN, NULL_PTR);

  /* declare floating point functions */
  builtin_function ("__sin", double_ftype_double, NOT_BUILT_IN, "sin");
  builtin_function ("__cos", double_ftype_double, NOT_BUILT_IN, "cos");
  builtin_function ("__tan", double_ftype_double, NOT_BUILT_IN, "tan");
  builtin_function ("__asin", double_ftype_double, NOT_BUILT_IN, "asin");
  builtin_function ("__acos", double_ftype_double, NOT_BUILT_IN, "acos");
  builtin_function ("__atan", double_ftype_double, NOT_BUILT_IN, "atan");
  builtin_function ("__exp", double_ftype_double, NOT_BUILT_IN, "exp");
  builtin_function ("__log", double_ftype_double, NOT_BUILT_IN, "log");
  builtin_function ("__log10", double_ftype_double, NOT_BUILT_IN, "log10");
  builtin_function ("__sqrt", double_ftype_double, NOT_BUILT_IN, "sqrt");

  tasking_init ();
  timing_init ();
  inout_init ();

  /* These are predefined value builtin routine calls, built
     by the compiler, but over-ridable by user procedures of
     the same names.  Note the lack of a leading underscore. */
  builtin_function ((ignore_case || ! special_UC) ?  "abs" : "ABS",
		    chill_predefined_function_type,
		    BUILT_IN_CH_ABS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "abstime" : "ABSTIME",
		    chill_predefined_function_type,
		    BUILT_IN_ABSTIME, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "allocate" : "ALLOCATE",
		    chill_predefined_function_type,
		    BUILT_IN_ALLOCATE, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "allocate_memory" : "ALLOCATE_MEMORY",
		    chill_predefined_function_type,
		    BUILT_IN_ALLOCATE_MEMORY, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "addr" : "ADDR",
		    chill_predefined_function_type,
		    BUILT_IN_ADDR, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "allocate_global_memory" : "ALLOCATE_GLOBAL_MEMORY",
		    chill_predefined_function_type,
		    BUILT_IN_ALLOCATE_GLOBAL_MEMORY, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "arccos" : "ARCCOS",
		    chill_predefined_function_type,
		    BUILT_IN_ARCCOS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "arcsin" : "ARCSIN",
		    chill_predefined_function_type,
		    BUILT_IN_ARCSIN, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "arctan" : "ARCTAN",
		    chill_predefined_function_type,
		    BUILT_IN_ARCTAN, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "card" : "CARD",
		    chill_predefined_function_type,
		    BUILT_IN_CARD, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "cos" : "COS",
		    chill_predefined_function_type,
		    BUILT_IN_CH_COS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "days" : "DAYS",
		    chill_predefined_function_type,
		    BUILT_IN_DAYS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "descr" : "DESCR",
		    chill_predefined_function_type,
		    BUILT_IN_DESCR, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "getstack" : "GETSTACK",
		    chill_predefined_function_type,
		    BUILT_IN_GETSTACK, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "exp" : "EXP",
		    chill_predefined_function_type,
		    BUILT_IN_EXP, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "hours" : "HOURS",
		    chill_predefined_function_type,
		    BUILT_IN_HOURS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "inttime" : "INTTIME",
		    chill_predefined_function_type,
		    BUILT_IN_INTTIME, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "length" : "LENGTH",
		    chill_predefined_function_type,
		    BUILT_IN_LENGTH, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "log" : "LOG",
		    chill_predefined_function_type,
		    BUILT_IN_LOG, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "lower" : "LOWER",
		    chill_predefined_function_type,
		    BUILT_IN_LOWER, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "ln" : "LN",
		    chill_predefined_function_type,
		    BUILT_IN_LN, NULL_PTR);
  /* Note: these are *not* the C integer MAX and MIN.  They're
     for powerset arguments. */
  builtin_function ((ignore_case || ! special_UC) ?  "max" : "MAX",
		    chill_predefined_function_type,
		    BUILT_IN_MAX, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "millisecs" : "MILLISECS",
		    chill_predefined_function_type,
		    BUILT_IN_MILLISECS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "min" : "MIN",
		    chill_predefined_function_type,
		    BUILT_IN_MIN, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "minutes" : "MINUTES",
		    chill_predefined_function_type,
		    BUILT_IN_MINUTES, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "num" : "NUM",
		    chill_predefined_function_type,
		    BUILT_IN_NUM, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "pred" : "PRED",
		    chill_predefined_function_type,
		    BUILT_IN_PRED, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "return_memory" : "RETURN_MEMORY",
		    chill_predefined_function_type,
		    BUILT_IN_RETURN_MEMORY, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "secs" : "SECS",
		    chill_predefined_function_type,
		    BUILT_IN_SECS, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "sin" : "SIN",
		    chill_predefined_function_type,
		    BUILT_IN_CH_SIN, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "size" : "SIZE",
		    chill_predefined_function_type,
		    BUILT_IN_SIZE, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "sqrt" : "SQRT",
		    chill_predefined_function_type,
		    BUILT_IN_SQRT, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "succ" : "SUCC",
		    chill_predefined_function_type,
		    BUILT_IN_SUCC, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "tan" : "TAN",
		    chill_predefined_function_type,
		    BUILT_IN_TAN, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "terminate" : "TERMINATE",
		    chill_predefined_function_type,
		    BUILT_IN_TERMINATE, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ?  "upper" : "UPPER",
		    chill_predefined_function_type,
		    BUILT_IN_UPPER, NULL_PTR);

  build_chill_descr_type ();
  build_chill_inttime_type ();
  
  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  start_identifier_warnings ();

  pass = 1;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (name, type, function_code, library_name)
     char *name;
     tree type;
     enum built_in_function function_code;
     char *library_name;
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  /* If -traditional, permit redefining a builtin function any way you like.
     (Though really, if the program redefines these functions,
     it probably won't work right unless compiled with -fno-builtin.)  */
  if (flag_traditional && name[0] != '_')
    DECL_BUILT_IN_NONANSI (decl) = 1;
  if (library_name)
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_decl_rtl (decl, NULL_PTR, 1);
  pushdecl (decl);
  if (function_code != NOT_BUILT_IN)
    {
      DECL_BUILT_IN (decl) = 1;
      DECL_SET_FUNCTION_CODE (decl, function_code);
    }

  return decl;
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression. */

void
constant_expression_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_CONSTANT_OVERFLOW (value) && pedantic)
    pedwarn ("overflow in constant expression");
}


/* Finish processing of a declaration;
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (decl)
     tree decl;
{
  int was_incomplete = (DECL_SIZE (decl) == 0);
  int temporary = allocation_temporary_p ();

  /* Pop back to the obstack that is current for this binding level.
     This is because MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack.  But don't discard the
     temporary data yet.  */
  pop_obstacks ();
#if 0 /* pop_obstacks was near the end; this is what was here.  */
  if (current_scope == global_scope && temporary)
    end_temporary_allocation ();
#endif

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0
	  && TYPE_SIZE (TREE_TYPE (decl)) != 0)
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0 && TREE_CODE (TREE_TYPE (decl)) != ERROR_MARK)
	{
	  error_with_decl (decl, "storage size of `%s' isn't known");
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	}
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* The last argument (at_end) is set to 1 as a kludge to force
	 assemble_variable to be called. */
      if (TREE_CODE (TREE_TYPE (decl)) != ERROR_MARK)
	rest_of_decl_compilation (decl, (char*) 0, global_bindings_p (), 1);

      /* Compute the RTL of a decl if not yet set.
	 (For normal user variables, satisfy_decl sets it.) */
      if (! TREE_STATIC (decl) && ! DECL_EXTERNAL (decl))
	{
	  if (was_incomplete)
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	      expand_decl (decl);
	    }
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      rest_of_decl_compilation (decl, NULL_PTR,
				global_bindings_p (), 0);
    }

  /* ??? After 2.3, test (init != 0) instead of TREE_CODE.  */
  if (!(TREE_CODE (decl) == FUNCTION_DECL && DECL_INLINE (decl))
      && temporary && TREE_PERMANENT (decl))
    {
      /* We need to remember that this array HAD an initialization,
	 but discard the actual temporary nodes,
	 since we can't have a permanent node keep pointing to them.  */
      /* We make an exception for inline functions, since it's
	 normal for a local extern redeclaration of an inline function
	 to have a copy of the top-level decl's DECL_INLINE.  */
      if (DECL_INITIAL (decl) != 0)
	DECL_INITIAL (decl) = error_mark_node;
    }

#if 0
  /* Resume permanent allocation, if not within a function.  */
  /* The corresponding push_obstacks_nochange is in start_decl,
     and in push_parm_decl and in grokfield.  */
  pop_obstacks ();
#endif

  /* If we have gone back from temporary to permanent allocation,
     actually free the temporary space that we no longer need.  */
  if (temporary && !allocation_temporary_p ())
    permanent_allocation (0);

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_scope == global_scope)
    get_pending_sizes ();
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl ATTRIBUTE_UNUSED;
{
  /* There are no cleanups in C.  */
  return NULL_TREE;
}

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 1 if DO_DEFAULT).  */

int
complete_array_type (type, initial_value, do_default)
     tree type ATTRIBUTE_UNUSED, initial_value ATTRIBUTE_UNUSED;
     int do_default ATTRIBUTE_UNUSED;
{
  /* Only needed so we can link with ../c-typeck.c. */
  abort ();
}

/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.

   We also do a push_obstacks_nochange
   whose matching pop is in finish_struct.  */

tree
start_struct (code, name)
     enum chill_tree_code code;
     tree name ATTRIBUTE_UNUSED;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  register tree ref = 0;

  push_obstacks_nochange ();
  if (current_scope == global_scope)
    end_temporary_allocation ();

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  ref = make_node (code);
/*  pushtag (name, ref); */
  return ref;
}

#if 0
/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (x, y)
     tree *x, *y;
{
  return (long)DECL_NAME (*x) - (long)DECL_NAME (*y);
}
#endif
/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.

   We also do a pop_obstacks to match the push in start_struct.  */

tree
finish_struct (t, fieldlist)
     register tree t, fieldlist;
{
  register tree x;

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes.
     Set DECL_FIELD_SIZE to the specified size, or 0 if none specified.
     The specified size is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;
      DECL_FIELD_SIZE (x) = 0;
    }

  TYPE_FIELDS (t) = fieldlist;

  if (pass != 1)
    t = layout_chill_struct_type (t);

  /* The matching push is in start_struct.  */
  pop_obstacks ();

  return t;
}

/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (t)
     tree t;
{
  if (TYPE_SIZE (t) != 0)
    return;
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
}

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (name)
     tree name ATTRIBUTE_UNUSED;
{
  register tree enumtype;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

#if 0
  /* The corresponding pop_obstacks is in finish_enum.  */
  push_obstacks_nochange ();
  /* If these symbols and types are global, make them permanent.  */
  if (current_scope == global_scope)
    end_temporary_allocation ();
#endif

  enumtype = make_node (ENUMERAL_TYPE);
/*  pushtag (name, enumtype); */
  return enumtype;
}

/* Determine the precision this type needs.  */
unsigned
get_type_precision (minnode, maxnode)
     tree minnode, maxnode;
{
  unsigned precision = 0;

  if (TREE_INT_CST_HIGH (minnode) >= 0
      ? tree_int_cst_lt (TYPE_MAX_VALUE (unsigned_type_node), maxnode)
      : (tree_int_cst_lt (minnode, TYPE_MIN_VALUE (integer_type_node))
	 || tree_int_cst_lt (TYPE_MAX_VALUE (integer_type_node), maxnode)))
    precision = TYPE_PRECISION (long_long_integer_type_node);
  else
    {
      HOST_WIDE_INT maxvalue = TREE_INT_CST_LOW (maxnode);
      HOST_WIDE_INT minvalue = TREE_INT_CST_LOW (minnode);

      if (maxvalue > 0)
	precision = floor_log2 (maxvalue) + 1;
      if (minvalue < 0)
	{
	  /* Compute number of bits to represent magnitude of a negative value.
	     Add one to MINVALUE since range of negative numbers
	     includes the power of two.  */
	  unsigned negprecision = floor_log2 (-minvalue - 1) + 1;
	  if (negprecision > precision)
	    precision = negprecision;
	  precision += 1;	/* room for sign bit */
	}

      if (!precision)
	precision = 1;
    }
  return precision;
}

void
layout_enum (enumtype)
     tree enumtype;
{
  register tree pair, tem;
  tree minnode = 0, maxnode = 0;
  unsigned precision = 0;

  /* Do arithmetic using double integers, but don't use fold/build. */
  union tree_node enum_next_node;
  /* This is 1 plus the last enumerator constant value.  */
  tree enum_next_value = &enum_next_node;

  /* Nonzero means that there was overflow computing enum_next_value.  */
  int enum_overflow = 0;

  tree values = TYPE_VALUES (enumtype);

  if (TYPE_SIZE (enumtype) != NULL_TREE)
    return;

  /* Initialize enum_next_value to zero. */
  TREE_TYPE (enum_next_value) = integer_type_node;
  TREE_INT_CST_LOW (enum_next_value) = TREE_INT_CST_LOW (integer_zero_node);
  TREE_INT_CST_HIGH (enum_next_value) = TREE_INT_CST_HIGH (integer_zero_node);

  /* After processing and defining all the values of an enumeration type,
     install their decls in the enumeration type and finish it off.

     TYPE_VALUES currently contains a list of (purpose: NAME, value: DECL).
     This gets converted to a list of (purpose: NAME, value: VALUE). */


  /* For each enumerator, calculate values, if defaulted.
     Convert to correct type (the enumtype).
     Also, calculate the minimum and maximum values.  */

  for (pair = values; pair; pair = TREE_CHAIN (pair))
    {
      tree decl = TREE_VALUE (pair);
      tree value = DECL_INITIAL (decl);

      /* Remove no-op casts from the value.  */
      if (value != NULL_TREE)
	STRIP_TYPE_NOPS (value);

      if (value != NULL_TREE)
	{
	  if (TREE_CODE (value) == INTEGER_CST)
	    {
	      constant_expression_warning (value);
	      if (tree_int_cst_lt (value, integer_zero_node))
		{
		  error ("enumerator value for `%s' is less then 0",
			 IDENTIFIER_POINTER (DECL_NAME (decl)));
		  value = error_mark_node;
		}
	    }
	  else
	    {
	      error ("enumerator value for `%s' not integer constant",
		     IDENTIFIER_POINTER (DECL_NAME (decl)));
	      value = error_mark_node;
	    }
	}

      if (value != error_mark_node)
	{
	  if (value == NULL_TREE) /* Default based on previous value.  */
	    {
	      value = enum_next_value;
	      if (enum_overflow)
		error ("overflow in enumeration values");
	    }
	  value = build_int_2 (TREE_INT_CST_LOW (value),
			       TREE_INT_CST_HIGH (value));
	  TREE_TYPE (value) = enumtype;
	  DECL_INITIAL (decl) = value;
	  CH_DERIVED_FLAG (value) = 1;
      
	  if (pair == values)
	    minnode = maxnode = value;
	  else
	    {
	      if (tree_int_cst_lt (maxnode, value))
		maxnode = value;
	      if (tree_int_cst_lt (value, minnode))
		minnode = value;
	    }

	  /* Set basis for default for next value.  */
	  add_double (TREE_INT_CST_LOW (value), TREE_INT_CST_HIGH (value), 1, 0,
		      &TREE_INT_CST_LOW (enum_next_value),
		      &TREE_INT_CST_HIGH (enum_next_value));
	  enum_overflow = tree_int_cst_lt (enum_next_value, value);
	}
      else
	DECL_INITIAL (decl) = value; /* error_mark_node */
    }

  /* Fix all error_mark_nodes in enum. Increment maxnode and assign value.
     This is neccessary to make a duplicate value check in the enum */
  for (pair = values; pair; pair = TREE_CHAIN (pair))
    {
      tree decl = TREE_VALUE (pair);
      if (DECL_INITIAL (decl) == error_mark_node)
	{
	  tree value;
	  add_double (TREE_INT_CST_LOW (maxnode), TREE_INT_CST_HIGH (maxnode), 1, 0,
		      &TREE_INT_CST_LOW (enum_next_value),
		      &TREE_INT_CST_HIGH (enum_next_value));
	  value = build_int_2 (TREE_INT_CST_LOW (enum_next_value),
			       TREE_INT_CST_HIGH (enum_next_value));
	  TREE_TYPE (value) = enumtype;
	  CH_DERIVED_FLAG (value) = 1;
	  DECL_INITIAL (decl) = value;

	  maxnode = value;
	}
    }

  /* Now check if we have duplicate values within the enum */
  for (pair = values; pair; pair = TREE_CHAIN (pair))
    {
      tree succ;
      tree decl1 = TREE_VALUE (pair);
      tree val1 = DECL_INITIAL (decl1);

      for (succ = TREE_CHAIN (pair); succ; succ = TREE_CHAIN (succ))
	{
	  if (pair != succ)
	    {
	      tree decl2 = TREE_VALUE (succ);
	      tree val2 = DECL_INITIAL (decl2);
	      if (tree_int_cst_equal (val1, val2))
		error ("enumerators `%s' and `%s' have equal values",
		       IDENTIFIER_POINTER (DECL_NAME (decl1)),
		       IDENTIFIER_POINTER (DECL_NAME (decl2)));
	    }
	}
    }

  TYPE_MIN_VALUE (enumtype) = minnode;
  TYPE_MAX_VALUE (enumtype) = maxnode;

  precision = get_type_precision (minnode, maxnode);

  if (flag_short_enums || precision > TYPE_PRECISION (integer_type_node))
    /* Use the width of the narrowest normal C type which is wide enough.  */
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (type_for_size (precision, 1));
  else
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (integer_type_node);

  layout_type (enumtype);

#if 0
  /* An enum can have some negative values; then it is signed.  */
  TREE_UNSIGNED (enumtype) = ! tree_int_cst_lt (minnode, integer_zero_node);
#else
  /* Z200/1988 page 19 says:
     For each pair of integer literal expression e1, e2 in the set list NUM (e1)
     and NUM (e2) must deliver different non-negative results */
  TREE_UNSIGNED (enumtype) = 1;
#endif

  for (pair = values; pair; pair = TREE_CHAIN (pair))
    {
      tree decl = TREE_VALUE (pair);
      DECL_SIZE (decl) = TYPE_SIZE (enumtype);
      DECL_ALIGN (decl) = TYPE_ALIGN (enumtype);

      /* Set the TREE_VALUE to the name, rather than the decl,
	 since that is what the rest of the compiler expects. */
      TREE_VALUE (pair) = DECL_INITIAL (decl);
    }

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_MODE (tem) = TYPE_MODE (enumtype);
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
      TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
    }

#if 0
  /* This matches a push in start_enum.  */
  pop_obstacks ();
#endif
}

tree
finish_enum (enumtype, values)
     register tree enumtype, values;
{
  TYPE_VALUES (enumtype) = values = nreverse (values);

  /* If satisfy_decl is called on one of the enum CONST_DECLs,
     this will make sure that the enumtype gets laid out then. */
  for ( ; values; values = TREE_CHAIN (values))
    TREE_TYPE (TREE_VALUE (values)) = enumtype;

  return enumtype;
}


/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   Return a tree-list containing the CONST_DECL and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (name, value)
     tree name, value;
{
  register tree decl;
  int named = name != NULL_TREE;

  if (pass == 2)
    {
      if (name)
	(void) get_next_decl ();
      return NULL_TREE;
    }

  if (name == NULL_TREE)
    {
      static int unnamed_value_warned = 0;
      static int next_dummy_enum_value = 0;
      char buf[20];
      if (!unnamed_value_warned)
	{
	  unnamed_value_warned = 1;
	  warning ("undefined value in SET mode is obsolete and deprecated.");
	}
      sprintf (buf, "__star_%d", next_dummy_enum_value++);
      name = get_identifier (buf);
    }

  decl = build_decl (CONST_DECL, name, integer_type_node);
  CH_DECL_ENUM (decl) = 1;
  DECL_INITIAL (decl) = value;
  if (named)
    {
      if (pass == 0)
	{
	  push_obstacks_nochange ();
	  pushdecl (decl);
	  finish_decl (decl);
	}
      else
	save_decl (decl);
    }
  return build_tree_list (name, decl);

#if 0
  tree old_value = lookup_name_current_level (name);

  if (old_value != NULL_TREE
      && TREE_CODE (old_value)=!= CONST_DECL
      && (value == NULL_TREE || operand_equal_p (value, old_value, 1)))
    {
      if (value == NULL_TREE)
	{
	  if (TREE_CODE (old_value) == CONST_DECL)
	    value = DECL_INITIAL (old_value);
	  else
	    abort ();
	}
      return saveable_tree_cons (old_value, value, NULL_TREE);
    }
#endif
}

/* Record that this function is going to be a varargs function.
   This is called before store_parm_decls, which is too early
   to call mark_varargs directly.  */

void
c_mark_varargs ()
{
  c_function_varargs = 1;
}

/* Function needed for CHILL interface.  */
tree
get_parm_decls ()
{
  return current_function_parms;
}

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct c_function
{
  struct c_function *next;
  struct scope *scope;
  tree chill_result_decl;
  int result_never_set;
};

struct c_function *c_function_chain;

/* Save and reinitialize the variables
   used during compilation of a C function.  */

void
push_chill_function_context ()
{
  struct c_function *p
    = (struct c_function *) xmalloc (sizeof (struct c_function));

  push_function_context ();

  p->next = c_function_chain;
  c_function_chain = p;

  p->scope = current_scope;
  p->chill_result_decl = chill_result_decl;
  p->result_never_set = result_never_set;
}

/* Restore the variables used during compilation of a C function.  */

void
pop_chill_function_context ()
{
  struct c_function *p = c_function_chain;
#if 0
  tree link;
  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);
#endif

  pop_function_context ();

  c_function_chain = p->next;

  current_scope = p->scope;
  chill_result_decl = p->chill_result_decl;
  result_never_set = p->result_never_set;

  free (p);
}

/* Following from Jukka Virtanen's GNU Pascal */
/* To implement WITH statement:

   1) Call shadow_record_fields for each record_type element in the WITH
      element list. Each call creates a new binding level.
   
   2) construct a component_ref for EACH field in the record,
      and store it to the IDENTIFIER_LOCAL_VALUE after adding
      the old value to the shadow list

   3) let lookup_name do the rest

   4) pop all of the binding levels after the WITH statement ends.
      (restoring old local values) You have to keep track of the number
      of times you called it.
*/

/*
 * Save an arbitrary tree-expression as the IDENTIFIER_LOCAL_VALUE
 * of a name.  Save the name's previous value.  Check for name 
 * collisions with another value under the same name at the same
 * nesting level.  This is used to implement the DO WITH construct
 * and the temporary for the location iteration loop.
 */
void
save_expr_under_name (name, expr)
     tree name, expr;
{
  tree alias = build_alias_decl (NULL_TREE, NULL_TREE, name);

  DECL_ABSTRACT_ORIGIN (alias) = expr;
  TREE_CHAIN (alias) = NULL_TREE;
  pushdecllist (alias, 0);
}

void
do_based_decl (name, mode, base_var)
     tree name, mode, base_var;
{
  tree decl;
  if (pass == 1)
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      decl = make_node (BASED_DECL);
      DECL_NAME (decl) = name;
      TREE_TYPE (decl) = mode;
      DECL_ABSTRACT_ORIGIN (decl) = base_var;
      save_decl (decl);
      pop_obstacks ();
    }
  else
    {
      tree base_decl;
      decl = get_next_decl ();
      if (name != DECL_NAME (decl))
	abort();
      /* FIXME: This isn't a complete test */
      base_decl = lookup_name (base_var);
      if (base_decl == NULL_TREE)
	error ("BASE variable never declared");
      else if (TREE_CODE (base_decl) == FUNCTION_DECL)
	error ("cannot BASE a variable on a PROC/PROCESS name");
    }
}

void
do_based_decls (names, mode, base_var)
     tree names, mode, base_var;
{
  if (names == NULL_TREE || TREE_CODE (names) == TREE_LIST)
    {
      for (; names != NULL_TREE; names = TREE_CHAIN (names))
	do_based_decl (names, mode, base_var);
    }
  else if (TREE_CODE (names) != ERROR_MARK)
    do_based_decl (names, mode, base_var);
}

/*
 * Declare the fields so that lookup_name() will find them as
 * component refs for Pascal WITH or CHILL DO WITH.
 *
 * Proceeds to the inner layers of Pascal/CHILL variant record
 *
 * Internal routine of shadow_record_fields ()
 */
static void
handle_one_level (parent, fields)
     tree parent, fields;
{
  tree field, name;

  switch (TREE_CODE (TREE_TYPE (parent))) 
    {
    case RECORD_TYPE:
    case UNION_TYPE:
      for (field = fields; field; field = TREE_CHAIN (field)) {
	name = DECL_NAME (field);
	if (name == NULL_TREE || name == ELSE_VARIANT_NAME)
	  /* proceed through variant part */
	  handle_one_level (parent, TYPE_FIELDS (TREE_TYPE (field)));
	else 
	  {
	    tree field_alias = make_node (WITH_DECL);
	    DECL_NAME (field_alias) = name;
	    TREE_TYPE (field_alias) = TREE_TYPE (field);
	    DECL_ABSTRACT_ORIGIN (field_alias) = parent;
	    TREE_CHAIN (field_alias) = NULL_TREE;
	    pushdecllist (field_alias, 0);
	  }
      }
      break;
    default:
      error ("INTERNAL ERROR: handle_one_level is broken");
    }
}

/*
 * For each FIELD_DECL node in a RECORD_TYPE, we have to declare
 * a name so that lookup_name will find a COMPONENT_REF node
 * when the name is referenced. This happens in Pascal WITH statement.
 */
void
shadow_record_fields (struct_val)
     tree struct_val;
{
    if (pass == 1 || struct_val == NULL_TREE)
      return;

    handle_one_level (struct_val, TYPE_FIELDS (TREE_TYPE (struct_val)));
}

static char exception_prefix [] = "__Ex_";

tree
build_chill_exception_decl (name)
     char *name;
{
  tree decl, ex_name, ex_init, ex_type;
  int  name_len = strlen (name);
  char *ex_string = (char *)
          alloca (strlen (exception_prefix) + name_len + 1);

  sprintf(ex_string, "%s%s", exception_prefix, name);
  ex_name = get_identifier (ex_string);
  decl = IDENTIFIER_LOCAL_VALUE (ex_name);
  if (decl)
    return decl;

  /* finish_decl is too eager about switching back to the
     ambient context.  This decl's rtl must live in the permanent_obstack.  */
  push_obstacks (&permanent_obstack, &permanent_obstack);
  push_obstacks_nochange ();
  ex_type = build_array_type (char_type_node,
			      build_index_2_type (integer_zero_node,
						  build_int_2 (name_len, 0)));
  decl = build_lang_decl (VAR_DECL, ex_name, ex_type);
  ex_init = build_string (name_len, name);
  TREE_TYPE (ex_init) = ex_type;
  DECL_INITIAL (decl) = ex_init;
  TREE_READONLY (decl) = 1;
  TREE_STATIC (decl) = 1;
  pushdecl_top_level (decl);
  finish_decl (decl);
  pop_obstacks ();		/* Return to the ambient context.  */
  return decl;
}

extern tree      module_init_list;

/*
 * This function is called from the parser to preface the entire
 * compilation.  It contains module-level actions and reach-bound
 * initialization.
 */
void
start_outer_function ()
{
  start_chill_function (pass < 2 ? get_identifier ("_GLOBAL_")
			: DECL_NAME (global_function_decl),
			void_type_node, NULL_TREE, NULL_TREE, NULL_TREE);
  global_function_decl = current_function_decl;
  global_scope = current_scope;
  chill_at_module_level = 1;
}

/* This function finishes the global_function_decl, and if it is non-empty
 * (as indiacted by seen_action), adds it to module_init_list.
 */
void
finish_outer_function ()
{
  /* If there was module-level code in this module (not just function
     declarations), we allocate space for this module's init list entry,
     and fill in the module's function's address. */

  extern tree initializer_type;
  char *fname_str = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
  char *init_entry_name = (char *)xmalloc ((unsigned)(strlen (fname_str) + 20));
  tree  init_entry_id;
  tree  init_entry_decl;
  tree  initializer;
      
  finish_chill_function ();

  chill_at_module_level = 0;


  if (!seen_action)
    return;

  sprintf (init_entry_name, "__tmp_%s_init_entry",  fname_str);
  init_entry_id = get_identifier (init_entry_name);

  init_entry_decl = build1 (ADDR_EXPR,
			    TREE_TYPE (TYPE_FIELDS (initializer_type)),
			    global_function_decl);
  TREE_CONSTANT (init_entry_decl) = 1;
  initializer = build (CONSTRUCTOR, initializer_type, NULL_TREE,
		       tree_cons (NULL_TREE, init_entry_decl,
				  build_tree_list (NULL_TREE,
						   null_pointer_node)));
  TREE_CONSTANT (initializer) = 1;
  init_entry_decl
    = do_decl (init_entry_id, initializer_type, 1, 1, initializer, 0);
  DECL_SOURCE_LINE (init_entry_decl) = 0;
  if (pass == 1)
    /* tell chill_finish_compile that there's 
       module-level code to be processed. */
    module_init_list = integer_one_node;
  else if (build_constructor)
    module_init_list = tree_cons (global_function_decl,
				  init_entry_decl,
				  module_init_list);

  make_decl_rtl (global_function_decl, NULL, 0);
}
