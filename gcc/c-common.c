/* Subroutines shared by all languages that are variants of C.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "real.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "c-pragma.h"
#include "rtl.h"
#include "ggc.h"
#include "expr.h"
#include "c-common.h"
#include "diagnostic.h"
#include "tm_p.h"
#include "obstack.h"
#include "cpplib.h"
#include "target.h"
#include "langhooks.h"
#include "except.h"		/* For USING_SJLJ_EXCEPTIONS.  */
#include "tree-inline.h"
#include "c-tree.h"

cpp_reader *parse_in;		/* Declared in c-pragma.h.  */

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

#ifndef WCHAR_TYPE
#define WCHAR_TYPE "int"
#endif

/* WCHAR_TYPE gets overridden by -fshort-wchar.  */
#define MODIFIED_WCHAR_TYPE \
	(flag_short_wchar ? "short unsigned int" : WCHAR_TYPE)

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

#ifndef WINT_TYPE
#define WINT_TYPE "unsigned int"
#endif

#ifndef INTMAX_TYPE
#define INTMAX_TYPE ((INT_TYPE_SIZE == LONG_LONG_TYPE_SIZE)	\
		     ? "int"					\
		     : ((LONG_TYPE_SIZE == LONG_LONG_TYPE_SIZE)	\
			? "long int"				\
			: "long long int"))
#endif

#ifndef UINTMAX_TYPE
#define UINTMAX_TYPE ((INT_TYPE_SIZE == LONG_LONG_TYPE_SIZE)	\
		     ? "unsigned int"				\
		     : ((LONG_TYPE_SIZE == LONG_LONG_TYPE_SIZE)	\
			? "long unsigned int"			\
			: "long long unsigned int"))
#endif

#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX ""
#endif

/* The variant of the C language being processed.  */

enum c_language_kind c_language;

/* The following symbols are subsumed in the c_global_trees array, and
   listed here individually for documentation purposes.

   INTEGER_TYPE and REAL_TYPE nodes for the standard data types.

	tree short_integer_type_node;
	tree long_integer_type_node;
	tree long_long_integer_type_node;

	tree short_unsigned_type_node;
	tree long_unsigned_type_node;
	tree long_long_unsigned_type_node;

	tree boolean_type_node;
	tree boolean_false_node;
	tree boolean_true_node;

	tree ptrdiff_type_node;

	tree unsigned_char_type_node;
	tree signed_char_type_node;
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
	tree intTI_type_node;

	tree unsigned_intQI_type_node;
	tree unsigned_intHI_type_node;
	tree unsigned_intSI_type_node;
	tree unsigned_intDI_type_node;
	tree unsigned_intTI_type_node;

	tree widest_integer_literal_type_node;
	tree widest_unsigned_literal_type_node;

   Nodes for types `void *' and `const void *'.

	tree ptr_type_node, const_ptr_type_node;

   Nodes for types `char *' and `const char *'.

	tree string_type_node, const_string_type_node;

   Type `char[SOMENUMBER]'.
   Used when an array of char is needed and the size is irrelevant.

	tree char_array_type_node;

   Type `int[SOMENUMBER]' or something like it.
   Used when an array of int needed and the size is irrelevant.

	tree int_array_type_node;

   Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.

	tree wchar_array_type_node;

   Type `int ()' -- used for implicit declaration of functions.

	tree default_function_type;

   A VOID_TYPE node, packaged in a TREE_LIST.

	tree void_list_node;

  The lazily created VAR_DECLs for __FUNCTION__, __PRETTY_FUNCTION__,
  and __func__. (C doesn't generate __FUNCTION__ and__PRETTY_FUNCTION__
  VAR_DECLS, but C++ does.)

	tree function_name_decl_node;
	tree pretty_function_name_decl_node;
	tree c99_function_name_decl_node;

  Stack of nested function name VAR_DECLs.
  
	tree saved_function_name_decls;

*/

tree c_global_trees[CTI_MAX];

/* Switches common to the C front ends.  */

/* Nonzero if prepreprocessing only.  */
int flag_preprocess_only;

/* Nonzero if an ISO standard was selected.  It rejects macros in the
   user's namespace.  */
int flag_iso;

/* Nonzero whenever Objective-C functionality is being used.  */
int flag_objc;

/* Nonzero if -undef was given.  It suppresses target built-in macros
   and assertions.  */
int flag_undef;

/* Nonzero means don't recognize the non-ANSI builtin functions.  */

int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

int flag_no_nonansi_builtin;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double;

/* Nonzero means give `wchar_t' the same size as `short'.  */

int flag_short_wchar;

/* Nonzero means allow Microsoft extensions without warnings or errors.  */
int flag_ms_extensions;

/* Nonzero means don't recognize the keyword `asm'.  */

int flag_no_asm;

/* Nonzero means give string constants the type `const char *', as mandated
   by the standard.  */

int flag_const_strings;

/* Nonzero means `$' can be in an identifier.  */

#ifndef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 1
#endif
int dollars_in_ident = DOLLARS_IN_IDENTIFIERS;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */

int flag_signed_bitfields = 1;
int explicit_flag_signed_bitfields;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

int warn_cast_qual;

/* Warn about functions which might be candidates for format attributes.  */

int warn_missing_format_attribute;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

int warn_pointer_arith;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */

int warn_missing_prototypes;

/* Warn if adding () is suggested.  */

int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */

int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.
   If -1, neither -Wsign-compare nor -Wno-sign-compare has been specified.  */

int warn_sign_compare;

/* Nonzero means warn about usage of long long when `-pedantic'.  */

int warn_long_long = 1;

/* Nonzero means warn about deprecated conversion from string constant to
   `char *'.  */

int warn_write_strings;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

int warn_redundant_decls;

/* Warn about testing equality of floating point numbers.  */

int warn_float_equal;

/* Warn about a subscript that has type char.  */

int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */

int warn_conversion;

/* Warn about #pragma directives that are not recognized.  */      

int warn_unknown_pragmas; /* Tri state variable.  */  

/* Warn about format/argument anomalies in calls to formatted I/O functions
   (*printf, *scanf, strftime, strfmon, etc.).  */

int warn_format;

/* Warn about Y2K problems with strftime formats.  */

int warn_format_y2k;

/* Warn about excess arguments to formats.  */

int warn_format_extra_args;

/* Warn about zero-length formats.  */

int warn_format_zero_length;

/* Warn about non-literal format arguments.  */

int warn_format_nonliteral;

/* Warn about possible security problems with calls to format functions.  */

int warn_format_security;


/* C/ObjC language option variables.  */


/* Nonzero means message about use of implicit function declarations;
 1 means warning; 2 means error.  */

int mesg_implicit_function_declaration = -1;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.  */

int flag_cond_mismatch;

/* Nonzero means enable C89 Amendment 1 features.  */

int flag_isoc94;

/* Nonzero means use the ISO C99 dialect of C.  */

int flag_isoc99;

/* Nonzero means that we have builtin functions, and main is an int */

int flag_hosted = 1;

/* Nonzero means add default format_arg attributes for functions not
   in ISO C.  */

int flag_noniso_default_format_attributes = 1;

/* Nonzero means warn when casting a function call to a type that does
   not match the return type (e.g. (float)sqrt() or (anything*)malloc()
   when there is no previous declaration of sqrt or malloc.  */

int warn_bad_function_cast;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */

int warn_traditional;

/* Nonzero means warn for non-prototype function decls
   or non-prototyped defs without previous prototype.  */

int warn_strict_prototypes;

/* Nonzero means warn for any global function def
   without separate previous decl.  */

int warn_missing_declarations;

/* Nonzero means warn about declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */

int warn_nested_externs;

/* Warn if main is suspicious.  */

int warn_main;

/* Nonzero means warn about possible violations of sequence point rules.  */

int warn_sequence_point;

/* Nonzero means to warn about compile-time division by zero.  */
int warn_div_by_zero = 1;

/* Nonzero means warn about use of implicit int.  */

int warn_implicit_int;

/* Warn about NULL being passed to argument slots marked as requiring
   non-NULL.  */ 
      
int warn_nonnull;


/* ObjC language option variables.  */


/* Open and close the file for outputting class declarations, if
   requested (ObjC).  */

int flag_gen_declaration;

/* Generate code for GNU or NeXT runtime environment.  */

#ifdef NEXT_OBJC_RUNTIME
int flag_next_runtime = 1;
#else
int flag_next_runtime = 0;
#endif

/* Tells the compiler that this is a special run.  Do not perform any
   compiling, instead we are to test some platform dependent features
   and output a C header file with appropriate definitions.  */

int print_struct_values;

/* ???.  Undocumented.  */

const char *constant_string_class_name;

/* Warn if multiple methods are seen for the same selector, but with
   different argument types.  Performs the check on the whole selector
   table at the end of compilation.  */

int warn_selector;

/* Warn if a @selector() is found, and no method with that selector
   has been previously declared.  The check is done on each
   @selector() as soon as it is found - so it warns about forward
   declarations.  */

int warn_undeclared_selector;

/* Warn if methods required by a protocol are not implemented in the 
   class adopting it.  When turned off, methods inherited to that
   class are also considered implemented.  */

int warn_protocol = 1;


/* C++ language option variables.  */


/* Nonzero means don't recognize any extension keywords.  */

int flag_no_gnu_keywords;

/* Nonzero means do emit exported implementations of functions even if
   they can be inlined.  */

int flag_implement_inlines = 1;

/* Nonzero means do emit exported implementations of templates, instead of
   multiple static copies in each file that needs a definition.  */

int flag_external_templates;

/* Nonzero means that the decision to emit or not emit the implementation of a
   template depends on where the template is instantiated, rather than where
   it is defined.  */

int flag_alt_external_templates;

/* Nonzero means that implicit instantiations will be emitted if needed.  */

int flag_implicit_templates = 1;

/* Nonzero means that implicit instantiations of inline templates will be
   emitted if needed, even if instantiations of non-inline templates
   aren't.  */

int flag_implicit_inline_templates = 1;

/* Nonzero means generate separate instantiation control files and
   juggle them at link time.  */

int flag_use_repository;

/* Nonzero if we want to issue diagnostics that the standard says are not
   required.  */

int flag_optional_diags = 1;

/* Nonzero means we should attempt to elide constructors when possible.  */

int flag_elide_constructors = 1;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

int flag_default_inline = 1;

/* Controls whether compiler generates 'type descriptor' that give
   run-time type information.  */

int flag_rtti = 1;

/* Nonzero if we want to conserve space in the .o files.  We do this
   by putting uninitialized data and runtime initialized data into
   .common instead of .data at the expense of not flagging multiple
   definitions.  */

int flag_conserve_space;

/* Nonzero if we want to obey access control semantics.  */

int flag_access_control = 1;

/* Nonzero if we want to check the return value of new and avoid calling
   constructors if it is a null pointer.  */

int flag_check_new;

/* Nonzero if we want the new ISO rules for pushing a new scope for `for'
   initialization variables.
   0: Old rules, set by -fno-for-scope.
   2: New ISO rules, set by -ffor-scope.
   1: Try to implement new ISO rules, but with backup compatibility
   (and warnings).  This is the default, for now.  */

int flag_new_for_scope = 1;

/* Nonzero if we want to emit defined symbols with common-like linkage as
   weak symbols where possible, in order to conform to C++ semantics.
   Otherwise, emit them as local symbols.  */

int flag_weak = 1;

/* Nonzero to use __cxa_atexit, rather than atexit, to register
   destructors for local statics and global objects.  */

int flag_use_cxa_atexit = DEFAULT_USE_CXA_ATEXIT;

/* Nonzero means output .vtable_{entry,inherit} for use in doing vtable gc.  */

int flag_vtable_gc;

/* Nonzero means make the default pedwarns warnings instead of errors.
   The value of this flag is ignored if -pedantic is specified.  */

int flag_permissive;

/* Nonzero means to implement standard semantics for exception
   specifications, calling unexpected if an exception is thrown that
   doesn't match the specification.  Zero means to treat them as
   assertions and optimize accordingly, but not check them.  */

int flag_enforce_eh_specs = 1;

/*  The version of the C++ ABI in use.  The following values are
    allowed:

    0: The version of the ABI believed most conformant with the 
       C++ ABI specification.  This ABI may change as bugs are
       discovered and fixed.  Therefore, 0 will not necessarily
       indicate the same ABI in different versions of G++.

    1: The version of the ABI first used in G++ 3.2.

    Additional positive integers will be assigned as new versions of
    the ABI become the default version of the ABI.  */

int flag_abi_version = 1;

/* Nonzero means warn about things that will change when compiling
   with an ABI-compliant compiler.  */

int warn_abi = 0;

/* Nonzero means warn about implicit declarations.  */

int warn_implicit = 1;

/* Nonzero means warn when all ctors or dtors are private, and the class
   has no friends.  */

int warn_ctor_dtor_privacy = 1;

/* Nonzero means warn in function declared in derived class has the
   same name as a virtual in the base class, but fails to match the
   type signature of any virtual function in the base class.  */

int warn_overloaded_virtual;

/* Nonzero means warn when declaring a class that has a non virtual
   destructor, when it really ought to have a virtual one.  */

int warn_nonvdtor;

/* Nonzero means warn when the compiler will reorder code.  */

int warn_reorder;

/* Nonzero means warn when synthesis behavior differs from Cfront's.  */

int warn_synth;

/* Nonzero means warn when we convert a pointer to member function
   into a pointer to (void or function).  */

int warn_pmf2ptr = 1;

/* Nonzero means warn about violation of some Effective C++ style rules.  */

int warn_ecpp;

/* Nonzero means warn where overload resolution chooses a promotion from
   unsigned to signed over a conversion to an unsigned of the same size.  */

int warn_sign_promo;

/* Nonzero means warn when an old-style cast is used.  */

int warn_old_style_cast;

/* Nonzero means warn when non-templatized friend functions are
   declared within a template */

int warn_nontemplate_friend = 1;

/* Nonzero means complain about deprecated features.  */

int warn_deprecated = 1;

/* Maximum template instantiation depth.  This limit is rather
   arbitrary, but it exists to limit the time it takes to notice
   infinite template instantiations.  */

int max_tinst_depth = 500;



/* The elements of `ridpointers' are identifier nodes for the reserved
   type names and storage classes.  It is indexed by a RID_... value.  */
tree *ridpointers;

tree (*make_fname_decl)                PARAMS ((tree, int));

/* If non-NULL, the address of a language-specific function that
   returns 1 for language-specific statement codes.  */
int (*lang_statement_code_p)           PARAMS ((enum tree_code));

/* If non-NULL, the address of a language-specific function that takes
   any action required right before expand_function_end is called.  */
void (*lang_expand_function_end)       PARAMS ((void));

/* Nonzero means the expression being parsed will never be evaluated.
   This is a count, since unevaluated expressions can nest.  */
int skip_evaluation;

/* Information about how a function name is generated.  */
struct fname_var_t
{
  tree *const decl;	/* pointer to the VAR_DECL.  */
  const unsigned rid;	/* RID number for the identifier.  */
  const int pretty;	/* How pretty is it? */
};

/* The three ways of getting then name of the current function.  */

const struct fname_var_t fname_vars[] =
{
  /* C99 compliant __func__, must be first.  */
  {&c99_function_name_decl_node, RID_C99_FUNCTION_NAME, 0},
  /* GCC __FUNCTION__ compliant.  */
  {&function_name_decl_node, RID_FUNCTION_NAME, 0},
  /* GCC __PRETTY_FUNCTION__ compliant.  */
  {&pretty_function_name_decl_node, RID_PRETTY_FUNCTION_NAME, 1},
  {NULL, 0, 0},
};

static int constant_fits_type_p		PARAMS ((tree, tree));

/* Keep a stack of if statements.  We record the number of compound
   statements seen up to the if keyword, as well as the line number
   and file of the if.  If a potentially ambiguous else is seen, that
   fact is recorded; the warning is issued when we can be sure that
   the enclosing if statement does not have an else branch.  */
typedef struct
{
  int compstmt_count;
  int line;
  const char *file;
  int needs_warning;
  tree if_stmt;
} if_elt;

static if_elt *if_stack;

/* Amount of space in the if statement stack.  */
static int if_stack_space = 0;

/* Stack pointer.  */
static int if_stack_pointer = 0;

static tree handle_packed_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_nocommon_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_common_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_noreturn_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_noinline_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_always_inline_attribute PARAMS ((tree *, tree, tree, int,
						    bool *));
static tree handle_used_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_unused_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_const_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_transparent_union_attribute PARAMS ((tree *, tree, tree,
							int, bool *));
static tree handle_constructor_attribute PARAMS ((tree *, tree, tree, int,
						  bool *));
static tree handle_destructor_attribute PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_mode_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_section_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_aligned_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_weak_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_alias_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_visibility_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_tls_model_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_no_instrument_function_attribute PARAMS ((tree *, tree,
							     tree, int,
							     bool *));
static tree handle_malloc_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_no_limit_stack_attribute PARAMS ((tree *, tree, tree, int,
						     bool *));
static tree handle_pure_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_deprecated_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_vector_size_attribute PARAMS ((tree *, tree, tree, int,
						  bool *));
static tree handle_nonnull_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_nothrow_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree handle_cleanup_attribute	PARAMS ((tree *, tree, tree, int,
						 bool *));
static tree vector_size_helper PARAMS ((tree, tree));

static void check_function_nonnull	PARAMS ((tree, tree));
static void check_nonnull_arg		PARAMS ((void *, tree,
						 unsigned HOST_WIDE_INT));
static bool nonnull_check_p		PARAMS ((tree, unsigned HOST_WIDE_INT));
static bool get_nonnull_operand		PARAMS ((tree,
						 unsigned HOST_WIDE_INT *));
void builtin_define_std PARAMS ((const char *));
static void builtin_define_with_value PARAMS ((const char *, const char *,
                                               int));
static void builtin_define_with_int_value PARAMS ((const char *,
						   HOST_WIDE_INT));
static void builtin_define_with_hex_fp_value PARAMS ((const char *, tree,
						      int, const char *,
						      const char *));
static void builtin_define_type_max PARAMS ((const char *, tree, int));
static void builtin_define_type_precision PARAMS ((const char *, tree));
static void builtin_define_float_constants PARAMS ((const char *,
						    const char *, tree));

/* Table of machine-independent attributes common to all C-like languages.  */
const struct attribute_spec c_common_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "packed",                 0, 0, false, false, false,
      			      handle_packed_attribute },
  { "nocommon",               0, 0, true,  false, false,
			      handle_nocommon_attribute },
  { "common",                 0, 0, true,  false, false,
			      handle_common_attribute },
  /* FIXME: logically, noreturn attributes should be listed as
     "false, true, true" and apply to function types.  But implementing this
     would require all the places in the compiler that use TREE_THIS_VOLATILE
     on a decl to identify non-returning functions to be located and fixed
     to check the function type instead.  */
  { "noreturn",               0, 0, true,  false, false,
			      handle_noreturn_attribute },
  { "volatile",               0, 0, true,  false, false,
			      handle_noreturn_attribute },
  { "noinline",               0, 0, true,  false, false,
			      handle_noinline_attribute },
  { "always_inline",          0, 0, true,  false, false,
			      handle_always_inline_attribute },
  { "used",                   0, 0, true,  false, false,
			      handle_used_attribute },
  { "unused",                 0, 0, false, false, false,
			      handle_unused_attribute },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",                  0, 0, true,  false, false,
			      handle_const_attribute },
  { "transparent_union",      0, 0, false, false, false,
			      handle_transparent_union_attribute },
  { "constructor",            0, 0, true,  false, false,
			      handle_constructor_attribute },
  { "destructor",             0, 0, true,  false, false,
			      handle_destructor_attribute },
  { "mode",                   1, 1, false,  true, false,
			      handle_mode_attribute },
  { "section",                1, 1, true,  false, false,
			      handle_section_attribute },
  { "aligned",                0, 1, false, false, false,
			      handle_aligned_attribute },
  { "weak",                   0, 0, true,  false, false,
			      handle_weak_attribute },
  { "alias",                  1, 1, true,  false, false,
			      handle_alias_attribute },
  { "no_instrument_function", 0, 0, true,  false, false,
			      handle_no_instrument_function_attribute },
  { "malloc",                 0, 0, true,  false, false,
			      handle_malloc_attribute },
  { "no_stack_limit",         0, 0, true,  false, false,
			      handle_no_limit_stack_attribute },
  { "pure",                   0, 0, true,  false, false,
			      handle_pure_attribute },
  { "deprecated",             0, 0, false, false, false,
			      handle_deprecated_attribute },
  { "vector_size",	      1, 1, false, true, false,
			      handle_vector_size_attribute },
  { "visibility",	      1, 1, true,  false, false,
			      handle_visibility_attribute },
  { "tls_model",	      1, 1, true,  false, false,
			      handle_tls_model_attribute },
  { "nonnull",                0, -1, false, true, true,
			      handle_nonnull_attribute },
  { "nothrow",                0, 0, true,  false, false,
			      handle_nothrow_attribute },
  { "may_alias",	      0, 0, false, true, false, NULL },
  { "cleanup",		      1, 1, true, false, false,
			      handle_cleanup_attribute },
  { NULL,                     0, 0, false, false, false, NULL }
};

/* Give the specifications for the format attributes, used by C and all
   descendents.  */

const struct attribute_spec c_common_format_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "format",                 3, 3, false, true,  true,
			      handle_format_attribute },
  { "format_arg",             1, 1, false, true,  true,
			      handle_format_arg_attribute },
  { NULL,                     0, 0, false, false, false, NULL }
};

/* Record the start of an if-then, and record the start of it
   for ambiguous else detection.

   COND is the condition for the if-then statement.

   IF_STMT is the statement node that has already been created for
   this if-then statement.  It is created before parsing the
   condition to keep line number information accurate.  */

void
c_expand_start_cond (cond, compstmt_count, if_stmt)
     tree cond;
     int compstmt_count;
     tree if_stmt;
{
  /* Make sure there is enough space on the stack.  */
  if (if_stack_space == 0)
    {
      if_stack_space = 10;
      if_stack = (if_elt *) xmalloc (10 * sizeof (if_elt));
    }
  else if (if_stack_space == if_stack_pointer)
    {
      if_stack_space += 10;
      if_stack = (if_elt *) xrealloc (if_stack, if_stack_space * sizeof (if_elt));
    }

  IF_COND (if_stmt) = cond;
  add_stmt (if_stmt);

  /* Record this if statement.  */
  if_stack[if_stack_pointer].compstmt_count = compstmt_count;
  if_stack[if_stack_pointer].file = input_filename;
  if_stack[if_stack_pointer].line = lineno;
  if_stack[if_stack_pointer].needs_warning = 0;
  if_stack[if_stack_pointer].if_stmt = if_stmt;
  if_stack_pointer++;
}

/* Called after the then-clause for an if-statement is processed.  */

void
c_finish_then ()
{
  tree if_stmt = if_stack[if_stack_pointer - 1].if_stmt;
  RECHAIN_STMTS (if_stmt, THEN_CLAUSE (if_stmt));
}

/* Record the end of an if-then.  Optionally warn if a nested
   if statement had an ambiguous else clause.  */

void
c_expand_end_cond ()
{
  if_stack_pointer--;
  if (if_stack[if_stack_pointer].needs_warning)
    warning_with_file_and_line (if_stack[if_stack_pointer].file,
				if_stack[if_stack_pointer].line,
				"suggest explicit braces to avoid ambiguous `else'");
  last_expr_type = NULL_TREE;
}

/* Called between the then-clause and the else-clause
   of an if-then-else.  */

void
c_expand_start_else ()
{
  /* An ambiguous else warning must be generated for the enclosing if
     statement, unless we see an else branch for that one, too.  */
  if (warn_parentheses
      && if_stack_pointer > 1
      && (if_stack[if_stack_pointer - 1].compstmt_count
	  == if_stack[if_stack_pointer - 2].compstmt_count))
    if_stack[if_stack_pointer - 2].needs_warning = 1;

  /* Even if a nested if statement had an else branch, it can't be
     ambiguous if this one also has an else.  So don't warn in that
     case.  Also don't warn for any if statements nested in this else.  */
  if_stack[if_stack_pointer - 1].needs_warning = 0;
  if_stack[if_stack_pointer - 1].compstmt_count--;
}

/* Called after the else-clause for an if-statement is processed.  */

void
c_finish_else ()
{
  tree if_stmt = if_stack[if_stack_pointer - 1].if_stmt;
  RECHAIN_STMTS (if_stmt, ELSE_CLAUSE (if_stmt));
}

/* Begin an if-statement.  Returns a newly created IF_STMT if
   appropriate.

   Unlike the C++ front-end, we do not call add_stmt here; it is
   probably safe to do so, but I am not very familiar with this
   code so I am being extra careful not to change its behavior
   beyond what is strictly necessary for correctness.  */

tree
c_begin_if_stmt ()
{
  tree r;
  r = build_stmt (IF_STMT, NULL_TREE, NULL_TREE, NULL_TREE);
  return r;
}

/* Begin a while statement.  Returns a newly created WHILE_STMT if
   appropriate.

   Unlike the C++ front-end, we do not call add_stmt here; it is
   probably safe to do so, but I am not very familiar with this
   code so I am being extra careful not to change its behavior
   beyond what is strictly necessary for correctness.  */

tree
c_begin_while_stmt ()
{
  tree r;
  r = build_stmt (WHILE_STMT, NULL_TREE, NULL_TREE);
  return r;
}

void
c_finish_while_stmt_cond (cond, while_stmt)
     tree while_stmt;
     tree cond;
{
  WHILE_COND (while_stmt) = cond;
}

/* Push current bindings for the function name VAR_DECLS.  */

void
start_fname_decls ()
{
  unsigned ix;
  tree saved = NULL_TREE;
  
  for (ix = 0; fname_vars[ix].decl; ix++)
    {
      tree decl = *fname_vars[ix].decl;

      if (decl)
	{
	  saved = tree_cons (decl, build_int_2 (ix, 0), saved);
	  *fname_vars[ix].decl = NULL_TREE;
	}
    }
  if (saved || saved_function_name_decls)
    /* Normally they'll have been NULL, so only push if we've got a
       stack, or they are non-NULL.  */
    saved_function_name_decls = tree_cons (saved, NULL_TREE,
					   saved_function_name_decls);
}

/* Finish up the current bindings, adding them into the
   current function's statement tree. This is done by wrapping the
   function's body in a COMPOUND_STMT containing these decls too. This
   must be done _before_ finish_stmt_tree is called. If there is no
   current function, we must be at file scope and no statements are
   involved. Pop the previous bindings.  */

void
finish_fname_decls ()
{
  unsigned ix;
  tree body = NULL_TREE;
  tree stack = saved_function_name_decls;

  for (; stack && TREE_VALUE (stack); stack = TREE_CHAIN (stack))
    body = chainon (TREE_VALUE (stack), body);
  
  if (body)
    {
      /* They were called into existence, so add to statement tree.  */
      body = chainon (body,
		      TREE_CHAIN (DECL_SAVED_TREE (current_function_decl)));
      body = build_stmt (COMPOUND_STMT, body);
      
      COMPOUND_STMT_NO_SCOPE (body) = 1;
      TREE_CHAIN (DECL_SAVED_TREE (current_function_decl)) = body;
    }
  
  for (ix = 0; fname_vars[ix].decl; ix++)
    *fname_vars[ix].decl = NULL_TREE;
  
  if (stack)
    {
      /* We had saved values, restore them.  */
      tree saved;

      for (saved = TREE_PURPOSE (stack); saved; saved = TREE_CHAIN (saved))
	{
	  tree decl = TREE_PURPOSE (saved);
	  unsigned ix = TREE_INT_CST_LOW (TREE_VALUE (saved));
	  
	  *fname_vars[ix].decl = decl;
	}
      stack = TREE_CHAIN (stack);
    }
  saved_function_name_decls = stack;
}

/* Return the text name of the current function, suitable prettified
   by PRETTY_P.  */

const char *
fname_as_string (pretty_p)
     int pretty_p;
{
  const char *name = NULL;
  
  if (pretty_p)
    name = (current_function_decl
	    ? (*lang_hooks.decl_printable_name) (current_function_decl, 2)
	    : "top level");
  else if (current_function_decl && DECL_NAME (current_function_decl))
    name = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
  else
    name = "";
  return name;
}

/* Return the text name of the current function, formatted as
   required by the supplied RID value.  */

const char *
fname_string (rid)
     unsigned rid;
{
  unsigned ix;
  
  for (ix = 0; fname_vars[ix].decl; ix++)
    if (fname_vars[ix].rid == rid)
      break;
  return fname_as_string (fname_vars[ix].pretty);
}

/* Return the VAR_DECL for a const char array naming the current
   function. If the VAR_DECL has not yet been created, create it
   now. RID indicates how it should be formatted and IDENTIFIER_NODE
   ID is its name (unfortunately C and C++ hold the RID values of
   keywords in different places, so we can't derive RID from ID in
   this language independent code.  */

tree
fname_decl (rid, id)
     unsigned rid;
     tree id;
{
  unsigned ix;
  tree decl = NULL_TREE;

  for (ix = 0; fname_vars[ix].decl; ix++)
    if (fname_vars[ix].rid == rid)
      break;

  decl = *fname_vars[ix].decl;
  if (!decl)
    {
      tree saved_last_tree = last_tree;
      /* If a tree is built here, it would normally have the lineno of
	 the current statement.  Later this tree will be moved to the
	 beginning of the function and this line number will be wrong.
	 To avoid this problem set the lineno to 0 here; that prevents
	 it from appearing in the RTL.  */
      int saved_lineno = lineno;
      lineno = 0;
      
      decl = (*make_fname_decl) (id, fname_vars[ix].pretty);
      if (last_tree != saved_last_tree)
	{
	  /* We created some statement tree for the decl. This belongs
	     at the start of the function, so remove it now and reinsert
	     it after the function is complete.  */
	  tree stmts = TREE_CHAIN (saved_last_tree);

	  TREE_CHAIN (saved_last_tree) = NULL_TREE;
	  last_tree = saved_last_tree;
	  saved_function_name_decls = tree_cons (decl, stmts,
						 saved_function_name_decls);
	}
      *fname_vars[ix].decl = decl;
      lineno = saved_lineno;
    }
  if (!ix && !current_function_decl)
    pedwarn_with_decl (decl, "`%s' is not defined outside of function scope");
  
  return decl;
}

/* Given a STRING_CST, give it a suitable array-of-chars data type.  */

tree
fix_string_type (value)
      tree value;
{
  const int wchar_bytes = TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT;
  const int wide_flag = TREE_TYPE (value) == wchar_array_type_node;
  const int nchars_max = flag_isoc99 ? 4095 : 509;
  int length = TREE_STRING_LENGTH (value);
  int nchars;

  /* Compute the number of elements, for the array type.  */
  nchars = wide_flag ? length / wchar_bytes : length;

  if (pedantic && nchars - 1 > nchars_max && c_language == clk_c)
    pedwarn ("string length `%d' is greater than the length `%d' ISO C%d compilers are required to support",
	     nchars - 1, nchars_max, flag_isoc99 ? 99 : 89);

  /* Create the array type for the string constant.
     -Wwrite-strings says make the string constant an array of const char
     so that copying it to a non-const pointer will get a warning.
     For C++, this is the standard behavior.  */
  if (flag_const_strings && ! flag_writable_strings)
    {
      tree elements
	= build_type_variant (wide_flag ? wchar_type_node : char_type_node,
			      1, 0);
      TREE_TYPE (value)
	= build_array_type (elements,
			    build_index_type (build_int_2 (nchars - 1, 0)));
    }
  else
    TREE_TYPE (value)
      = build_array_type (wide_flag ? wchar_type_node : char_type_node,
			  build_index_type (build_int_2 (nchars - 1, 0)));

  TREE_CONSTANT (value) = 1;
  TREE_READONLY (value) = ! flag_writable_strings;
  TREE_STATIC (value) = 1;
  return value;
}

/* Given a VARRAY of STRING_CST nodes, concatenate them into one
   STRING_CST.  */

tree
combine_strings (strings)
     varray_type strings;
{
  const int wchar_bytes = TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT;
  const int nstrings = VARRAY_ACTIVE_SIZE (strings);
  tree value, t;
  int length = 1;
  int wide_length = 0;
  int wide_flag = 0;
  int i;
  char *p, *q;

  /* Don't include the \0 at the end of each substring.  Count wide
     strings and ordinary strings separately.  */
  for (i = 0; i < nstrings; ++i)
    {
      t = VARRAY_TREE (strings, i);

      if (TREE_TYPE (t) == wchar_array_type_node)
	{
	  wide_length += TREE_STRING_LENGTH (t) - wchar_bytes;
	  wide_flag = 1;
	}
      else
	{
	  length += (TREE_STRING_LENGTH (t) - 1);
	  if (C_ARTIFICIAL_STRING_P (t) && !in_system_header)
	    warning ("concatenation of string literals with __FUNCTION__ is deprecated"); 
	}
    }

  /* If anything is wide, the non-wides will be converted,
     which makes them take more space.  */
  if (wide_flag)
    length = length * wchar_bytes + wide_length;

  p = xmalloc (length);

  /* Copy the individual strings into the new combined string.
     If the combined string is wide, convert the chars to ints
     for any individual strings that are not wide.  */

  q = p;
  for (i = 0; i < nstrings; ++i)
    {
      int len, this_wide;

      t = VARRAY_TREE (strings, i);
      this_wide = TREE_TYPE (t) == wchar_array_type_node;
      len = TREE_STRING_LENGTH (t) - (this_wide ? wchar_bytes : 1);
      if (this_wide == wide_flag)
	{
	  memcpy (q, TREE_STRING_POINTER (t), len);
	  q += len;
	}
      else
	{
	  const int nzeros = (TYPE_PRECISION (wchar_type_node)
			      / BITS_PER_UNIT) - 1;
	  int j, k;

	  if (BYTES_BIG_ENDIAN)
	    {
	      for (k = 0; k < len; k++)
		{
		  for (j = 0; j < nzeros; j++)
		    *q++ = 0;
		  *q++ = TREE_STRING_POINTER (t)[k];
		}
	    }
	  else
	    {
	      for (k = 0; k < len; k++)
		{
		  *q++ = TREE_STRING_POINTER (t)[k];
		  for (j = 0; j < nzeros; j++)
		    *q++ = 0;
		}
	    }
	}
    }

  /* Nul terminate the string.  */
  if (wide_flag)
    {
      for (i = 0; i < wchar_bytes; i++)
	*q++ = 0;
    }
  else
    *q = 0;

  value = build_string (length, p);
  free (p);

  if (wide_flag)
    TREE_TYPE (value) = wchar_array_type_node;
  else
    TREE_TYPE (value) = char_array_type_node;

  return value;
}

static int is_valid_printf_arglist PARAMS ((tree));
static rtx c_expand_builtin PARAMS ((tree, rtx, enum machine_mode, enum expand_modifier));
static rtx c_expand_builtin_printf PARAMS ((tree, rtx, enum machine_mode,
					    enum expand_modifier, int, int));
static rtx c_expand_builtin_fprintf PARAMS ((tree, rtx, enum machine_mode,
					     enum expand_modifier, int, int));

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == VECTOR_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_CONSTANT_OVERFLOW (value) && pedantic)
    pedwarn ("overflow in constant expression");
}

/* Print a warning if an expression had overflow in folding.
   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) might be a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts.  */

void
overflow_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST
       || (TREE_CODE (value) == COMPLEX_CST
	   && TREE_CODE (TREE_REALPART (value)) == INTEGER_CST))
      && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("integer overflow in expression");
    }
  else if ((TREE_CODE (value) == REAL_CST
	    || (TREE_CODE (value) == COMPLEX_CST
		&& TREE_CODE (TREE_REALPART (value)) == REAL_CST))
	   && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("floating point overflow in expression");
    }
  else if (TREE_CODE (value) == VECTOR_CST && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("vector overflow in expression");
    }
}

/* Print a warning if a large constant is truncated to unsigned,
   or if -Wconversion is used and a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */

void
unsigned_conversion_warning (result, operand)
     tree result, operand;
{
  tree type = TREE_TYPE (result);

  if (TREE_CODE (operand) == INTEGER_CST
      && TREE_CODE (type) == INTEGER_TYPE
      && TREE_UNSIGNED (type)
      && skip_evaluation == 0
      && !int_fits_type_p (operand, type))
    {
      if (!int_fits_type_p (operand, c_common_signed_type (type)))
	/* This detects cases like converting -129 or 256 to unsigned char.  */
	warning ("large integer implicitly truncated to unsigned type");
      else if (warn_conversion)
	warning ("negative integer implicitly converted to unsigned type");
    }
}

/* Nonzero if constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

static int
constant_fits_type_p (c, type)
     tree c, type;
{
  if (TREE_CODE (c) == INTEGER_CST)
    return int_fits_type_p (c, type);

  c = convert (type, c);
  return !TREE_OVERFLOW (c);
}     

/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (type, expr)
     tree type, expr;
{
  tree t = convert (type, expr);
  if (TREE_CODE (t) == INTEGER_CST)
    {
      if (TREE_OVERFLOW (t))
	{
	  TREE_OVERFLOW (t) = 0;

	  /* Do not diagnose overflow in a constant expression merely
	     because a conversion overflowed.  */
	  TREE_CONSTANT_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (expr);

	  /* No warning for converting 0x80000000 to int.  */
	  if (!(TREE_UNSIGNED (type) < TREE_UNSIGNED (TREE_TYPE (expr))
		&& TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
		&& TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))
	    /* If EXPR fits in the unsigned version of TYPE,
	       don't warn unless pedantic.  */
	    if ((pedantic
		 || TREE_UNSIGNED (type)
		 || ! constant_fits_type_p (expr,
					    c_common_unsigned_type (type)))
	        && skip_evaluation == 0)
	      warning ("overflow in implicit constant conversion");
	}
      else
	unsigned_conversion_warning (t, expr);
    }
  return t;
}

/* A node in a list that describes references to variables (EXPR), which are
   either read accesses if WRITER is zero, or write accesses, in which case
   WRITER is the parent of EXPR.  */
struct tlist
{
  struct tlist *next;
  tree expr, writer;
};

/* Used to implement a cache the results of a call to verify_tree.  We only
   use this for SAVE_EXPRs.  */
struct tlist_cache
{
  struct tlist_cache *next;
  struct tlist *cache_before_sp;
  struct tlist *cache_after_sp;
  tree expr;
};

/* Obstack to use when allocating tlist structures, and corresponding
   firstobj.  */
static struct obstack tlist_obstack;
static char *tlist_firstobj = 0;

/* Keep track of the identifiers we've warned about, so we can avoid duplicate
   warnings.  */
static struct tlist *warned_ids;
/* SAVE_EXPRs need special treatment.  We process them only once and then
   cache the results.  */
static struct tlist_cache *save_expr_cache;

static void add_tlist PARAMS ((struct tlist **, struct tlist *, tree, int));
static void merge_tlist PARAMS ((struct tlist **, struct tlist *, int));
static void verify_tree PARAMS ((tree, struct tlist **, struct tlist **, tree));
static int warning_candidate_p PARAMS ((tree));
static void warn_for_collisions PARAMS ((struct tlist *));
static void warn_for_collisions_1 PARAMS ((tree, tree, struct tlist *, int));
static struct tlist *new_tlist PARAMS ((struct tlist *, tree, tree));
static void verify_sequence_points PARAMS ((tree));

/* Create a new struct tlist and fill in its fields.  */
static struct tlist *
new_tlist (next, t, writer)
     struct tlist *next;
     tree t;
     tree writer;
{
  struct tlist *l;
  l = (struct tlist *) obstack_alloc (&tlist_obstack, sizeof *l);
  l->next = next;
  l->expr = t;
  l->writer = writer;
  return l;
}

/* Add duplicates of the nodes found in ADD to the list *TO.  If EXCLUDE_WRITER
   is nonnull, we ignore any node we find which has a writer equal to it.  */

static void
add_tlist (to, add, exclude_writer, copy)
     struct tlist **to;
     struct tlist *add;
     tree exclude_writer;
     int copy;
{
  while (add)
    {
      struct tlist *next = add->next;
      if (! copy)
	add->next = *to;
      if (! exclude_writer || add->writer != exclude_writer)
	*to = copy ? new_tlist (*to, add->expr, add->writer) : add;
      add = next;
    }
}

/* Merge the nodes of ADD into TO.  This merging process is done so that for
   each variable that already exists in TO, no new node is added; however if
   there is a write access recorded in ADD, and an occurrence on TO is only
   a read access, then the occurrence in TO will be modified to record the
   write.  */

static void
merge_tlist (to, add, copy)
     struct tlist **to;
     struct tlist *add;
     int copy;
{
  struct tlist **end = to;

  while (*end)
    end = &(*end)->next;

  while (add)
    {
      int found = 0;
      struct tlist *tmp2;
      struct tlist *next = add->next;

      for (tmp2 = *to; tmp2; tmp2 = tmp2->next)
	if (tmp2->expr == add->expr)
	  {
	    found = 1;
	    if (! tmp2->writer)
	      tmp2->writer = add->writer;
	  }
      if (! found)
	{
	  *end = copy ? add : new_tlist (NULL, add->expr, add->writer);
	  end = &(*end)->next;
	  *end = 0;
	}
      add = next;
    }
}

/* WRITTEN is a variable, WRITER is its parent.  Warn if any of the variable
   references in list LIST conflict with it, excluding reads if ONLY writers
   is nonzero.  */

static void
warn_for_collisions_1 (written, writer, list, only_writes)
     tree written, writer;
     struct tlist *list;
     int only_writes;
{
  struct tlist *tmp;

  /* Avoid duplicate warnings.  */
  for (tmp = warned_ids; tmp; tmp = tmp->next)
    if (tmp->expr == written)
      return;

  while (list)
    {
      if (list->expr == written
	  && list->writer != writer
	  && (! only_writes || list->writer))
	{
	  warned_ids = new_tlist (warned_ids, written, NULL_TREE);
	  warning ("operation on `%s' may be undefined",
		   IDENTIFIER_POINTER (DECL_NAME (list->expr)));
	}
      list = list->next;
    }
}

/* Given a list LIST of references to variables, find whether any of these
   can cause conflicts due to missing sequence points.  */

static void
warn_for_collisions (list)
     struct tlist *list;
{
  struct tlist *tmp;
  
  for (tmp = list; tmp; tmp = tmp->next)
    {
      if (tmp->writer)
	warn_for_collisions_1 (tmp->expr, tmp->writer, list, 0);
    }
}

/* Return nonzero if X is a tree that can be verified by the sequence point
   warnings.  */
static int
warning_candidate_p (x)
     tree x;
{
  return TREE_CODE (x) == VAR_DECL || TREE_CODE (x) == PARM_DECL;
}

/* Walk the tree X, and record accesses to variables.  If X is written by the
   parent tree, WRITER is the parent.
   We store accesses in one of the two lists: PBEFORE_SP, and PNO_SP.  If this
   expression or its only operand forces a sequence point, then everything up
   to the sequence point is stored in PBEFORE_SP.  Everything else gets stored
   in PNO_SP.
   Once we return, we will have emitted warnings if any subexpression before
   such a sequence point could be undefined.  On a higher level, however, the
   sequence point may not be relevant, and we'll merge the two lists.

   Example: (b++, a) + b;
   The call that processes the COMPOUND_EXPR will store the increment of B
   in PBEFORE_SP, and the use of A in PNO_SP.  The higher-level call that
   processes the PLUS_EXPR will need to merge the two lists so that
   eventually, all accesses end up on the same list (and we'll warn about the
   unordered subexpressions b++ and b.

   A note on merging.  If we modify the former example so that our expression
   becomes
     (b++, b) + a
   care must be taken not simply to add all three expressions into the final
   PNO_SP list.  The function merge_tlist takes care of that by merging the
   before-SP list of the COMPOUND_EXPR into its after-SP list in a special
   way, so that no more than one access to B is recorded.  */

static void
verify_tree (x, pbefore_sp, pno_sp, writer)
     tree x;
     struct tlist **pbefore_sp, **pno_sp;
     tree writer;
{
  struct tlist *tmp_before, *tmp_nosp, *tmp_list2, *tmp_list3;
  enum tree_code code;
  char class;

  /* X may be NULL if it is the operand of an empty statement expression
     ({ }).  */
  if (x == NULL)
    return;

 restart:
  code = TREE_CODE (x);
  class = TREE_CODE_CLASS (code);

  if (warning_candidate_p (x))
    {
      *pno_sp = new_tlist (*pno_sp, x, writer);
      return;
    }

  switch (code)
    {
    case CONSTRUCTOR:
      return;

    case COMPOUND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      tmp_before = tmp_nosp = tmp_list3 = 0;
      verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_nosp, NULL_TREE);
      warn_for_collisions (tmp_nosp);
      merge_tlist (pbefore_sp, tmp_before, 0);
      merge_tlist (pbefore_sp, tmp_nosp, 0);
      verify_tree (TREE_OPERAND (x, 1), &tmp_list3, pno_sp, NULL_TREE);
      merge_tlist (pbefore_sp, tmp_list3, 0);
      return;

    case COND_EXPR:
      tmp_before = tmp_list2 = 0;
      verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_list2, NULL_TREE);
      warn_for_collisions (tmp_list2);
      merge_tlist (pbefore_sp, tmp_before, 0);
      merge_tlist (pbefore_sp, tmp_list2, 1);

      tmp_list3 = tmp_nosp = 0;
      verify_tree (TREE_OPERAND (x, 1), &tmp_list3, &tmp_nosp, NULL_TREE);
      warn_for_collisions (tmp_nosp);
      merge_tlist (pbefore_sp, tmp_list3, 0);

      tmp_list3 = tmp_list2 = 0;
      verify_tree (TREE_OPERAND (x, 2), &tmp_list3, &tmp_list2, NULL_TREE);
      warn_for_collisions (tmp_list2);
      merge_tlist (pbefore_sp, tmp_list3, 0);
      /* Rather than add both tmp_nosp and tmp_list2, we have to merge the
	 two first, to avoid warning for (a ? b++ : b++).  */
      merge_tlist (&tmp_nosp, tmp_list2, 0);
      add_tlist (pno_sp, tmp_nosp, NULL_TREE, 0);
      return;

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      verify_tree (TREE_OPERAND (x, 0), pno_sp, pno_sp, x);
      return;

    case MODIFY_EXPR:
      tmp_before = tmp_nosp = tmp_list3 = 0;
      verify_tree (TREE_OPERAND (x, 1), &tmp_before, &tmp_nosp, NULL_TREE);
      verify_tree (TREE_OPERAND (x, 0), &tmp_list3, &tmp_list3, x);
      /* Expressions inside the LHS are not ordered wrt. the sequence points
	 in the RHS.  Example:
	   *a = (a++, 2)
	 Despite the fact that the modification of "a" is in the before_sp
	 list (tmp_before), it conflicts with the use of "a" in the LHS.
	 We can handle this by adding the contents of tmp_list3
	 to those of tmp_before, and redoing the collision warnings for that
	 list.  */
      add_tlist (&tmp_before, tmp_list3, x, 1);
      warn_for_collisions (tmp_before);
      /* Exclude the LHS itself here; we first have to merge it into the
	 tmp_nosp list.  This is done to avoid warning for "a = a"; if we
	 didn't exclude the LHS, we'd get it twice, once as a read and once
	 as a write.  */
      add_tlist (pno_sp, tmp_list3, x, 0);
      warn_for_collisions_1 (TREE_OPERAND (x, 0), x, tmp_nosp, 1);

      merge_tlist (pbefore_sp, tmp_before, 0);
      if (warning_candidate_p (TREE_OPERAND (x, 0)))
	merge_tlist (&tmp_nosp, new_tlist (NULL, TREE_OPERAND (x, 0), x), 0);
      add_tlist (pno_sp, tmp_nosp, NULL_TREE, 1);
      return;

    case CALL_EXPR:
      /* We need to warn about conflicts among arguments and conflicts between
	 args and the function address.  Side effects of the function address,
	 however, are not ordered by the sequence point of the call.  */
      tmp_before = tmp_nosp = tmp_list2 = tmp_list3 = 0;
      verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_nosp, NULL_TREE);
      if (TREE_OPERAND (x, 1))
	verify_tree (TREE_OPERAND (x, 1), &tmp_list2, &tmp_list3, NULL_TREE);
      merge_tlist (&tmp_list3, tmp_list2, 0);
      add_tlist (&tmp_before, tmp_list3, NULL_TREE, 0);
      add_tlist (&tmp_before, tmp_nosp, NULL_TREE, 0);
      warn_for_collisions (tmp_before);
      add_tlist (pbefore_sp, tmp_before, NULL_TREE, 0);
      return;

    case TREE_LIST:
      /* Scan all the list, e.g. indices of multi dimensional array.  */
      while (x)
	{
	  tmp_before = tmp_nosp = 0;
	  verify_tree (TREE_VALUE (x), &tmp_before, &tmp_nosp, NULL_TREE);
	  merge_tlist (&tmp_nosp, tmp_before, 0);
	  add_tlist (pno_sp, tmp_nosp, NULL_TREE, 0);
	  x = TREE_CHAIN (x);
	}
      return;

    case SAVE_EXPR:
      {
	struct tlist_cache *t;
	for (t = save_expr_cache; t; t = t->next)
	  if (t->expr == x)
	    break;

	if (! t)
	  {
	    t = (struct tlist_cache *) obstack_alloc (&tlist_obstack,
						      sizeof *t);
	    t->next = save_expr_cache;
	    t->expr = x;
	    save_expr_cache = t;

	    tmp_before = tmp_nosp = 0;
	    verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_nosp, NULL_TREE);
	    warn_for_collisions (tmp_nosp);

	    tmp_list3 = 0;
	    while (tmp_nosp)
	      {
		struct tlist *t = tmp_nosp;
		tmp_nosp = t->next;
		merge_tlist (&tmp_list3, t, 0);
	      }
	    t->cache_before_sp = tmp_before;
	    t->cache_after_sp = tmp_list3;
	  }
	merge_tlist (pbefore_sp, t->cache_before_sp, 1);
	add_tlist (pno_sp, t->cache_after_sp, NULL_TREE, 1);
	return;
      }
    default:
      break;
    }

  if (class == '1')
    {
      if (first_rtl_op (code) == 0)
	return;
      x = TREE_OPERAND (x, 0);
      writer = 0;
      goto restart;
    }

  switch (class)
    {
    case 'r':
    case '<':
    case '2':
    case 'b':
    case 'e':
    case 's':
    case 'x':
      {
	int lp;
	int max = first_rtl_op (TREE_CODE (x));
	for (lp = 0; lp < max; lp++)
	  {
	    tmp_before = tmp_nosp = 0;
	    verify_tree (TREE_OPERAND (x, lp), &tmp_before, &tmp_nosp, NULL_TREE);
	    merge_tlist (&tmp_nosp, tmp_before, 0);
	    add_tlist (pno_sp, tmp_nosp, NULL_TREE, 0);
	  }
	break;
      }
    }
}

/* Try to warn for undefined behavior in EXPR due to missing sequence
   points.  */

static void
verify_sequence_points (expr)
     tree expr;
{
  struct tlist *before_sp = 0, *after_sp = 0;

  warned_ids = 0;
  save_expr_cache = 0;
  if (tlist_firstobj == 0)
    {
      gcc_obstack_init (&tlist_obstack);
      tlist_firstobj = obstack_alloc (&tlist_obstack, 0);
    }

  verify_tree (expr, &before_sp, &after_sp, 0);
  warn_for_collisions (after_sp);
  obstack_free (&tlist_obstack, tlist_firstobj);
}

tree
c_expand_expr_stmt (expr)
     tree expr;
{
  /* Do default conversion if safe and possibly important,
     in case within ({...}).  */
  if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
       && (flag_isoc99 || lvalue_p (expr)))
      || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
    expr = default_conversion (expr);

  if (warn_sequence_point)
    verify_sequence_points (expr);

  if (TREE_TYPE (expr) != error_mark_node
      && !COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (expr))
      && TREE_CODE (TREE_TYPE (expr)) != ARRAY_TYPE)
    error ("expression statement has incomplete type");

  last_expr_type = TREE_TYPE (expr); 
  return add_stmt (build_stmt (EXPR_STMT, expr));
}

/* Validate the expression after `case' and apply default promotions.  */

tree
check_case_value (value)
     tree value;
{
  if (value == NULL_TREE)
    return value;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (value);
  /* In C++, the following is allowed:

       const int i = 3;
       switch (...) { case i: ... }

     So, we try to reduce the VALUE to a constant that way.  */
  if (c_language == clk_cplusplus)
    {
      value = decl_constant_value (value);
      STRIP_TYPE_NOPS (value);
      value = fold (value);
    }

  if (TREE_CODE (value) != INTEGER_CST
      && value != error_mark_node)
    {
      error ("case label does not reduce to an integer constant");
      value = error_mark_node;
    }
  else
    /* Promote char or short to int.  */
    value = default_conversion (value);

  constant_expression_warning (value);

  return value;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
c_common_type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  return 0;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
c_common_type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;

  if (mode == QImode)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == HImode)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == SImode)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == DImode)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  switch (mode)
    {
    case V16QImode:
      return unsignedp ? unsigned_V16QI_type_node : V16QI_type_node;
    case V8HImode:
      return unsignedp ? unsigned_V8HI_type_node : V8HI_type_node;
    case V4SImode:
      return unsignedp ? unsigned_V4SI_type_node : V4SI_type_node;
    case V2DImode:
      return unsignedp ? unsigned_V2DI_type_node : V2DI_type_node;
    case V2SImode:
      return unsignedp ? unsigned_V2SI_type_node : V2SI_type_node;
    case V2HImode:
      return unsignedp ? unsigned_V2HI_type_node : V2HI_type_node;
    case V4HImode:
      return unsignedp ? unsigned_V4HI_type_node : V4HI_type_node;
    case V8QImode:
      return unsignedp ? unsigned_V8QI_type_node : V8QI_type_node;
    case V1DImode:
      return unsignedp ? unsigned_V1DI_type_node : V1DI_type_node;
    case V16SFmode:
      return V16SF_type_node;
    case V4SFmode:
      return V4SF_type_node;
    case V2SFmode:
      return V2SF_type_node;
    case V2DFmode:
      return V2DF_type_node;
    default:
      break;
    }

  return 0;
}

/* Return an unsigned type the same as TYPE in other respects.  */
tree
c_common_unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node)
    return unsigned_intTI_type_node;
#endif
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;

  return c_common_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
c_common_signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == unsigned_intTI_type_node)
    return intTI_type_node;
#endif
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;

  return c_common_signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
c_common_signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);

#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  return type;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

unsigned int
min_precision (value, unsignedp)
     tree value;
     int unsignedp;
{
  int log;

  /* If the value is negative, compute its negative minus 1.  The latter
     adjustment is because the absolute value of the largest negative value
     is one larger than the largest positive value.  This is equivalent to
     a bit-wise negation, so use that operation instead.  */

  if (tree_int_cst_sgn (value) < 0)
    value = fold (build1 (BIT_NOT_EXPR, TREE_TYPE (value), value));

  /* Return the number of bits needed, taking into account the fact
     that we need one more bit for a signed than unsigned type.  */

  if (integer_zerop (value))
    log = 0;
  else
    log = tree_floor_log2 (value);

  return log + 1 + ! unsignedp;
}

/* Print an error message for invalid operands to arith operation
   CODE.  NOP_EXPR is used as a special case (see
   c_common_truthvalue_conversion).  */

void
binary_op_error (code)
     enum tree_code code;
{
  const char *opname;

  switch (code)
    {
    case NOP_EXPR:
      error ("invalid truth-value expression");
      return;

    case PLUS_EXPR:
      opname = "+"; break;
    case MINUS_EXPR:
      opname = "-"; break;
    case MULT_EXPR:
      opname = "*"; break;
    case MAX_EXPR:
      opname = "max"; break;
    case MIN_EXPR:
      opname = "min"; break;
    case EQ_EXPR:
      opname = "=="; break;
    case NE_EXPR:
      opname = "!="; break;
    case LE_EXPR:
      opname = "<="; break;
    case GE_EXPR:
      opname = ">="; break;
    case LT_EXPR:
      opname = "<"; break;
    case GT_EXPR:
      opname = ">"; break;
    case LSHIFT_EXPR:
      opname = "<<"; break;
    case RSHIFT_EXPR:
      opname = ">>"; break;
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      opname = "%"; break;
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
      opname = "/"; break;
    case BIT_AND_EXPR:
      opname = "&"; break;
    case BIT_IOR_EXPR:
      opname = "|"; break;
    case TRUTH_ANDIF_EXPR:
      opname = "&&"; break;
    case TRUTH_ORIF_EXPR:
      opname = "||"; break;
    case BIT_XOR_EXPR:
      opname = "^"; break;
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      opname = "rotate"; break;
    default:
      opname = "unknown"; break;
    }
  error ("invalid operands to binary %s", opname);
}

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.
   This function is also responsible for converting the two operands
   to the proper common type for comparison.

   The arguments of this function are all pointers to local variables
   of build_binary_op: OP0_PTR is &OP0, OP1_PTR is &OP1,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   If this function returns nonzero, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

tree
shorten_compare (op0_ptr, op1_ptr, restype_ptr, rescode_ptr)
     tree *op0_ptr, *op1_ptr;
     tree *restype_ptr;
     enum tree_code *rescode_ptr;
{
  tree type;
  tree op0 = *op0_ptr;
  tree op1 = *op1_ptr;
  int unsignedp0, unsignedp1;
  int real1, real2;
  tree primop0, primop1;
  enum tree_code code = *rescode_ptr;

  /* Throw away any conversions to wider types
     already present in the operands.  */

  primop0 = get_narrower (op0, &unsignedp0);
  primop1 = get_narrower (op1, &unsignedp1);

  /* Handle the case that OP0 does not *contain* a conversion
     but it *requires* conversion to FINAL_TYPE.  */

  if (op0 == primop0 && TREE_TYPE (op0) != *restype_ptr)
    unsignedp0 = TREE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TREE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  Don't do this if
     the second arg is 0.  */

  if (TREE_CONSTANT (primop0)
      && ! integer_zerop (primop1) && ! real_zerop (primop1))
    {
      tree tem = primop0;
      int temi = unsignedp0;
      primop0 = primop1;
      primop1 = tem;
      tem = op0;
      op0 = op1;
      op1 = tem;
      *op0_ptr = op0;
      *op1_ptr = op1;
      unsignedp0 = unsignedp1;
      unsignedp1 = temi;
      temi = real1;
      real1 = real2;
      real2 = temi;

      switch (code)
	{
	case LT_EXPR:
	  code = GT_EXPR;
	  break;
	case GT_EXPR:
	  code = LT_EXPR;
	  break;
	case LE_EXPR:
	  code = GE_EXPR;
	  break;
	case GE_EXPR:
	  code = LE_EXPR;
	  break;
	default:
	  break;
	}
      *rescode_ptr = code;
    }

  /* If comparing an integer against a constant more bits wide,
     maybe we can deduce a value of 1 or 0 independent of the data.
     Or else truncate the constant now
     rather than extend the variable at run time.

     This is only interesting if the constant is the wider arg.
     Also, it is not safe if the constant is unsigned and the
     variable arg is signed, since in this case the variable
     would be sign-extended and then regarded as unsigned.
     Our technique fails in this case because the lowest/highest
     possible unsigned results don't follow naturally from the
     lowest/highest possible values of the variable operand.
     For just EQ_EXPR and NE_EXPR there is another technique that
     could be used: see if the constant can be faithfully represented
     in the other operand's type, by truncating it and reextending it
     and see if that preserves the constant's value.  */

  if (!real1 && !real2
      && TREE_CODE (primop1) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = TREE_UNSIGNED (*restype_ptr);
      tree val;

      type = c_common_signed_or_unsigned_type (unsignedp0,
					       TREE_TYPE (primop0));

      /* If TYPE is an enumeration, then we need to get its min/max
	 values from it's underlying integral type, not the enumerated
	 type itself.  */
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	type = c_common_type_for_size (TYPE_PRECISION (type), unsignedp0);

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = c_common_signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
	primop1 = convert (*restype_ptr, primop1);
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      if (unsignedp && unsignedp0)
	{
	  min_gt = INT_CST_LT_UNSIGNED (primop1, minval);
	  max_gt = INT_CST_LT_UNSIGNED (primop1, maxval);
	  min_lt = INT_CST_LT_UNSIGNED (minval, primop1);
	  max_lt = INT_CST_LT_UNSIGNED (maxval, primop1);
	}
      else
	{
	  min_gt = INT_CST_LT (primop1, minval);
	  max_gt = INT_CST_LT (primop1, maxval);
	  min_lt = INT_CST_LT (minval, primop1);
	  max_lt = INT_CST_LT (maxval, primop1);
	}

      val = 0;
      /* This used to be a switch, but Genix compiler can't handle that.  */
      if (code == NE_EXPR)
	{
	  if (max_lt || min_gt)
	    val = boolean_true_node;
	}
      else if (code == EQ_EXPR)
	{
	  if (max_lt || min_gt)
	    val = boolean_false_node;
	}
      else if (code == LT_EXPR)
	{
	  if (max_lt)
	    val = boolean_true_node;
	  if (!min_lt)
	    val = boolean_false_node;
	}
      else if (code == GT_EXPR)
	{
	  if (min_gt)
	    val = boolean_true_node;
	  if (!max_gt)
	    val = boolean_false_node;
	}
      else if (code == LE_EXPR)
	{
	  if (!max_gt)
	    val = boolean_true_node;
	  if (min_gt)
	    val = boolean_false_node;
	}
      else if (code == GE_EXPR)
	{
	  if (!min_lt)
	    val = boolean_true_node;
	  if (max_lt)
	    val = boolean_false_node;
	}

      /* If primop0 was sign-extended and unsigned comparison specd,
	 we did a signed comparison above using the signed type bounds.
	 But the comparison we output must be unsigned.

	 Also, for inequalities, VAL is no good; but if the signed
	 comparison had *any* fixed result, it follows that the
	 unsigned comparison just tests the sign in reverse
	 (positive values are LE, negative ones GE).
	 So we can generate an unsigned comparison
	 against an extreme value of the signed type.  */

      if (unsignedp && !unsignedp0)
	{
	  if (val != 0)
	    switch (code)
	      {
	      case LT_EXPR:
	      case GE_EXPR:
		primop1 = TYPE_MIN_VALUE (type);
		val = 0;
		break;

	      case LE_EXPR:
	      case GT_EXPR:
		primop1 = TYPE_MAX_VALUE (type);
		val = 0;
		break;

	      default:
		break;
	      }
	  type = c_common_unsigned_type (type);
	}

      if (TREE_CODE (primop0) != INTEGER_CST)
	{
	  if (val == boolean_false_node)
	    warning ("comparison is always false due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always true due to limited range of data type");
	}

      if (val != 0)
	{
	  /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	  if (TREE_SIDE_EFFECTS (primop0))
	    return build (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
	  return val;
	}

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }
  else if (real1 && real2
	   && (TYPE_PRECISION (TREE_TYPE (primop0))
	       == TYPE_PRECISION (TREE_TYPE (primop1))))
    type = TREE_TYPE (primop0);

  /* If args' natural types are both narrower than nominal type
     and both extend in the same manner, compare them
     in the type of the wider arg.
     Otherwise must actually extend both to the nominal
     common type lest different ways of extending
     alter the result.
     (eg, (short)-1 == (unsigned short)-1  should be 0.)  */

  else if (unsignedp0 == unsignedp1 && real1 == real2
	   && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr)
	   && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr))
    {
      type = common_type (TREE_TYPE (primop0), TREE_TYPE (primop1));
      type = c_common_signed_or_unsigned_type (unsignedp0
					       || TREE_UNSIGNED (*restype_ptr),
					       type);
      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop0
	= convert (c_common_signed_or_unsigned_type (unsignedp0,
						     TREE_TYPE (primop0)),
		   primop0);
      primop1
	= convert (c_common_signed_or_unsigned_type (unsignedp1,
						     TREE_TYPE (primop1)),
		   primop1);
    }
  else
    {
      /* Here we must do the comparison on the nominal type
	 using the args exactly as we received them.  */
      type = *restype_ptr;
      primop0 = op0;
      primop1 = op1;

      if (!real1 && !real2 && integer_zerop (primop1)
	  && TREE_UNSIGNED (*restype_ptr))
	{
	  tree value = 0;
	  switch (code)
	    {
	    case GE_EXPR:
	      /* All unsigned values are >= 0, so we warn if extra warnings
		 are requested.  However, if OP0 is a constant that is
		 >= 0, the signedness of the comparison isn't an issue,
		 so suppress the warning.  */
	      if (extra_warnings && !in_system_header
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (c_common_signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression >= 0 is always true");
	      value = boolean_true_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings && !in_system_header
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (c_common_signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression < 0 is always false");
	      value = boolean_false_node;
	      break;

	    default:
	      break;
	    }

	  if (value != 0)
	    {
	      /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	      if (TREE_SIDE_EFFECTS (primop0))
		return build (COMPOUND_EXPR, TREE_TYPE (value),
			      primop0, value);
	      return value;
	    }
	}
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = boolean_type_node;

  return 0;
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

tree
pointer_int_sum (resultcode, ptrop, intop)
     enum tree_code resultcode;
     tree ptrop, intop;
{
  tree size_exp;

  tree result;
  tree folded;

  /* The result is a pointer of the same type that is being added.  */

  tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer of type `void *' used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer to a function used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == METHOD_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer to member function used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == OFFSET_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer to a member used in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = size_in_bytes (TREE_TYPE (result_type));

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */

  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && ! TREE_CONSTANT (intop)
      && TREE_CONSTANT (TREE_OPERAND (intop, 1))
      && TREE_CONSTANT (size_exp)
      /* If the constant comes from pointer subtraction,
	 skip this optimization--it would cause an error.  */
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (intop, 0))) == INTEGER_TYPE
      /* If the constant is unsigned, and smaller than the pointer size,
	 then we must skip this optimization.  This is because it could cause
	 an overflow error if the constant is negative but INTOP is not.  */
      && (! TREE_UNSIGNED (TREE_TYPE (intop))
	  || (TYPE_PRECISION (TREE_TYPE (intop))
	      == TYPE_PRECISION (TREE_TYPE (ptrop)))))
    {
      enum tree_code subcode = resultcode;
      tree int_type = TREE_TYPE (intop);
      if (TREE_CODE (intop) == MINUS_EXPR)
	subcode = (subcode == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR);
      /* Convert both subexpression types to the type of intop,
	 because weird cases involving pointer arithmetic
	 can result in a sum or difference with different type args.  */
      ptrop = build_binary_op (subcode, ptrop,
			       convert (int_type, TREE_OPERAND (intop, 1)), 1);
      intop = convert (int_type, TREE_OPERAND (intop, 0));
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype)
      || TREE_UNSIGNED (TREE_TYPE (intop)) != TREE_UNSIGNED (sizetype))
    intop = convert (c_common_type_for_size (TYPE_PRECISION (sizetype), 
					     TREE_UNSIGNED (sizetype)), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate
     pointer type (actually unsigned integral).  */

  intop = convert (result_type,
		   build_binary_op (MULT_EXPR, intop,
				    convert (TREE_TYPE (intop), size_exp), 1));

  /* Create the sum or difference.  */

  result = build (resultcode, result_type, ptrop, intop);

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (ptrop) & TREE_CONSTANT (intop);
  return folded;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.  */

tree
c_common_truthvalue_conversion (expr)
     tree expr;
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

#if 0 /* This appears to be wrong for C++.  */
  /* These really should return error_mark_node after 2.4 is stable.
     But not all callers handle ERROR_MARK properly.  */
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case RECORD_TYPE:
      error ("struct type value used where scalar is required");
      return boolean_false_node;

    case UNION_TYPE:
      error ("union type value used where scalar is required");
      return boolean_false_node;

    case ARRAY_TYPE:
      error ("array type value used where scalar is required");
      return boolean_false_node;

    default:
      break;
    }
#endif /* 0 */

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      TREE_TYPE (expr) = boolean_type_node;
      return expr;

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? boolean_false_node : boolean_true_node;

    case REAL_CST:
      return real_zerop (expr) ? boolean_false_node : boolean_true_node;

    case ADDR_EXPR:
      /* If we are taking the address of an external decl, it might be zero
	 if it is weak, so we cannot optimize.  */
      if (DECL_P (TREE_OPERAND (expr, 0))
	  && DECL_EXTERNAL (TREE_OPERAND (expr, 0)))
	break;

      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, boolean_type_node,
		      TREE_OPERAND (expr, 0), boolean_true_node);
      else
	return boolean_true_node;

    case COMPLEX_EXPR:
      return build_binary_op ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			       ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
			      0);

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is nonzero or zero.  */
      return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or nonzero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	return build (COMPOUND_EXPR, boolean_type_node, TREE_OPERAND (expr, 1),
		      c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through...  */
    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    case MINUS_EXPR:
      /* Perhaps reduce (x - y) != 0 to (x != y).  The expressions
	 aren't guaranteed to the be same for modes that can represent
	 infinity, since if x and y are both +infinity, or both
	 -infinity, then x - y is not a number.

	 Note that this transformation is safe when x or y is NaN.
	 (x - y) is then NaN, and both (x - y) != 0 and x != y will
	 be false.  */
      if (HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (TREE_OPERAND (expr, 0)))))
	break;
      /* fall through...  */
    case BIT_XOR_EXPR:
      /* This and MINUS_EXPR can be changed into a comparison of the
	 two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
	  == TREE_TYPE (TREE_OPERAND (expr, 1)))
	return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
				TREE_OPERAND (expr, 1), 1);
      return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
			      fold (build1 (NOP_EXPR,
					    TREE_TYPE (TREE_OPERAND (expr, 0)),
					    TREE_OPERAND (expr, 1))), 1);

    case BIT_AND_EXPR:
      if (integer_onep (TREE_OPERAND (expr, 1))
	  && TREE_TYPE (expr) != boolean_type_node)
	/* Using convert here would cause infinite recursion.  */
	return build1 (NOP_EXPR, boolean_type_node, expr);
      break;

    case MODIFY_EXPR:
      if (warn_parentheses && C_EXP_ORIGINAL_CODE (expr) == MODIFY_EXPR)
	warning ("suggest parentheses around assignment used as truth value");
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree t = save_expr (expr);
      return (build_binary_op
	      ((TREE_SIDE_EFFECTS (expr)
		? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	c_common_truthvalue_conversion (build_unary_op (REALPART_EXPR, t, 0)),
	c_common_truthvalue_conversion (build_unary_op (IMAGPART_EXPR, t, 0)),
	       0));
    }

  return build_binary_op (NE_EXPR, expr, integer_zero_node, 1);
}

static tree builtin_function_2 PARAMS ((const char *, const char *, tree, tree,
					int, enum built_in_class, int, int,
					tree));

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
c_build_qualified_type (type, type_quals)
     tree type;
     int type_quals;
{
  /* A restrict-qualified pointer type must be a pointer to object or
     incomplete type.  Note that the use of POINTER_TYPE_P also allows
     REFERENCE_TYPEs, which is appropriate for C++.  Unfortunately,
     the C++ front-end also use POINTER_TYPE for pointer-to-member
     values, so even though it should be illegal to use `restrict'
     with such an entity we don't flag that here.  Thus, special case
     code for that case is required in the C++ front-end.  */
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && (!POINTER_TYPE_P (type)
	  || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (type))))
    {
      error ("invalid use of `restrict'");
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_array_type (c_build_qualified_type (TREE_TYPE (type),
						     type_quals),
			     TYPE_DOMAIN (type));
  return build_qualified_type (type, type_quals);
}

/* Apply the TYPE_QUALS to the new DECL.  */

void
c_apply_type_quals_to_decl (type_quals, decl)
     int type_quals;
     tree decl;
{
  if ((type_quals & TYPE_QUAL_CONST)
      || (TREE_TYPE (decl) 
	  && TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE))
    TREE_READONLY (decl) = 1;
  if (type_quals & TYPE_QUAL_VOLATILE)
    {
      TREE_SIDE_EFFECTS (decl) = 1;
      TREE_THIS_VOLATILE (decl) = 1;
    }
  if (type_quals & TYPE_QUAL_RESTRICT)
    {
      if (!TREE_TYPE (decl)
	  || !POINTER_TYPE_P (TREE_TYPE (decl))
	  || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (TREE_TYPE (decl))))
	error ("invalid use of `restrict'");
      else if (flag_strict_aliasing)
	/* Indicate we need to make a unique alias set for this pointer.
	   We can't do it here because it might be pointing to an
	   incomplete type.  */
	DECL_POINTER_ALIAS_SET (decl) = -2;
    }
}

/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

HOST_WIDE_INT
c_common_get_alias_set (t)
     tree t;
{
  tree u;
  
  /* Permit type-punning when accessing a union, provided the access
     is directly through the union.  For example, this code does not
     permit taking the address of a union member and then storing
     through it.  Even the type-punning allowed here is a GCC
     extension, albeit a common and useful one; the C standard says
     that such accesses have implementation-defined behavior.  */
  for (u = t;
       TREE_CODE (u) == COMPONENT_REF || TREE_CODE (u) == ARRAY_REF;
       u = TREE_OPERAND (u, 0))
    if (TREE_CODE (u) == COMPONENT_REF
	&& TREE_CODE (TREE_TYPE (TREE_OPERAND (u, 0))) == UNION_TYPE)
      return 0;

  /* That's all the expressions we handle specially.  */
  if (! TYPE_P (t))
    return -1;

  /* The C standard guarantess that any object may be accessed via an
     lvalue that has character type.  */
  if (t == char_type_node
      || t == signed_char_type_node
      || t == unsigned_char_type_node)
    return 0;

  /* If it has the may_alias attribute, it can alias anything.  */
  if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (t)))
    return 0;

  /* The C standard specifically allows aliasing between signed and
     unsigned variants of the same type.  We treat the signed
     variant as canonical.  */
  if (TREE_CODE (t) == INTEGER_TYPE && TREE_UNSIGNED (t))
    {
      tree t1 = c_common_signed_type (t);

      /* t1 == t can happen for boolean nodes which are always unsigned.  */
      if (t1 != t)
	return get_alias_set (t1);
    }
  else if (POINTER_TYPE_P (t))
    {
      tree t1;

      /* Unfortunately, there is no canonical form of a pointer type.
	 In particular, if we have `typedef int I', then `int *', and
	 `I *' are different types.  So, we have to pick a canonical
	 representative.  We do this below.

	 Technically, this approach is actually more conservative that
	 it needs to be.  In particular, `const int *' and `int *'
	 should be in different alias sets, according to the C and C++
	 standard, since their types are not the same, and so,
	 technically, an `int **' and `const int **' cannot point at
	 the same thing.

         But, the standard is wrong.  In particular, this code is
	 legal C++:

            int *ip;
            int **ipp = &ip;
            const int* const* cipp = &ipp;

         And, it doesn't make sense for that to be legal unless you
	 can dereference IPP and CIPP.  So, we ignore cv-qualifiers on
	 the pointed-to types.  This issue has been reported to the
	 C++ committee.  */
      t1 = build_type_no_quals (t);
      if (t1 != t)
	return get_alias_set (t1);
    }

  return -1;
}

/* Compute the value of 'sizeof (TYPE)' or '__alignof__ (TYPE)', where the
   second parameter indicates which OPERATOR is being applied.  The COMPLAIN
   flag controls whether we should diagnose possibly ill-formed
   constructs or not.  */
tree
c_sizeof_or_alignof_type (type, op, complain)
     tree type;
     enum tree_code op;
     int complain;
{
  const char *op_name;
  tree value = NULL;
  enum tree_code type_code = TREE_CODE (type);
  
  my_friendly_assert (op == SIZEOF_EXPR || op == ALIGNOF_EXPR, 20020720);
  op_name = op == SIZEOF_EXPR ? "sizeof" : "__alignof__";
  
  if (type_code == FUNCTION_TYPE)
    {
      if (op == SIZEOF_EXPR)
	{
	  if (complain && (pedantic || warn_pointer_arith))
	    pedwarn ("invalid application of `sizeof' to a function type");
	  value = size_one_node;
	}
      else
	value = size_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);
    }
  else if (type_code == VOID_TYPE || type_code == ERROR_MARK)
    {
      if (type_code == VOID_TYPE 
	  && complain && (pedantic || warn_pointer_arith))
	pedwarn ("invalid application of `%s' to a void type", op_name);
      value = size_one_node;
    }
  else if (!COMPLETE_TYPE_P (type))
    {
      if (complain)
	error ("invalid application of `%s' to an incomplete type", op_name);
      value = size_zero_node;
    }
  else
    {
      if (op == SIZEOF_EXPR)
	/* Convert in case a char is more than one unit.  */
	value = size_binop (CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
			    size_int (TYPE_PRECISION (char_type_node)
				      / BITS_PER_UNIT));
      else
	value = size_int (TYPE_ALIGN (type) / BITS_PER_UNIT);
    }

  /* VALUE will have an integer type with TYPE_IS_SIZETYPE set.
     TYPE_IS_SIZETYPE means that certain things (like overflow) will
     never happen.  However, this node should really have type
     `size_t', which is just a typedef for an ordinary integer type.  */
  value = fold (build1 (NOP_EXPR, size_type_node, value));
  my_friendly_assert (!TYPE_IS_SIZETYPE (TREE_TYPE (value)), 20001021);
  
  return value;
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of EXPR, measured in bytes.  For VAR_DECL's and
   FIELD_DECL's return DECL_ALIGN (which can be set from an
   "aligned" __attribute__ specification).  */

tree
c_alignof_expr (expr)
     tree expr;
{
  tree t;

  if (TREE_CODE (expr) == VAR_DECL)
    t = size_int (DECL_ALIGN (expr) / BITS_PER_UNIT);
 
  else if (TREE_CODE (expr) == COMPONENT_REF
	   && DECL_C_BIT_FIELD (TREE_OPERAND (expr, 1)))
    {
      error ("`__alignof' applied to a bit-field");
      t = size_one_node;
    }
  else if (TREE_CODE (expr) == COMPONENT_REF
	   && TREE_CODE (TREE_OPERAND (expr, 1)) == FIELD_DECL)
    t = size_int (DECL_ALIGN (TREE_OPERAND (expr, 1)) / BITS_PER_UNIT);
 
  else if (TREE_CODE (expr) == INDIRECT_REF)
    {
      tree t = TREE_OPERAND (expr, 0);
      tree best = t;
      int bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
 
      while (TREE_CODE (t) == NOP_EXPR
	     && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
	{
	  int thisalign;

	  t = TREE_OPERAND (t, 0);
	  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
	  if (thisalign > bestalign)
	    best = t, bestalign = thisalign;
	}
      return c_alignof (TREE_TYPE (TREE_TYPE (best)));
    }
  else
    return c_alignof (TREE_TYPE (expr));

  return fold (build1 (NOP_EXPR, size_type_node, t));
}

/* Handle C and C++ default attributes.  */

enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#define DEF_FN_ATTR(NAME, ATTRS, PREDICATE) /* No entry needed in enum.  */
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
#undef DEF_FN_ATTR
  ATTR_LAST
};

static GTY(()) tree built_in_attributes[(int) ATTR_LAST];

static bool c_attrs_initialized = false;

static void c_init_attributes PARAMS ((void));

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.  */

void
c_common_nodes_and_builtins ()
{
  enum builtin_type 
  {
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_POINTER_TYPE
    BT_LAST
  };

  typedef enum builtin_type builtin_type;

  tree builtin_types[(int) BT_LAST];
  int wchar_type_size;
  tree array_domain_type;
  tree va_list_ref_type_node;
  tree va_list_arg_type_node;

  /* Define `int' and `char' first so that dbx will output them first.  */
  record_builtin_type (RID_INT, NULL, integer_type_node);
  record_builtin_type (RID_CHAR, "char", char_type_node);

  /* `signed' is the same as `int'.  FIXME: the declarations of "signed",
     "unsigned long", "long long unsigned" and "unsigned short" were in C++
     but not C.  Are the conditionals here needed?  */
  if (c_language == clk_cplusplus)
    record_builtin_type (RID_SIGNED, NULL, integer_type_node);
  record_builtin_type (RID_LONG, "long int", long_integer_type_node);
  record_builtin_type (RID_UNSIGNED, "unsigned int", unsigned_type_node);
  record_builtin_type (RID_MAX, "long unsigned int",
		       long_unsigned_type_node);
  if (c_language == clk_cplusplus)
    record_builtin_type (RID_MAX, "unsigned long", long_unsigned_type_node);
  record_builtin_type (RID_MAX, "long long int",
		       long_long_integer_type_node);
  record_builtin_type (RID_MAX, "long long unsigned int",
		       long_long_unsigned_type_node);
  if (c_language == clk_cplusplus)
    record_builtin_type (RID_MAX, "long long unsigned",
			 long_long_unsigned_type_node);
  record_builtin_type (RID_SHORT, "short int", short_integer_type_node);
  record_builtin_type (RID_MAX, "short unsigned int",
		       short_unsigned_type_node);
  if (c_language == clk_cplusplus)
    record_builtin_type (RID_MAX, "unsigned short",
			 short_unsigned_type_node);

  /* Define both `signed char' and `unsigned char'.  */
  record_builtin_type (RID_MAX, "signed char", signed_char_type_node);
  record_builtin_type (RID_MAX, "unsigned char", unsigned_char_type_node);

  /* These are types that c_common_type_for_size and
     c_common_type_for_mode use.  */
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    intQI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    intHI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    intSI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
					    get_identifier ("__int128_t"),
					    intTI_type_node));
#endif
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    unsigned_intQI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    unsigned_intHI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    unsigned_intSI_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    unsigned_intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
					    get_identifier ("__uint128_t"),
					    unsigned_intTI_type_node));
#endif

  /* Create the widest literal types.  */
  widest_integer_literal_type_node
    = make_signed_type (HOST_BITS_PER_WIDE_INT * 2);
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    widest_integer_literal_type_node));

  widest_unsigned_literal_type_node
    = make_unsigned_type (HOST_BITS_PER_WIDE_INT * 2);
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL, NULL_TREE,
					    widest_unsigned_literal_type_node));

  /* `unsigned long' is the standard type for sizeof.
     Note that stddef.h uses `unsigned long',
     and this must agree, even if long and int are the same size.  */
  size_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (SIZE_TYPE)));
  signed_size_type_node = c_common_signed_type (size_type_node);
  set_sizetype (size_type_node);

  build_common_tree_nodes_2 (flag_short_double);

  record_builtin_type (RID_FLOAT, NULL, float_type_node);
  record_builtin_type (RID_DOUBLE, NULL, double_type_node);
  record_builtin_type (RID_MAX, "long double", long_double_type_node);

  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
					    get_identifier ("complex int"),
					    complex_integer_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
					    get_identifier ("complex float"),
					    complex_float_type_node));
  (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
					    get_identifier ("complex double"),
					    complex_double_type_node));
  (*lang_hooks.decls.pushdecl)
    (build_decl (TYPE_DECL, get_identifier ("complex long double"),
		 complex_long_double_type_node));

  /* Types which are common to the fortran compiler and libf2c.  When
     changing these, you also need to be concerned with f/com.h.  */

  if (TYPE_PRECISION (float_type_node)
      == TYPE_PRECISION (long_integer_type_node))
    {
      g77_integer_type_node = long_integer_type_node;
      g77_uinteger_type_node = long_unsigned_type_node;
    }
  else if (TYPE_PRECISION (float_type_node)
	   == TYPE_PRECISION (integer_type_node))
    {
      g77_integer_type_node = integer_type_node;
      g77_uinteger_type_node = unsigned_type_node;
    }
  else
    g77_integer_type_node = g77_uinteger_type_node = NULL_TREE;

  if (g77_integer_type_node != NULL_TREE)
    {
      (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
						get_identifier ("__g77_integer"),
						g77_integer_type_node));
      (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
						get_identifier ("__g77_uinteger"),
						g77_uinteger_type_node));
    }

  if (TYPE_PRECISION (float_type_node) * 2
      == TYPE_PRECISION (long_integer_type_node))
    {
      g77_longint_type_node = long_integer_type_node;
      g77_ulongint_type_node = long_unsigned_type_node;
    }
  else if (TYPE_PRECISION (float_type_node) * 2
	   == TYPE_PRECISION (long_long_integer_type_node))
    {
      g77_longint_type_node = long_long_integer_type_node;
      g77_ulongint_type_node = long_long_unsigned_type_node;
    }
  else
    g77_longint_type_node = g77_ulongint_type_node = NULL_TREE;

  if (g77_longint_type_node != NULL_TREE)
    {
      (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
						get_identifier ("__g77_longint"),
						g77_longint_type_node));
      (*lang_hooks.decls.pushdecl) (build_decl (TYPE_DECL,
						get_identifier ("__g77_ulongint"),
						g77_ulongint_type_node));
    }

  record_builtin_type (RID_VOID, NULL, void_type_node);

  void_zero_node = build_int_2 (0, 0);
  TREE_TYPE (void_zero_node) = void_type_node;

  void_list_node = build_void_list_node ();

  /* Make a type to be the domain of a few array types
     whose domains don't really matter.
     200 is small enough that it always fits in size_t
     and large enough that it can hold most function names for the
     initializations of __FUNCTION__ and __PRETTY_FUNCTION__.  */
  array_domain_type = build_index_type (size_int (200));

  /* Make a type for arrays of characters.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, array_domain_type);

  /* Likewise for arrays of ints.  */
  int_array_type_node
    = build_array_type (integer_type_node, array_domain_type);

  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type
			  (char_type_node, TYPE_QUAL_CONST));

  /* This is special for C++ so functions can be overloaded.  */
  wchar_type_node = get_identifier (MODIFIED_WCHAR_TYPE);
  wchar_type_node = TREE_TYPE (identifier_global_value (wchar_type_node));
  wchar_type_size = TYPE_PRECISION (wchar_type_node);
  if (c_language == clk_cplusplus)
    {
      if (TREE_UNSIGNED (wchar_type_node))
	wchar_type_node = make_unsigned_type (wchar_type_size);
      else
	wchar_type_node = make_signed_type (wchar_type_size);
      record_builtin_type (RID_WCHAR, "wchar_t", wchar_type_node);
    }
  else
    {
      signed_wchar_type_node = c_common_signed_type (wchar_type_node);
      unsigned_wchar_type_node = c_common_unsigned_type (wchar_type_node);
    }

  /* This is for wide string constants.  */
  wchar_array_type_node
    = build_array_type (wchar_type_node, array_domain_type);

  wint_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (WINT_TYPE)));

  intmax_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (INTMAX_TYPE)));
  uintmax_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (UINTMAX_TYPE)));

  default_function_type = build_function_type (integer_type_node, NULL_TREE);
  ptrdiff_type_node
    = TREE_TYPE (identifier_global_value (get_identifier (PTRDIFF_TYPE)));
  unsigned_ptrdiff_type_node = c_common_unsigned_type (ptrdiff_type_node);

  (*lang_hooks.decls.pushdecl)
    (build_decl (TYPE_DECL, get_identifier ("__builtin_va_list"),
		 va_list_type_node));

  (*lang_hooks.decls.pushdecl)
    (build_decl (TYPE_DECL, get_identifier ("__builtin_ptrdiff_t"),
		 ptrdiff_type_node));

  (*lang_hooks.decls.pushdecl)
    (build_decl (TYPE_DECL, get_identifier ("__builtin_size_t"),
		 sizetype));

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      va_list_arg_type_node = va_list_ref_type_node =
	build_pointer_type (TREE_TYPE (va_list_type_node));
    }
  else
    {
      va_list_arg_type_node = va_list_type_node;
      va_list_ref_type_node = build_reference_type (va_list_type_node);
    }
 
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)		\
  builtin_types[(int) ENUM]				\
    = build_function_type (builtin_types[(int) RETURN],	\
			   void_list_node);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN],			\
			   tree_cons (NULL_TREE,			\
				      builtin_types[(int) ARG1],	\
				      void_list_node));
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)	\
  builtin_types[(int) ENUM]				\
    = build_function_type 				\
      (builtin_types[(int) RETURN],			\
       tree_cons (NULL_TREE,				\
		  builtin_types[(int) ARG1],		\
		  tree_cons (NULL_TREE,			\
			     builtin_types[(int) ARG2],	\
			     void_list_node)));
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)		 \
  builtin_types[(int) ENUM]						 \
    = build_function_type						 \
      (builtin_types[(int) RETURN],					 \
       tree_cons (NULL_TREE,						 \
		  builtin_types[(int) ARG1],				 \
		  tree_cons (NULL_TREE,					 \
			     builtin_types[(int) ARG2],			 \
			     tree_cons (NULL_TREE,			 \
					builtin_types[(int) ARG3],	 \
					void_list_node))));
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)	\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons 					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],	 	\
			      tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG4],	\
					 void_list_node)))));
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN], NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1)			 \
   builtin_types[(int) ENUM]						 \
    = build_function_type (builtin_types[(int) RETURN], 		 \
			   tree_cons (NULL_TREE,			 \
				      builtin_types[(int) ARG1],	 \
				      NULL_TREE));

#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2)	\
   builtin_types[(int) ENUM]					\
    = build_function_type 					\
      (builtin_types[(int) RETURN],				\
       tree_cons (NULL_TREE,					\
		  builtin_types[(int) ARG1],			\
		  tree_cons (NULL_TREE,				\
			     builtin_types[(int) ARG2],		\
			     NULL_TREE)));

#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3)		\
   builtin_types[(int) ENUM]						\
    = build_function_type 						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons (NULL_TREE,			\
					builtin_types[(int) ARG3],	\
					NULL_TREE))));

#define DEF_POINTER_TYPE(ENUM, TYPE)			\
  builtin_types[(int) ENUM]				\
    = build_pointer_type (builtin_types[(int) TYPE]);
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_POINTER_TYPE

  if (!c_attrs_initialized)
    c_init_attributes ();

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE,			\
		    BOTH_P, FALLBACK_P, NONANSI_P, ATTRS)		\
  if (NAME)								\
    {									\
      tree decl;							\
									\
      if (strncmp (NAME, "__builtin_", strlen ("__builtin_")) != 0)	\
	abort ();							\
									\
      if (!BOTH_P)							\
	decl = builtin_function (NAME, builtin_types[TYPE], ENUM,	\
				 CLASS,					\
				 (FALLBACK_P				\
				  ? (NAME + strlen ("__builtin_"))	\
				  : NULL),				\
				 built_in_attributes[(int) ATTRS]);	\
      else								\
	decl = builtin_function_2 (NAME,				\
				   NAME + strlen ("__builtin_"),	\
				   builtin_types[TYPE],			\
				   builtin_types[LIBTYPE],		\
				   ENUM,				\
				   CLASS,				\
				   FALLBACK_P,				\
				   NONANSI_P,				\
				   built_in_attributes[(int) ATTRS]);	\
									\
      built_in_decls[(int) ENUM] = decl;				\
    }									
#include "builtins.def"
#undef DEF_BUILTIN

  (*targetm.init_builtins) ();

  main_identifier_node = get_identifier ("main");
}

tree
build_va_arg (expr, type)
     tree expr, type;
{
  return build1 (VA_ARG_EXPR, type, expr);
}


/* Linked list of disabled built-in functions.  */

typedef struct disabled_builtin
{
  const char *name;
  struct disabled_builtin *next;
} disabled_builtin;
static disabled_builtin *disabled_builtins = NULL;

static bool builtin_function_disabled_p PARAMS ((const char *));

/* Disable a built-in function specified by -fno-builtin-NAME.  If NAME
   begins with "__builtin_", give an error.  */

void
disable_builtin_function (name)
     const char *name;
{
  if (strncmp (name, "__builtin_", strlen ("__builtin_")) == 0)
    error ("cannot disable built-in function `%s'", name);
  else
    {
      disabled_builtin *new = xmalloc (sizeof (disabled_builtin));
      new->name = name;
      new->next = disabled_builtins;
      disabled_builtins = new;
    }
}


/* Return true if the built-in function NAME has been disabled, false
   otherwise.  */

static bool
builtin_function_disabled_p (name)
     const char *name;
{
  disabled_builtin *p;
  for (p = disabled_builtins; p != NULL; p = p->next)
    {
      if (strcmp (name, p->name) == 0)
	return true;
    }
  return false;
}


/* Possibly define a builtin function with one or two names.  BUILTIN_NAME
   is an __builtin_-prefixed name; NAME is the ordinary name; one or both
   of these may be NULL (though both being NULL is useless).
   BUILTIN_TYPE is the type of the __builtin_-prefixed function;
   TYPE is the type of the function with the ordinary name.  These
   may differ if the ordinary name is declared with a looser type to avoid
   conflicts with headers.  FUNCTION_CODE and CLASS are as for
   builtin_function.  If LIBRARY_NAME_P is nonzero, NAME is passed as
   the LIBRARY_NAME parameter to builtin_function when declaring BUILTIN_NAME.
   If NONANSI_P is nonzero, the name NAME is treated as a non-ANSI name;
   ATTRS is the tree list representing the builtin's function attributes.
   Returns the declaration of BUILTIN_NAME, if any, otherwise
   the declaration of NAME.  Does not declare NAME if flag_no_builtin,
   or if NONANSI_P and flag_no_nonansi_builtin.  */

static tree
builtin_function_2 (builtin_name, name, builtin_type, type, function_code,
		    class, library_name_p, nonansi_p, attrs)
     const char *builtin_name;
     const char *name;
     tree builtin_type;
     tree type;
     int function_code;
     enum built_in_class class;
     int library_name_p;
     int nonansi_p;
     tree attrs;
{
  tree bdecl = NULL_TREE;
  tree decl = NULL_TREE;
  if (builtin_name != 0)
    {
      bdecl = builtin_function (builtin_name, builtin_type, function_code,
				class, library_name_p ? name : NULL,
				attrs);
    }
  if (name != 0 && !flag_no_builtin && !builtin_function_disabled_p (name)
      && !(nonansi_p && flag_no_nonansi_builtin))
    {
      decl = builtin_function (name, type, function_code, class, NULL,
			       attrs);
      if (nonansi_p)
	DECL_BUILT_IN_NONANSI (decl) = 1;
    }
  return (bdecl != 0 ? bdecl : decl);
}

/* Nonzero if the type T promotes to int.  This is (nearly) the
   integral promotions defined in ISO C99 6.3.1.1/2.  */

bool
c_promoting_integer_type_p (t)
     tree t;
{
  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
      return (TYPE_MAIN_VARIANT (t) == char_type_node
	      || TYPE_MAIN_VARIANT (t) == signed_char_type_node
	      || TYPE_MAIN_VARIANT (t) == unsigned_char_type_node
	      || TYPE_MAIN_VARIANT (t) == short_integer_type_node
	      || TYPE_MAIN_VARIANT (t) == short_unsigned_type_node
	      || TYPE_PRECISION (t) < TYPE_PRECISION (integer_type_node));

    case ENUMERAL_TYPE:
      /* ??? Technically all enumerations not larger than an int
	 promote to an int.  But this is used along code paths
	 that only want to notice a size change.  */
      return TYPE_PRECISION (t) < TYPE_PRECISION (integer_type_node);

    case BOOLEAN_TYPE:
      return 1;

    default:
      return 0;
    }
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
self_promoting_args_p (parms)
     tree parms;
{
  tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0 && type != void_type_node)
	return 0;

      if (type == 0)
	return 0;

      if (TYPE_MAIN_VARIANT (type) == float_type_node)
	return 0;

      if (c_promoting_integer_type_p (type))
	return 0;
    }
  return 1;
}

/* Recursively examines the array elements of TYPE, until a non-array
   element type is found.  */

tree
strip_array_types (type)
     tree type;
{
  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  return type;
}

static tree expand_unordered_cmp PARAMS ((tree, tree, enum tree_code,
					  enum tree_code));

/* Expand a call to an unordered comparison function such as
   __builtin_isgreater().  FUNCTION is the function's declaration and
   PARAMS a list of the values passed.  For __builtin_isunordered(),
   UNORDERED_CODE is UNORDERED_EXPR and ORDERED_CODE is NOP_EXPR.  In
   other cases, UNORDERED_CODE and ORDERED_CODE are comparison codes
   that give the opposite of the desired result.  UNORDERED_CODE is
   used for modes that can hold NaNs and ORDERED_CODE is used for the
   rest.  */

static tree
expand_unordered_cmp (function, params, unordered_code, ordered_code)
     tree function, params;
     enum tree_code unordered_code, ordered_code;
{
  tree arg0, arg1, type;
  enum tree_code code0, code1;

  /* Check that we have exactly two arguments.  */
  if (params == 0 || TREE_CHAIN (params) == 0)
    {
      error ("too few arguments to function `%s'",
	     IDENTIFIER_POINTER (DECL_NAME (function)));
      return error_mark_node;
    }
  else if (TREE_CHAIN (TREE_CHAIN (params)) != 0)
    {
      error ("too many arguments to function `%s'",
	     IDENTIFIER_POINTER (DECL_NAME (function)));
      return error_mark_node;
    }

  arg0 = TREE_VALUE (params);
  arg1 = TREE_VALUE (TREE_CHAIN (params));

  code0 = TREE_CODE (TREE_TYPE (arg0));
  code1 = TREE_CODE (TREE_TYPE (arg1));

  /* Make sure that the arguments have a common type of REAL.  */
  type = 0;
  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
      && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
    type = common_type (TREE_TYPE (arg0), TREE_TYPE (arg1));

  if (type == 0 || TREE_CODE (type) != REAL_TYPE)
    {
      error ("non-floating-point argument to function `%s'",
	     IDENTIFIER_POINTER (DECL_NAME (function)));
      return error_mark_node;
    }

  if (unordered_code == UNORDERED_EXPR)
    {
      if (MODE_HAS_NANS (TYPE_MODE (type)))
	return build_binary_op (unordered_code,
				convert (type, arg0),
				convert (type, arg1),
				0);
      else
	return integer_zero_node;
    }

  return build_unary_op (TRUTH_NOT_EXPR,
			 build_binary_op (MODE_HAS_NANS (TYPE_MODE (type))
					  ? unordered_code
					  : ordered_code,
					  convert (type, arg0),
					  convert (type, arg1),
					  0),
			 0);
}


/* Recognize certain built-in functions so we can make tree-codes
   other than CALL_EXPR.  We do this when it enables fold-const.c
   to do something useful.  */
/* ??? By rights this should go in builtins.c, but only C and C++
   implement build_{binary,unary}_op.  Not exactly sure what bits
   of functionality are actually needed from those functions, or
   where the similar functionality exists in the other front ends.  */

tree
expand_tree_builtin (function, params, coerced_params)
     tree function, params, coerced_params;
{
  if (DECL_BUILT_IN_CLASS (function) != BUILT_IN_NORMAL)
    return NULL_TREE;

  switch (DECL_FUNCTION_CODE (function))
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_LLABS:
    case BUILT_IN_IMAXABS:
    case BUILT_IN_FABS:
    case BUILT_IN_FABSL:
    case BUILT_IN_FABSF:
      if (coerced_params == 0)
	return integer_zero_node;
      return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);

    case BUILT_IN_CONJ:
    case BUILT_IN_CONJF:
    case BUILT_IN_CONJL:
      if (coerced_params == 0)
	return integer_zero_node;
      return build_unary_op (CONJ_EXPR, TREE_VALUE (coerced_params), 0);

    case BUILT_IN_CREAL:
    case BUILT_IN_CREALF:
    case BUILT_IN_CREALL:
      if (coerced_params == 0)
	return integer_zero_node;
      return build_unary_op (REALPART_EXPR, TREE_VALUE (coerced_params), 0);

    case BUILT_IN_CIMAG:
    case BUILT_IN_CIMAGF:
    case BUILT_IN_CIMAGL:
      if (coerced_params == 0)
	return integer_zero_node;
      return build_unary_op (IMAGPART_EXPR, TREE_VALUE (coerced_params), 0);

    case BUILT_IN_ISGREATER:
      return expand_unordered_cmp (function, params, UNLE_EXPR, LE_EXPR);

    case BUILT_IN_ISGREATEREQUAL:
      return expand_unordered_cmp (function, params, UNLT_EXPR, LT_EXPR);

    case BUILT_IN_ISLESS:
      return expand_unordered_cmp (function, params, UNGE_EXPR, GE_EXPR);

    case BUILT_IN_ISLESSEQUAL:
      return expand_unordered_cmp (function, params, UNGT_EXPR, GT_EXPR);

    case BUILT_IN_ISLESSGREATER:
      return expand_unordered_cmp (function, params, UNEQ_EXPR, EQ_EXPR);

    case BUILT_IN_ISUNORDERED:
      return expand_unordered_cmp (function, params, UNORDERED_EXPR, NOP_EXPR);

    default:
      break;
    }

  return NULL_TREE;
}

/* Returns nonzero if CODE is the code for a statement.  */

int
statement_code_p (code)
     enum tree_code code;
{
  switch (code)
    {
    case CLEANUP_STMT:
    case EXPR_STMT:
    case COMPOUND_STMT:
    case DECL_STMT:
    case IF_STMT:
    case FOR_STMT:
    case WHILE_STMT:
    case DO_STMT:
    case RETURN_STMT:
    case BREAK_STMT:
    case CONTINUE_STMT:
    case SCOPE_STMT:
    case SWITCH_STMT:
    case GOTO_STMT:
    case LABEL_STMT:
    case ASM_STMT:
    case FILE_STMT:
    case CASE_LABEL:
      return 1;

    default:
      if (lang_statement_code_p)
	return (*lang_statement_code_p) (code);
      return 0;
    }
}

/* Walk the statement tree, rooted at *tp.  Apply FUNC to all the
   sub-trees of *TP in a pre-order traversal.  FUNC is called with the
   DATA and the address of each sub-tree.  If FUNC returns a non-NULL
   value, the traversal is aborted, and the value returned by FUNC is
   returned.  If FUNC sets WALK_SUBTREES to zero, then the subtrees of
   the node being visited are not walked.

   We don't need a without_duplicates variant of this one because the
   statement tree is a tree, not a graph.  */

tree 
walk_stmt_tree (tp, func, data)
     tree *tp;
     walk_tree_fn func;
     void *data;
{
  enum tree_code code;
  int walk_subtrees;
  tree result;
  int i, len;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_stmt_tree (&(NODE), func, data);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  /* Skip subtrees below non-statement nodes.  */
  if (!statement_code_p (TREE_CODE (*tp)))
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  /* FUNC may have modified the tree, recheck that we're looking at a
     statement node.  */
  code = TREE_CODE (*tp);
  if (!statement_code_p (code))
    return NULL_TREE;

  /* Visit the subtrees unless FUNC decided that there was nothing
     interesting below this point in the tree.  */
  if (walk_subtrees)
    {
      /* Walk over all the sub-trees of this operand.  Statement nodes
	 never contain RTL, and we needn't worry about TARGET_EXPRs.  */
      len = TREE_CODE_LENGTH (code);

      /* Go through the subtrees.  We need to do this in forward order so
	 that the scope of a FOR_EXPR is handled properly.  */
      for (i = 0; i < len; ++i)
	WALK_SUBTREE (TREE_OPERAND (*tp, i));
    }

  /* Finally visit the chain.  This can be tail-recursion optimized if
     we write it this way.  */
  return walk_stmt_tree (&TREE_CHAIN (*tp), func, data);

#undef WALK_SUBTREE
}

/* Used to compare case labels.  K1 and K2 are actually tree nodes
   representing case labels, or NULL_TREE for a `default' label.
   Returns -1 if K1 is ordered before K2, -1 if K1 is ordered after
   K2, and 0 if K1 and K2 are equal.  */

int
case_compare (k1, k2)
     splay_tree_key k1;
     splay_tree_key k2;
{
  /* Consider a NULL key (such as arises with a `default' label) to be
     smaller than anything else.  */
  if (!k1)
    return k2 ? -1 : 0;
  else if (!k2)
    return k1 ? 1 : 0;

  return tree_int_cst_compare ((tree) k1, (tree) k2);
}

/* Process a case label for the range LOW_VALUE ... HIGH_VALUE.  If
   LOW_VALUE and HIGH_VALUE are both NULL_TREE then this case label is
   actually a `default' label.  If only HIGH_VALUE is NULL_TREE, then
   case label was declared using the usual C/C++ syntax, rather than
   the GNU case range extension.  CASES is a tree containing all the
   case ranges processed so far; COND is the condition for the
   switch-statement itself.  Returns the CASE_LABEL created, or
   ERROR_MARK_NODE if no CASE_LABEL is created.  */

tree
c_add_case_label (cases, cond, low_value, high_value)
     splay_tree cases;
     tree cond;
     tree low_value;
     tree high_value;
{
  tree type;
  tree label;
  tree case_label;
  splay_tree_node node;

  /* Create the LABEL_DECL itself.  */
  label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  DECL_CONTEXT (label) = current_function_decl;

  /* If there was an error processing the switch condition, bail now
     before we get more confused.  */
  if (!cond || cond == error_mark_node)
    {
      /* Add a label anyhow so that the back-end doesn't think that
	 the beginning of the switch is unreachable.  */
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
      return error_mark_node;
    }

  if ((low_value && TREE_TYPE (low_value) 
       && POINTER_TYPE_P (TREE_TYPE (low_value))) 
      || (high_value && TREE_TYPE (high_value)
	  && POINTER_TYPE_P (TREE_TYPE (high_value))))
    error ("pointers are not permitted as case values");

  /* Case ranges are a GNU extension.  */
  if (high_value && pedantic)
    {
      if (c_language == clk_cplusplus)
	pedwarn ("ISO C++ forbids range expressions in switch statements");
      else
	pedwarn ("ISO C forbids range expressions in switch statements");
    }

  type = TREE_TYPE (cond);
  if (low_value)
    {
      low_value = check_case_value (low_value);
      low_value = convert_and_check (type, low_value);
    }
  if (high_value)
    {
      high_value = check_case_value (high_value);
      high_value = convert_and_check (type, high_value);
    }

  /* If an error has occurred, bail out now.  */
  if (low_value == error_mark_node || high_value == error_mark_node)
    {
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
      return error_mark_node;
    }

  /* If the LOW_VALUE and HIGH_VALUE are the same, then this isn't
     really a case range, even though it was written that way.  Remove
     the HIGH_VALUE to simplify later processing.  */
  if (tree_int_cst_equal (low_value, high_value))
    high_value = NULL_TREE;
  if (low_value && high_value 
      && !tree_int_cst_lt (low_value, high_value)) 
    warning ("empty range specified");

  /* Look up the LOW_VALUE in the table of case labels we already
     have.  */
  node = splay_tree_lookup (cases, (splay_tree_key) low_value);
  /* If there was not an exact match, check for overlapping ranges.
     There's no need to do this if there's no LOW_VALUE or HIGH_VALUE;
     that's a `default' label and the only overlap is an exact match.  */
  if (!node && (low_value || high_value))
    {
      splay_tree_node low_bound;
      splay_tree_node high_bound;

      /* Even though there wasn't an exact match, there might be an
	 overlap between this case range and another case range.
	 Since we've (inductively) not allowed any overlapping case
	 ranges, we simply need to find the greatest low case label
	 that is smaller that LOW_VALUE, and the smallest low case
	 label that is greater than LOW_VALUE.  If there is an overlap
	 it will occur in one of these two ranges.  */
      low_bound = splay_tree_predecessor (cases,
					  (splay_tree_key) low_value);
      high_bound = splay_tree_successor (cases,
					 (splay_tree_key) low_value);

      /* Check to see if the LOW_BOUND overlaps.  It is smaller than
	 the LOW_VALUE, so there is no need to check unless the
	 LOW_BOUND is in fact itself a case range.  */
      if (low_bound
	  && CASE_HIGH ((tree) low_bound->value)
	  && tree_int_cst_compare (CASE_HIGH ((tree) low_bound->value),
				    low_value) >= 0)
	node = low_bound;
      /* Check to see if the HIGH_BOUND overlaps.  The low end of that
	 range is bigger than the low end of the current range, so we
	 are only interested if the current range is a real range, and
	 not an ordinary case label.  */
      else if (high_bound 
	       && high_value
	       && (tree_int_cst_compare ((tree) high_bound->key,
					 high_value)
		   <= 0))
	node = high_bound;
    }
  /* If there was an overlap, issue an error.  */
  if (node)
    {
      tree duplicate = CASE_LABEL_DECL ((tree) node->value);

      if (high_value)
	{
	  error ("duplicate (or overlapping) case value");
	  error_with_decl (duplicate, 
			   "this is the first entry overlapping that value");
	}
      else if (low_value)
	{
	  error ("duplicate case value") ;
	  error_with_decl (duplicate, "previously used here");
	}
      else
	{
	  error ("multiple default labels in one switch");
	  error_with_decl (duplicate, "this is the first default label");
	}
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
    }

  /* Add a CASE_LABEL to the statement-tree.  */
  case_label = add_stmt (build_case_label (low_value, high_value, label));
  /* Register this case label in the splay tree.  */
  splay_tree_insert (cases, 
		     (splay_tree_key) low_value,
		     (splay_tree_value) case_label);

  return case_label;
}

/* Finish an expression taking the address of LABEL.  Returns an
   expression for the address.  */

tree 
finish_label_address_expr (label)
     tree label;
{
  tree result;

  if (pedantic)
    {
      if (c_language == clk_cplusplus)
	pedwarn ("ISO C++ forbids taking the address of a label");
      else
	pedwarn ("ISO C forbids taking the address of a label");
    }

  label = lookup_label (label);
  if (label == NULL_TREE)
    result = null_pointer_node;
  else
    {
      TREE_USED (label) = 1;
      result = build1 (ADDR_EXPR, ptr_type_node, label);
      TREE_CONSTANT (result) = 1;
      /* The current function in not necessarily uninlinable.
	 Computed gotos are incompatible with inlining, but the value
	 here could be used only in a diagnostic, for example.  */
    }

  return result;
}

/* Hook used by expand_expr to expand language-specific tree codes.  */

rtx
c_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     int modifier;  /* Actually enum_modifier.  */
{
  switch (TREE_CODE (exp))
    {
    case STMT_EXPR:
      {
	tree rtl_expr;
	rtx result;
	bool preserve_result = false;
	bool return_target = false;

	/* Since expand_expr_stmt calls free_temp_slots after every
	   expression statement, we must call push_temp_slots here.
	   Otherwise, any temporaries in use now would be considered
	   out-of-scope after the first EXPR_STMT from within the
	   STMT_EXPR.  */
	push_temp_slots ();
	rtl_expr = expand_start_stmt_expr (!STMT_EXPR_NO_SCOPE (exp));

	/* If we want the result of this expression, find the last
           EXPR_STMT in the COMPOUND_STMT and mark it as addressable.  */
	if (target != const0_rtx
	    && TREE_CODE (STMT_EXPR_STMT (exp)) == COMPOUND_STMT
	    && TREE_CODE (COMPOUND_BODY (STMT_EXPR_STMT (exp))) == SCOPE_STMT)
	  {
	    tree expr = COMPOUND_BODY (STMT_EXPR_STMT (exp));
	    tree last = TREE_CHAIN (expr);

	    while (TREE_CHAIN (last))
	      {
		expr = last;
		last = TREE_CHAIN (last);
	      }

	    if (TREE_CODE (last) == SCOPE_STMT
		&& TREE_CODE (expr) == EXPR_STMT)
	      {
		if (target && TREE_CODE (EXPR_STMT_EXPR (expr)) == VAR_DECL
		    && DECL_RTL_IF_SET (EXPR_STMT_EXPR (expr)) == target)
		  /* If the last expression is a variable whose RTL is the
		     same as our target, just return the target; if it
		     isn't valid expanding the decl would produce different
		     RTL, and store_expr would try to do a copy.  */
		  return_target = true;
		else
		  {
		    /* Otherwise, note that we want the value from the last
		       expression.  */
		    TREE_ADDRESSABLE (expr) = 1;
		    preserve_result = true;
		  }
	      }
	  }

	expand_stmt (STMT_EXPR_STMT (exp));
	expand_end_stmt_expr (rtl_expr);

	result = expand_expr (rtl_expr, target, tmode, modifier);
	if (return_target)
	  result = target;
	else if (preserve_result && GET_CODE (result) == MEM)
	  {
	    if (GET_MODE (result) != BLKmode)
	      result = copy_to_reg (result);
	    else
	      preserve_temp_slots (result);
	  }

	/* If the statment-expression does not have a scope, then the
	   new temporaries we created within it must live beyond the
	   statement-expression.  */
	if (STMT_EXPR_NO_SCOPE (exp))
	  preserve_temp_slots (NULL_RTX);

	pop_temp_slots ();
	return result;
      }
      break;
      
    case CALL_EXPR:
      {
	if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	    && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		== FUNCTION_DECL)
	    && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	    && (DECL_BUILT_IN_CLASS (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		== BUILT_IN_FRONTEND))
	  return c_expand_builtin (exp, target, tmode, modifier);
	else
	  abort ();
      }
      break;

    case COMPOUND_LITERAL_EXPR:
      {
	/* Initialize the anonymous variable declared in the compound
	   literal, then return the variable.  */
	tree decl = COMPOUND_LITERAL_EXPR_DECL (exp);
	emit_local_var (decl);
	return expand_expr (decl, target, tmode, modifier);
      }

    default:
      abort ();
    }

  abort ();
  return NULL;
}

/* Hook used by safe_from_p to handle language-specific tree codes.  */

int
c_safe_from_p (target, exp)
     rtx target;
     tree exp;
{
  /* We can see statements here when processing the body of a
     statement-expression.  For a declaration statement declaring a
     variable, look at the variable's initializer.  */
  if (TREE_CODE (exp) == DECL_STMT) 
    {
      tree decl = DECL_STMT_DECL (exp);

      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_INITIAL (decl)
	  && !safe_from_p (target, DECL_INITIAL (decl), /*top_p=*/0))
	return 0;
    }

  /* For any statement, we must follow the statement-chain.  */
  if (statement_code_p (TREE_CODE (exp)) && TREE_CHAIN (exp))
    return safe_from_p (target, TREE_CHAIN (exp), /*top_p=*/0);

  /* Assume everything else is safe.  */
  return 1;
}

/* Hook used by unsafe_for_reeval to handle language-specific tree codes.  */

int
c_common_unsafe_for_reeval (exp)
     tree exp;
{
  /* Statement expressions may not be reevaluated, likewise compound
     literals.  */
  if (TREE_CODE (exp) == STMT_EXPR
      || TREE_CODE (exp) == COMPOUND_LITERAL_EXPR)
    return 2;

  /* Walk all other expressions.  */
  return -1;
}

/* Hook used by staticp to handle language-specific tree codes.  */

int
c_staticp (exp)
     tree exp;
{
  if (TREE_CODE (exp) == COMPOUND_LITERAL_EXPR
      && TREE_STATIC (COMPOUND_LITERAL_EXPR_DECL (exp)))
    return 1;
  return 0;
}

#define CALLED_AS_BUILT_IN(NODE) \
   (!strncmp (IDENTIFIER_POINTER (DECL_NAME (NODE)), "__builtin_", 10))

static rtx
c_expand_builtin (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  enum tree_code code = TREE_CODE (exp);
  const int ignore = (target == const0_rtx
		      || ((code == NON_LVALUE_EXPR || code == NOP_EXPR
			   || code == CONVERT_EXPR || code == REFERENCE_EXPR
			   || code == COND_EXPR)
			  && TREE_CODE (type) == VOID_TYPE));

  if (! optimize && ! CALLED_AS_BUILT_IN (fndecl))
    return expand_call (exp, target, ignore);

  switch (fcode)
    {
    case BUILT_IN_PRINTF:
      target = c_expand_builtin_printf (arglist, target, tmode,
					modifier, ignore, /*unlocked=*/ 0);
      if (target)
	return target;
      break;

    case BUILT_IN_PRINTF_UNLOCKED:
      target = c_expand_builtin_printf (arglist, target, tmode,
					modifier, ignore, /*unlocked=*/ 1);
      if (target)
	return target;
      break;

    case BUILT_IN_FPRINTF:
      target = c_expand_builtin_fprintf (arglist, target, tmode,
					 modifier, ignore, /*unlocked=*/ 0);
      if (target)
	return target;
      break;

    case BUILT_IN_FPRINTF_UNLOCKED:
      target = c_expand_builtin_fprintf (arglist, target, tmode,
					 modifier, ignore, /*unlocked=*/ 1);
      if (target)
	return target;
      break;

    default:			/* just do library call, if unknown builtin */
      error ("built-in function `%s' not currently supported",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
}

/* Check an arglist to *printf for problems.  The arglist should start
   at the format specifier, with the remaining arguments immediately
   following it.  */
static int
is_valid_printf_arglist (arglist)
     tree arglist;
{
  /* Save this value so we can restore it later.  */
  const int SAVE_pedantic = pedantic;
  int diagnostic_occurred = 0;
  tree attrs;

  /* Set this to a known value so the user setting won't affect code
     generation.  */
  pedantic = 1;
  /* Check to make sure there are no format specifier errors.  */
  attrs = tree_cons (get_identifier ("format"),
		     tree_cons (NULL_TREE,
				get_identifier ("printf"),
				tree_cons (NULL_TREE,
					   integer_one_node,
					   tree_cons (NULL_TREE,
						      build_int_2 (2, 0),
						      NULL_TREE))),
		     NULL_TREE);
  check_function_format (&diagnostic_occurred, attrs, arglist);

  /* Restore the value of `pedantic'.  */
  pedantic = SAVE_pedantic;

  /* If calling `check_function_format_ptr' produces a warning, we
     return false, otherwise we return true.  */
  return ! diagnostic_occurred;
}

/* If the arguments passed to printf are suitable for optimizations,
   we attempt to transform the call.  */
static rtx
c_expand_builtin_printf (arglist, target, tmode, modifier, ignore, unlocked)
     tree arglist;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
     int ignore;
     int unlocked;
{
  tree fn_putchar = unlocked ?
    built_in_decls[BUILT_IN_PUTCHAR_UNLOCKED] : built_in_decls[BUILT_IN_PUTCHAR];
  tree fn_puts = unlocked ?
    built_in_decls[BUILT_IN_PUTS_UNLOCKED] : built_in_decls[BUILT_IN_PUTS];
  tree fn, format_arg, stripped_string;

  /* If the return value is used, or the replacement _DECL isn't
     initialized, don't do the transformation.  */
  if (!ignore || !fn_putchar || !fn_puts)
    return 0;

  /* Verify the required arguments in the original call.  */
  if (arglist == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE))
    return 0;
  
  /* Check the specifier vs. the parameters.  */
  if (!is_valid_printf_arglist (arglist))
    return 0;
  
  format_arg = TREE_VALUE (arglist);
  stripped_string = format_arg;
  STRIP_NOPS (stripped_string);
  if (stripped_string && TREE_CODE (stripped_string) == ADDR_EXPR)
    stripped_string = TREE_OPERAND (stripped_string, 0);

  /* If the format specifier isn't a STRING_CST, punt.  */
  if (TREE_CODE (stripped_string) != STRING_CST)
    return 0;
  
  /* OK!  We can attempt optimization.  */

  /* If the format specifier was "%s\n", call __builtin_puts(arg2).  */
  if (strcmp (TREE_STRING_POINTER (stripped_string), "%s\n") == 0)
    {
      arglist = TREE_CHAIN (arglist);
      fn = fn_puts;
    }
  /* If the format specifier was "%c", call __builtin_putchar (arg2).  */
  else if (strcmp (TREE_STRING_POINTER (stripped_string), "%c") == 0)
    {
      arglist = TREE_CHAIN (arglist);
      fn = fn_putchar;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (TREE_STRING_POINTER (stripped_string), '%'))
	return 0;
      
      /* If the resulting constant string has a length of 1, call
         putchar.  Note, TREE_STRING_LENGTH includes the terminating
         NULL in its count.  */
      if (TREE_STRING_LENGTH (stripped_string) == 2)
        {
	  /* Given printf("c"), (where c is any one character,)
             convert "c"[0] to an int and pass that to the replacement
             function.  */
	  arglist = build_int_2 (TREE_STRING_POINTER (stripped_string)[0], 0);
	  arglist = build_tree_list (NULL_TREE, arglist);
	  
	  fn = fn_putchar;
        }
      /* If the resulting constant was "string\n", call
         __builtin_puts("string").  Ensure "string" has at least one
         character besides the trailing \n.  Note, TREE_STRING_LENGTH
         includes the terminating NULL in its count.  */
      else if (TREE_STRING_LENGTH (stripped_string) > 2
	       && TREE_STRING_POINTER (stripped_string)
	       [TREE_STRING_LENGTH (stripped_string) - 2] == '\n')
        {
	  /* Create a NULL-terminated string that's one char shorter
	     than the original, stripping off the trailing '\n'.  */
	  const int newlen = TREE_STRING_LENGTH (stripped_string) - 1;
	  char *newstr = (char *) alloca (newlen);
	  memcpy (newstr, TREE_STRING_POINTER (stripped_string), newlen - 1);
	  newstr[newlen - 1] = 0;
	  
	  arglist = fix_string_type (build_string (newlen, newstr));
	  arglist = build_tree_list (NULL_TREE, arglist);
	  fn = fn_puts;
	}
      else
	/* We'd like to arrange to call fputs(string) here, but we
           need stdout and don't have a way to get it ... yet.  */
	return 0;
    }
  
  return expand_expr (build_function_call (fn, arglist),
		      (ignore ? const0_rtx : target),
		      tmode, modifier);
}

/* If the arguments passed to fprintf are suitable for optimizations,
   we attempt to transform the call.  */
static rtx
c_expand_builtin_fprintf (arglist, target, tmode, modifier, ignore, unlocked)
     tree arglist;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
     int ignore;
     int unlocked;
{
  tree fn_fputc = unlocked ?
    built_in_decls[BUILT_IN_FPUTC_UNLOCKED] : built_in_decls[BUILT_IN_FPUTC];
  tree fn_fputs = unlocked ?
    built_in_decls[BUILT_IN_FPUTS_UNLOCKED] : built_in_decls[BUILT_IN_FPUTS];
  tree fn, format_arg, stripped_string;

  /* If the return value is used, or the replacement _DECL isn't
     initialized, don't do the transformation.  */
  if (!ignore || !fn_fputc || !fn_fputs)
    return 0;

  /* Verify the required arguments in the original call.  */
  if (arglist == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
      || (TREE_CHAIN (arglist) == 0)
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) !=
	  POINTER_TYPE))
    return 0;
  
  /* Check the specifier vs. the parameters.  */
  if (!is_valid_printf_arglist (TREE_CHAIN (arglist)))
    return 0;
  
  format_arg = TREE_VALUE (TREE_CHAIN (arglist));
  stripped_string = format_arg;
  STRIP_NOPS (stripped_string);
  if (stripped_string && TREE_CODE (stripped_string) == ADDR_EXPR)
    stripped_string = TREE_OPERAND (stripped_string, 0);

  /* If the format specifier isn't a STRING_CST, punt.  */
  if (TREE_CODE (stripped_string) != STRING_CST)
    return 0;
  
  /* OK!  We can attempt optimization.  */

  /* If the format specifier was "%s", call __builtin_fputs(arg3, arg1).  */
  if (strcmp (TREE_STRING_POINTER (stripped_string), "%s") == 0)
    {
      tree newarglist = build_tree_list (NULL_TREE, TREE_VALUE (arglist));
      arglist = tree_cons (NULL_TREE,
			   TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))),
			   newarglist);
      fn = fn_fputs;
    }
  /* If the format specifier was "%c", call __builtin_fputc (arg3, arg1).  */
  else if (strcmp (TREE_STRING_POINTER (stripped_string), "%c") == 0)
    {
      tree newarglist = build_tree_list (NULL_TREE, TREE_VALUE (arglist));
      arglist = tree_cons (NULL_TREE,
			   TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist))),
			   newarglist);
      fn = fn_fputc;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (TREE_STRING_POINTER (stripped_string), '%'))
	return 0;
      
      /* When "string" doesn't contain %, replace all cases of
         fprintf(stream,string) with fputs(string,stream).  The fputs
         builtin will take take of special cases like length==1.  */
      arglist = tree_cons (NULL_TREE, TREE_VALUE (TREE_CHAIN (arglist)),
			   build_tree_list (NULL_TREE, TREE_VALUE (arglist)));
      fn = fn_fputs;
    }
  
  return expand_expr (build_function_call (fn, arglist),
		      (ignore ? const0_rtx : target),
		      tmode, modifier);
}


/* Given a boolean expression ARG, return a tree representing an increment
   or decrement (as indicated by CODE) of ARG.  The front end must check for
   invalid cases (e.g., decrement in C++).  */
tree
boolean_increment (code, arg)
     enum tree_code code;
     tree arg;
{
  tree val;
  tree true_res = (c_language == clk_cplusplus
		   ? boolean_true_node
		   : c_bool_true_node);
  arg = stabilize_reference (arg);
  switch (code)
    {
    case PREINCREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      break;
    case POSTINCREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      arg = save_expr (arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    case PREDECREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, invert_truthvalue (arg));
      break;
    case POSTDECREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, invert_truthvalue (arg));
      arg = save_expr (arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    default:
      abort ();
    }
  TREE_SIDE_EFFECTS (val) = 1;
  return val;
}

/* Define NAME with value TYPE precision.  */
static void
builtin_define_type_precision (name, type)
     const char *name;
     tree type;
{
  builtin_define_with_int_value (name, TYPE_PRECISION (type));
}

/* Define the float.h constants for TYPE using NAME_PREFIX and FP_SUFFIX.  */
static void
builtin_define_float_constants (name_prefix, fp_suffix, type)
     const char *name_prefix;
     const char *fp_suffix;
     tree type;
{
  /* Used to convert radix-based values to base 10 values in several cases.

     In the max_exp -> max_10_exp conversion for 128-bit IEEE, we need at
     least 6 significant digits for correct results.  Using the fraction
     formed by (log(2)*1e6)/(log(10)*1e6) overflows a 32-bit integer as an
     intermediate; perhaps someone can find a better approximation, in the
     mean time, I suspect using doubles won't harm the bootstrap here.  */

  const double log10_2 = .30102999566398119521;
  double log10_b;
  const struct real_format *fmt;

  char name[64], buf[128];
  int dig, min_10_exp, max_10_exp;
  int decimal_dig;

  fmt = real_format_for_mode[TYPE_MODE (type) - QFmode];

  /* The radix of the exponent representation.  */
  if (type == float_type_node)
    builtin_define_with_int_value ("__FLT_RADIX__", fmt->b);
  log10_b = log10_2 * fmt->log2_b;

  /* The number of radix digits, p, in the floating-point significand.  */
  sprintf (name, "__%s_MANT_DIG__", name_prefix);
  builtin_define_with_int_value (name, fmt->p);

  /* The number of decimal digits, q, such that any floating-point number
     with q decimal digits can be rounded into a floating-point number with
     p radix b digits and back again without change to the q decimal digits,

	p log10 b			if b is a power of 10
 	floor((p - 1) log10 b)		otherwise
  */
  dig = (fmt->p - 1) * log10_b;
  sprintf (name, "__%s_DIG__", name_prefix);
  builtin_define_with_int_value (name, dig);

  /* The minimum negative int x such that b**(x-1) is a normalized float.  */
  sprintf (name, "__%s_MIN_EXP__", name_prefix);
  sprintf (buf, "(%d)", fmt->emin);
  builtin_define_with_value (name, buf, 0);

  /* The minimum negative int x such that 10**x is a normalized float,

	  ceil (log10 (b ** (emin - 1)))
	= ceil (log10 (b) * (emin - 1))

     Recall that emin is negative, so the integer truncation calculates
     the ceiling, not the floor, in this case.  */
  min_10_exp = (fmt->emin - 1) * log10_b;
  sprintf (name, "__%s_MIN_10_EXP__", name_prefix);
  sprintf (buf, "(%d)", min_10_exp);
  builtin_define_with_value (name, buf, 0);

  /* The maximum int x such that b**(x-1) is a representable float.  */
  sprintf (name, "__%s_MAX_EXP__", name_prefix);
  builtin_define_with_int_value (name, fmt->emax);

  /* The maximum int x such that 10**x is in the range of representable
     finite floating-point numbers,

	  floor (log10((1 - b**-p) * b**emax))
	= floor (log10(1 - b**-p) + log10(b**emax))
	= floor (log10(1 - b**-p) + log10(b)*emax)

     The safest thing to do here is to just compute this number.  But since
     we don't link cc1 with libm, we cannot.  We could implement log10 here
     a series expansion, but that seems too much effort because:

     Note that the first term, for all extant p, is a number exceedingly close
     to zero, but slightly negative.  Note that the second term is an integer
     scaling an irrational number, and that because of the floor we are only
     interested in its integral portion.

     In order for the first term to have any effect on the integral portion
     of the second term, the second term has to be exceedingly close to an
     integer itself (e.g. 123.000000000001 or something).  Getting a result
     that close to an integer requires that the irrational multiplicand have
     a long series of zeros in its expansion, which doesn't occur in the
     first 20 digits or so of log10(b).

     Hand-waving aside, crunching all of the sets of constants above by hand
     does not yield a case for which the first term is significant, which
     in the end is all that matters.  */
  max_10_exp = fmt->emax * log10_b;
  sprintf (name, "__%s_MAX_10_EXP__", name_prefix);
  builtin_define_with_int_value (name, max_10_exp);

  /* The number of decimal digits, n, such that any floating-point number
     can be rounded to n decimal digits and back again without change to
     the value. 

	p * log10(b)			if b is a power of 10
	ceil(1 + p * log10(b))		otherwise

     The only macro we care about is this number for the widest supported
     floating type, but we want this value for rendering constants below.  */
  {
    double d_decimal_dig = 1 + fmt->p * log10_b;
    decimal_dig = d_decimal_dig;
    if (decimal_dig < d_decimal_dig)
      decimal_dig++;
  }
  if (type == long_double_type_node)
    builtin_define_with_int_value ("__DECIMAL_DIG__", decimal_dig);

  /* Since, for the supported formats, B is always a power of 2, we
     construct the following numbers directly as a hexadecimal
     constants.  */

  /* The maximum representable finite floating-point number,
     (1 - b**-p) * b**emax  */
  {
    int i, n;
    char *p;

    strcpy (buf, "0x0.");
    n = fmt->p * fmt->log2_b;
    for (i = 0, p = buf + 4; i + 3 < n; i += 4)
      *p++ = 'f';
    if (i < n)
      *p++ = "08ce"[n - i];
    sprintf (p, "p%d", fmt->emax * fmt->log2_b);
  }
  sprintf (name, "__%s_MAX__", name_prefix);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix);

  /* The minimum normalized positive floating-point number,
     b**(emin-1).  */
  sprintf (name, "__%s_MIN__", name_prefix);
  sprintf (buf, "0x1p%d", (fmt->emin - 1) * fmt->log2_b);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix);

  /* The difference between 1 and the least value greater than 1 that is
     representable in the given floating point type, b**(1-p).  */
  sprintf (name, "__%s_EPSILON__", name_prefix);
  sprintf (buf, "0x1p%d", (1 - fmt->p) * fmt->log2_b);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix);

  /* For C++ std::numeric_limits<T>::denorm_min.  The minimum denormalized
     positive floating-point number, b**(emin-p).  Zero for formats that
     don't support denormals.  */
  sprintf (name, "__%s_DENORM_MIN__", name_prefix);
  if (fmt->has_denorm)
    {
      sprintf (buf, "0x1p%d", (fmt->emin - fmt->p) * fmt->log2_b);
      builtin_define_with_hex_fp_value (name, type, decimal_dig,
					buf, fp_suffix);
    }
  else
    {
      sprintf (buf, "0.0%s", fp_suffix);
      builtin_define_with_value (name, buf, 0);
    }
}

/* Hook that registers front end and target-specific built-ins.  */
void
cb_register_builtins (pfile)
     cpp_reader *pfile;
{
  /* -undef turns off target-specific built-ins.  */
  if (flag_undef)
    return;

  if (c_language == clk_cplusplus)
    {
      if (SUPPORTS_ONE_ONLY)
	cpp_define (pfile, "__GXX_WEAK__=1");
      else
	cpp_define (pfile, "__GXX_WEAK__=0");
      if (warn_deprecated)
	cpp_define (pfile, "__DEPRECATED");
    }
  if (flag_exceptions)
    cpp_define (pfile, "__EXCEPTIONS");

  /* represents the C++ ABI version, always defined so it can be used while
     preprocessing C and assembler.  */
  cpp_define (pfile, "__GXX_ABI_VERSION=102");

  /* libgcc needs to know this.  */
  if (USING_SJLJ_EXCEPTIONS)
    cpp_define (pfile, "__USING_SJLJ_EXCEPTIONS__");

  /* stddef.h needs to know these.  */
  builtin_define_with_value ("__SIZE_TYPE__", SIZE_TYPE, 0);
  builtin_define_with_value ("__PTRDIFF_TYPE__", PTRDIFF_TYPE, 0);
  builtin_define_with_value ("__WCHAR_TYPE__", MODIFIED_WCHAR_TYPE, 0);
  builtin_define_with_value ("__WINT_TYPE__", WINT_TYPE, 0);

  /* limits.h needs to know these.  */
  builtin_define_type_max ("__SCHAR_MAX__", signed_char_type_node, 0);
  builtin_define_type_max ("__SHRT_MAX__", short_integer_type_node, 0);
  builtin_define_type_max ("__INT_MAX__", integer_type_node, 0);
  builtin_define_type_max ("__LONG_MAX__", long_integer_type_node, 1);
  builtin_define_type_max ("__LONG_LONG_MAX__", long_long_integer_type_node, 2);
  builtin_define_type_max ("__WCHAR_MAX__", wchar_type_node, 0);

  builtin_define_type_precision ("__CHAR_BIT__", char_type_node);

  /* float.h needs to know these.  */

  builtin_define_with_int_value ("__FLT_EVAL_METHOD__",
				 TARGET_FLT_EVAL_METHOD);

  builtin_define_float_constants ("FLT", "F", float_type_node);
  builtin_define_float_constants ("DBL", "", double_type_node);
  builtin_define_float_constants ("LDBL", "L", long_double_type_node);

  /* For use in assembly language.  */
  builtin_define_with_value ("__REGISTER_PREFIX__", REGISTER_PREFIX, 0);
  builtin_define_with_value ("__USER_LABEL_PREFIX__", user_label_prefix, 0);

  /* Misc.  */
  builtin_define_with_value ("__VERSION__", version_string, 1);

  /* Other target-independent built-ins determined by command-line
     options.  */
  if (optimize_size)
    cpp_define (pfile, "__OPTIMIZE_SIZE__");
  if (optimize)
    cpp_define (pfile, "__OPTIMIZE__");

  if (flag_hosted)
    cpp_define (pfile, "__STDC_HOSTED__=1");
  else
    cpp_define (pfile, "__STDC_HOSTED__=0");

  if (fast_math_flags_set_p ())
    cpp_define (pfile, "__FAST_MATH__");
  if (flag_really_no_inline)
    cpp_define (pfile, "__NO_INLINE__");
  if (flag_signaling_nans)
    cpp_define (pfile, "__SUPPORT_SNAN__");
  if (flag_finite_math_only)
    cpp_define (pfile, "__FINITE_MATH_ONLY__=1");
  else
    cpp_define (pfile, "__FINITE_MATH_ONLY__=0");

  if (flag_iso)
    cpp_define (pfile, "__STRICT_ANSI__");

  if (!flag_signed_char)
    cpp_define (pfile, "__CHAR_UNSIGNED__");

  if (c_language == clk_cplusplus && TREE_UNSIGNED (wchar_type_node))
    cpp_define (pfile, "__WCHAR_UNSIGNED__");

  /* Make the choice of ObjC runtime visible to source code.  */
  if (flag_objc && flag_next_runtime)
    cpp_define (pfile, "__NEXT_RUNTIME__");

  /* A straightforward target hook doesn't work, because of problems
     linking that hook's body when part of non-C front ends.  */
# define preprocessing_asm_p() (cpp_get_options (pfile)->lang == CLK_ASM)
# define preprocessing_trad_p() (cpp_get_options (pfile)->traditional)
# define builtin_define(TXT) cpp_define (pfile, TXT)
# define builtin_assert(TXT) cpp_assert (pfile, TXT)
  TARGET_CPU_CPP_BUILTINS ();
  TARGET_OS_CPP_BUILTINS ();
}

/* Pass an object-like macro.  If it doesn't lie in the user's
   namespace, defines it unconditionally.  Otherwise define a version
   with two leading underscores, and another version with two leading
   and trailing underscores, and define the original only if an ISO
   standard was not nominated.

   e.g. passing "unix" defines "__unix", "__unix__" and possibly
   "unix".  Passing "_mips" defines "__mips", "__mips__" and possibly
   "_mips".  */
void
builtin_define_std (macro)
     const char *macro;
{
  size_t len = strlen (macro);
  char *buff = alloca (len + 5);
  char *p = buff + 2;
  char *q = p + len;

  /* prepend __ (or maybe just _) if in user's namespace.  */
  memcpy (p, macro, len + 1);
  if (!( *p == '_' && (p[1] == '_' || ISUPPER (p[1]))))
    {
      if (*p != '_')
	*--p = '_';
      if (p[1] != '_')
	*--p = '_';
    }
  cpp_define (parse_in, p);

  /* If it was in user's namespace...  */
  if (p != buff + 2)
    {
      /* Define the macro with leading and following __.  */
      if (q[-1] != '_')
	*q++ = '_';
      if (q[-2] != '_')
	*q++ = '_';
      *q = '\0';
      cpp_define (parse_in, p);

      /* Finally, define the original macro if permitted.  */
      if (!flag_iso)
	cpp_define (parse_in, macro);
    }
}

/* Pass an object-like macro and a value to define it to.  The third
   parameter says whether or not to turn the value into a string
   constant.  */
static void
builtin_define_with_value (macro, expansion, is_str)
     const char *macro;
     const char *expansion;
     int is_str;
{
  char *buf;
  size_t mlen = strlen (macro);
  size_t elen = strlen (expansion);
  size_t extra = 2;  /* space for an = and a NUL */

  if (is_str)
    extra += 2;  /* space for two quote marks */

  buf = alloca (mlen + elen + extra);
  if (is_str)
    sprintf (buf, "%s=\"%s\"", macro, expansion);
  else
    sprintf (buf, "%s=%s", macro, expansion);

  cpp_define (parse_in, buf);
}

/* Pass an object-like macro and an integer value to define it to.  */
static void
builtin_define_with_int_value (macro, value)
     const char *macro;
     HOST_WIDE_INT value;
{
  char *buf;
  size_t mlen = strlen (macro);
  size_t vlen = 18;
  size_t extra = 2; /* space for = and NUL.  */

  buf = alloca (mlen + vlen + extra);
  memcpy (buf, macro, mlen);
  buf[mlen] = '=';
  sprintf (buf + mlen + 1, HOST_WIDE_INT_PRINT_DEC, value);

  cpp_define (parse_in, buf);
}

/* Pass an object-like macro a hexadecimal floating-point value.  */
static void
builtin_define_with_hex_fp_value (macro, type, digits, hex_str, fp_suffix)
     const char *macro;
     tree type ATTRIBUTE_UNUSED;
     int digits;
     const char *hex_str;
     const char *fp_suffix;
{
  REAL_VALUE_TYPE real;
  char dec_str[64], buf[256];

  /* Hex values are really cool and convenient, except that they're
     not supported in strict ISO C90 mode.  First, the "p-" sequence
     is not valid as part of a preprocessor number.  Second, we get a
     pedwarn from the preprocessor, which has no context, so we can't
     suppress the warning with __extension__.

     So instead what we do is construct the number in hex (because 
     it's easy to get the exact correct value), parse it as a real,
     then print it back out as decimal.  */

  real_from_string (&real, hex_str);
  real_to_decimal (dec_str, &real, sizeof (dec_str), digits, 0);

  sprintf (buf, "%s=%s%s", macro, dec_str, fp_suffix);
  cpp_define (parse_in, buf);
}

/* Define MAX for TYPE based on the precision of the type.  IS_LONG is
   1 for type "long" and 2 for "long long".  We have to handle
   unsigned types, since wchar_t might be unsigned.  */

static void
builtin_define_type_max (macro, type, is_long)
     const char *macro;
     tree type;
     int is_long;
{
  static const char *const values[]
    = { "127", "255",
	"32767", "65535",
	"2147483647", "4294967295",
	"9223372036854775807", "18446744073709551615",
	"170141183460469231731687303715884105727",
	"340282366920938463463374607431768211455" };
  static const char *const suffixes[] = { "", "U", "L", "UL", "LL", "ULL" };

  const char *value, *suffix;
  char *buf;
  size_t idx;

  /* Pre-rendering the values mean we don't have to futz with printing a
     multi-word decimal value.  There are also a very limited number of
     precisions that we support, so it's really a waste of time.  */
  switch (TYPE_PRECISION (type))
    {
    case 8:	idx = 0; break;
    case 16:	idx = 2; break;
    case 32:	idx = 4; break;
    case 64:	idx = 6; break;
    case 128:	idx = 8; break;
    default:    abort ();
    }

  value = values[idx + TREE_UNSIGNED (type)];
  suffix = suffixes[is_long * 2 + TREE_UNSIGNED (type)];

  buf = alloca (strlen (macro) + 1 + strlen (value) + strlen (suffix) + 1);
  sprintf (buf, "%s=%s%s", macro, value, suffix);

  cpp_define (parse_in, buf);
}

static void
c_init_attributes ()
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)		\
  built_in_attributes[(int) ENUM] = NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)					     \
  built_in_attributes[(int) ENUM] = build_int_2 (VALUE, VALUE < 0 ? -1 : 0);
#define DEF_ATTR_IDENT(ENUM, STRING)				\
  built_in_attributes[(int) ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
  built_in_attributes[(int) ENUM]			\
    = tree_cons (built_in_attributes[(int) PURPOSE],	\
		 built_in_attributes[(int) VALUE],	\
		 built_in_attributes[(int) CHAIN]);
#define DEF_FN_ATTR(NAME, ATTRS, PREDICATE) /* No initialization needed.  */
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
#undef DEF_FN_ATTR
  c_attrs_initialized = true;
}

/* Depending on the name of DECL, apply default attributes to it.  */

void
c_common_insert_default_attributes (decl)
     tree decl;
{
  tree name = DECL_NAME (decl);

  if (!c_attrs_initialized)
    c_init_attributes ();

#define DEF_ATTR_NULL_TREE(ENUM) /* Nothing needed after initialization.  */
#define DEF_ATTR_INT(ENUM, VALUE)
#define DEF_ATTR_IDENT(ENUM, STRING)
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)
#define DEF_FN_ATTR(NAME, ATTRS, PREDICATE)			\
  if ((PREDICATE) && name == built_in_attributes[(int) NAME])	\
    decl_attributes (&decl, built_in_attributes[(int) ATTRS],	\
		     ATTR_FLAG_BUILT_IN);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
#undef DEF_FN_ATTR
}

/* Output a -Wshadow warning MSGID about NAME, an IDENTIFIER_NODE, and
   additionally give the location of the previous declaration DECL.  */
void
shadow_warning (msgid, name, decl)
     const char *msgid;
     tree name, decl;
{
  warning ("declaration of `%s' shadows %s", IDENTIFIER_POINTER (name), msgid);
  warning_with_file_and_line (DECL_SOURCE_FILE (decl),
			      DECL_SOURCE_LINE (decl),
			      "shadowed declaration is here");
}

/* Attribute handlers common to C front ends.  */

/* Handle a "packed" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_packed_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  tree *type = NULL;
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	type = &TREE_TYPE (*node);
    }
  else
    type = node;

  if (type)
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);
      TYPE_PACKED (*type) = 1;
    }
  else if (TREE_CODE (*node) == FIELD_DECL)
    DECL_PACKED (*node) = 1;
  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
     used for DECL_REGISTER.  It wouldn't mean anything anyway.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nocommon" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nocommon_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 0;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "common" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_common_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_THIS_VOLATILE (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type),
			     TREE_READONLY (TREE_TYPE (type)), 1));
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noinline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noinline_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_UNINLINABLE (*node) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "always_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_always_inline_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      /* Do nothing else, just set the attribute.  We'll get at
	 it later with lookup_attribute.  */
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "used" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_used_attribute (pnode, name, args, flags, no_add_attrs)
     tree *pnode;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree node = *pnode;

  if (TREE_CODE (node) == FUNCTION_DECL
      || (TREE_CODE (node) == VAR_DECL && TREE_STATIC (node)))
    TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (node))
      = TREE_USED (node) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "unused" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_unused_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  if (DECL_P (*node))
    {
      tree decl = *node;

      if (TREE_CODE (decl) == PARM_DECL
	  || TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == FUNCTION_DECL
	  || TREE_CODE (decl) == LABEL_DECL
	  || TREE_CODE (decl) == TYPE_DECL)
	TREE_USED (decl) = 1;
      else
	{
	  warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
    }
  else
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_type_copy (*node);
      TREE_USED (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_const_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment on noreturn in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_READONLY (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type), 1,
			     TREE_THIS_VOLATILE (TREE_TYPE (type))));
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "transparent_union" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_transparent_union_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  int is_type = 0;

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = 1;

  if (is_type
      && TREE_CODE (*type) == UNION_TYPE
      && (decl == 0
	  || (TYPE_FIELDS (*type) != 0
	      && TYPE_MODE (*type) == DECL_MODE (TYPE_FIELDS (*type)))))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);
      TYPE_TRANSPARENT_UNION (*type) = 1;
    }
  else if (decl != 0 && TREE_CODE (decl) == PARM_DECL
	   && TREE_CODE (*type) == UNION_TYPE
	   && TYPE_MODE (*type) == DECL_MODE (TYPE_FIELDS (*type)))
    DECL_TRANSPARENT_UNION (decl) = 1;
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "constructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_constructor_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      TREE_USED (decl) = 1;
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "destructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_destructor_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      DECL_STATIC_DESTRUCTOR (decl) = 1;
      TREE_USED (decl) = 1;
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "mode" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_mode_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = *node;

  *no_add_attrs = true;

  if (TREE_CODE (TREE_VALUE (args)) != IDENTIFIER_NODE)
    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
  else
    {
      int j;
      const char *p = IDENTIFIER_POINTER (TREE_VALUE (args));
      int len = strlen (p);
      enum machine_mode mode = VOIDmode;
      tree typefm;

      if (len > 4 && p[0] == '_' && p[1] == '_'
	  && p[len - 1] == '_' && p[len - 2] == '_')
	{
	  char *newp = (char *) alloca (len - 1);

	  strcpy (newp, &p[2]);
	  newp[len - 4] = '\0';
	  p = newp;
	}

      /* Change this type to have a type with the specified mode.
	 First check for the special modes.  */
      if (! strcmp (p, "byte"))
	mode = byte_mode;
      else if (!strcmp (p, "word"))
	mode = word_mode;
      else if (! strcmp (p, "pointer"))
	mode = ptr_mode;
      else
	for (j = 0; j < NUM_MACHINE_MODES; j++)
	  if (!strcmp (p, GET_MODE_NAME (j)))
	    mode = (enum machine_mode) j;

      if (mode == VOIDmode)
	error ("unknown machine mode `%s'", p);
      else if (0 == (typefm = (*lang_hooks.types.type_for_mode)
		     (mode, TREE_UNSIGNED (type))))
	error ("no data type for mode `%s'", p);
      else
	{
	  /* If this is a vector, make sure we either have hardware
	     support, or we can emulate it.  */
	  if ((GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	       || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	      && !vector_mode_valid_p (mode))
	    {
	      error ("unable to emulate '%s'", GET_MODE_NAME (mode));
	      return NULL_TREE;
	    }

	  *node = typefm;
	  /* No need to layout the type here.  The caller should do this.  */
	}
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_section_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (targetm.have_named_sections)
    {
      if ((TREE_CODE (decl) == FUNCTION_DECL
	   || TREE_CODE (decl) == VAR_DECL)
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  if (TREE_CODE (decl) == VAR_DECL
	      && current_function_decl != NULL_TREE
	      && ! TREE_STATIC (decl))
	    {
	      error_with_decl (decl,
			       "section attribute cannot be specified for local variables");
	      *no_add_attrs = true;
	    }

	  /* The decl may have already been given a section attribute
	     from a previous declaration.  Ensure they match.  */
	  else if (DECL_SECTION_NAME (decl) != NULL_TREE
		   && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			      TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
	    {
	      error_with_decl (*node,
			       "section of `%s' conflicts with previous declaration");
	      *no_add_attrs = true;
	    }
	  else
	    DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	}
      else
	{
	  error_with_decl (*node,
			   "section attribute not allowed for `%s'");
	  *no_add_attrs = true;
	}
    }
  else
    {
      error_with_decl (*node,
		       "section attributes are not supported for this target");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_aligned_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags;
     bool *no_add_attrs;
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  int is_type = 0;
  tree align_expr = (args ? TREE_VALUE (args)
		     : size_int (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
  int i;

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = 1;

  /* Strip any NOPs of any kind.  */
  while (TREE_CODE (align_expr) == NOP_EXPR
	 || TREE_CODE (align_expr) == CONVERT_EXPR
	 || TREE_CODE (align_expr) == NON_LVALUE_EXPR)
    align_expr = TREE_OPERAND (align_expr, 0);

  if (TREE_CODE (align_expr) != INTEGER_CST)
    {
      error ("requested alignment is not a constant");
      *no_add_attrs = true;
    }
  else if ((i = tree_log2 (align_expr)) == -1)
    {
      error ("requested alignment is not a power of 2");
      *no_add_attrs = true;
    }
  else if (i > HOST_BITS_PER_INT - 2)
    {
      error ("requested alignment is too large");
      *no_add_attrs = true;
    }
  else if (is_type)
    {
      /* If we have a TYPE_DECL, then copy the type, so that we
	 don't accidentally modify a builtin type.  See pushdecl.  */
      if (decl && TREE_TYPE (decl) != error_mark_node
	  && DECL_ORIGINAL_TYPE (decl) == NULL_TREE)
	{
	  tree tt = TREE_TYPE (decl);
	  *type = build_type_copy (*type);
	  DECL_ORIGINAL_TYPE (decl) = tt;
	  TYPE_NAME (*type) = decl;
	  TREE_USED (*type) = TREE_USED (decl);
	  TREE_TYPE (decl) = *type;
	}
      else if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_type_copy (*type);

      TYPE_ALIGN (*type) = (1 << i) * BITS_PER_UNIT;
      TYPE_USER_ALIGN (*type) = 1;
    }
  else if (TREE_CODE (decl) != VAR_DECL
	   && TREE_CODE (decl) != FIELD_DECL)
    {
      error_with_decl (decl,
		       "alignment may not be specified for `%s'");
      *no_add_attrs = true;
    }
  else
    {
      DECL_ALIGN (decl) = (1 << i) * BITS_PER_UNIT;
      DECL_USER_ALIGN (decl) = 1;
    }

  return NULL_TREE;
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_weak_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs ATTRIBUTE_UNUSED;
{
  declare_weak (*node);

  return NULL_TREE;
}

/* Handle an "alias" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alias_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
      || (TREE_CODE (decl) != FUNCTION_DECL && ! DECL_EXTERNAL (decl)))
    {
      error_with_decl (decl,
		       "`%s' defined both normally and as an alias");
      *no_add_attrs = true;
    }
  else if (decl_function_context (decl) == 0)
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("alias arg not a string");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      id = get_identifier (TREE_STRING_POINTER (id));
      /* This counts as a use of the object pointed to.  */
      TREE_USED (id) = 1;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	DECL_INITIAL (decl) = error_mark_node;
      else
	DECL_EXTERNAL (decl) = 0;
    }
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "visibility" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_visibility_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (decl_function_context (decl) != 0 || ! TREE_PUBLIC (decl))
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("visibility arg not a string");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      if (strcmp (TREE_STRING_POINTER (id), "hidden")
	  && strcmp (TREE_STRING_POINTER (id), "protected")
	  && strcmp (TREE_STRING_POINTER (id), "internal")
	  && strcmp (TREE_STRING_POINTER (id), "default"))
	{
	  error ("visibility arg must be one of \"default\", \"hidden\", \"protected\" or \"internal\"");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  return NULL_TREE;
}

/* Handle an "tls_model" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_tls_model_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (! DECL_THREAD_LOCAL (decl))
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("tls_model arg not a string");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      if (strcmp (TREE_STRING_POINTER (id), "local-exec")
	  && strcmp (TREE_STRING_POINTER (id), "initial-exec")
	  && strcmp (TREE_STRING_POINTER (id), "local-dynamic")
	  && strcmp (TREE_STRING_POINTER (id), "global-dynamic"))
	{
	  error ("tls_model arg must be one of \"local-exec\", \"initial-exec\", \"local-dynamic\" or \"global-dynamic\"");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  return NULL_TREE;
}

/* Handle a "no_instrument_function" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_instrument_function_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_with_decl (decl,
		       "`%s' attribute applies only to functions",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_with_decl (decl,
		       "can't set `%s' attribute after definition",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;

  return NULL_TREE;
}

/* Handle a "malloc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_malloc_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_MALLOC (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_limit_stack" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_limit_stack_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_with_decl (decl,
		       "`%s' attribute applies only to functions",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_with_decl (decl,
		       "can't set `%s' attribute after definition",
		       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    DECL_NO_LIMIT_STACK (decl) = 1;

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_PURE (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "cleanup" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_cleanup_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree decl = *node;
  tree cleanup_id, cleanup_decl;

  /* ??? Could perhaps support cleanups on TREE_STATIC, much like we do
     for global destructors in C++.  This requires infrastructure that
     we don't have generically at the moment.  It's also not a feature
     we'd be missing too much, since we do have attribute constructor.  */
  if (TREE_CODE (decl) != VAR_DECL || TREE_STATIC (decl))
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Verify that the argument is a function in scope.  */
  /* ??? We could support pointers to functions here as well, if
     that was considered desirable.  */
  cleanup_id = TREE_VALUE (args);
  if (TREE_CODE (cleanup_id) != IDENTIFIER_NODE)
    {
      error ("cleanup arg not an identifier");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  cleanup_decl = lookup_name (cleanup_id);
  if (!cleanup_decl || TREE_CODE (cleanup_decl) != FUNCTION_DECL)
    {
      error ("cleanup arg not a function");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* That the function has proper type is checked with the 
     eventual call to build_function_call.  */

  return NULL_TREE;
}

/* Handle a "deprecated" attribute; arguments as in
   struct attribute_spec.handler.  */
   
static tree
handle_deprecated_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags;
     bool *no_add_attrs;
{
  tree type = NULL_TREE;
  int warn = 0;
  const char *what = NULL;
  
  if (DECL_P (*node))
    {
      tree decl = *node;
      type = TREE_TYPE (decl);
      
      if (TREE_CODE (decl) == TYPE_DECL
	  || TREE_CODE (decl) == PARM_DECL
	  || TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == FUNCTION_DECL
	  || TREE_CODE (decl) == FIELD_DECL)
	TREE_DEPRECATED (decl) = 1;
      else
	warn = 1;
    }
  else if (TYPE_P (*node))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_type_copy (*node);
      TREE_DEPRECATED (*node) = 1;
      type = *node;
    }
  else
    warn = 1;
  
  if (warn)
    {
      *no_add_attrs = true;
      if (type && TYPE_NAME (type))
	{
	  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	    what = IDENTIFIER_POINTER (TYPE_NAME (*node));
	  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (type)))
	    what = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	}
      if (what)
	warning ("`%s' attribute ignored for `%s'",
		  IDENTIFIER_POINTER (name), what);
      else
	warning ("`%s' attribute ignored", 
		      IDENTIFIER_POINTER (name));
    }

  return NULL_TREE;
}

/* Keep a list of vector type nodes we created in handle_vector_size_attribute,
   to prevent us from duplicating type nodes unnecessarily.
   The normal mechanism to prevent duplicates is to use type_hash_canon, but
   since we want to distinguish types that are essentially identical (except
   for their debug representation), we use a local list here.  */
static GTY(()) tree vector_type_node_list = 0;

/* Handle a "vector_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_vector_size_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  unsigned HOST_WIDE_INT vecsize, nunits;
  enum machine_mode mode, orig_mode, new_mode;
  tree type = *node, new_type = NULL_TREE;
  tree type_list_node;

  *no_add_attrs = true;

  if (! host_integerp (TREE_VALUE (args), 1))
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      return NULL_TREE;
    }

  /* Get the vector size (in bytes).  */
  vecsize = tree_low_cst (TREE_VALUE (args), 1);

  /* We need to provide for vector pointers, vector arrays, and
     functions returning vectors.  For example:

       __attribute__((vector_size(16))) short *foo;

     In this case, the mode is SI, but the type being modified is
     HI, so we need to look further.  */

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  /* Get the mode of the type being modified.  */
  orig_mode = TYPE_MODE (type);

  if (TREE_CODE (type) == RECORD_TYPE
      || (GET_MODE_CLASS (orig_mode) != MODE_FLOAT
	  && GET_MODE_CLASS (orig_mode) != MODE_INT)
      || ! host_integerp (TYPE_SIZE_UNIT (type), 1))
    {
      error ("invalid vector type for attribute `%s'",
	     IDENTIFIER_POINTER (name));
      return NULL_TREE;
    }

  /* Calculate how many units fit in the vector.  */
  nunits = vecsize / tree_low_cst (TYPE_SIZE_UNIT (type), 1);

  /* Find a suitably sized vector.  */
  new_mode = VOIDmode;
  for (mode = GET_CLASS_NARROWEST_MODE (GET_MODE_CLASS (orig_mode) == MODE_INT
					? MODE_VECTOR_INT
					: MODE_VECTOR_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (vecsize == GET_MODE_SIZE (mode)
	&& nunits == (unsigned HOST_WIDE_INT) GET_MODE_NUNITS (mode))
      {
	new_mode = mode;
	break;
      }

    if (new_mode == VOIDmode)
    {
      error ("no vector mode with the size and type specified could be found");
      return NULL_TREE;
    }

  for (type_list_node = vector_type_node_list; type_list_node;
       type_list_node = TREE_CHAIN (type_list_node))
    {
      tree other_type = TREE_VALUE (type_list_node);
      tree record = TYPE_DEBUG_REPRESENTATION_TYPE (other_type);
      tree fields = TYPE_FIELDS (record);
      tree field_type = TREE_TYPE (fields);
      tree array_type = TREE_TYPE (field_type);
      if (TREE_CODE (fields) != FIELD_DECL
	  || TREE_CODE (field_type) != ARRAY_TYPE)
	abort ();

      if (TYPE_MODE (other_type) == mode && type == array_type)
	{
	  new_type = other_type;
	  break;
	}
    }

  if (new_type == NULL_TREE)
    {
      tree index, array, rt, list_node;

      new_type = (*lang_hooks.types.type_for_mode) (new_mode,
						    TREE_UNSIGNED (type));

      if (!new_type)
	{
	  error ("no vector mode with the size and type specified could be found");
	  return NULL_TREE;
	}

      new_type = build_type_copy (new_type);

      /* If this is a vector, make sure we either have hardware
         support, or we can emulate it.  */
      if ((GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	   || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
	  && !vector_mode_valid_p (mode))
	{
	  error ("unable to emulate '%s'", GET_MODE_NAME (mode));
	  return NULL_TREE;
	}

      /* Set the debug information here, because this is the only
	 place where we know the underlying type for a vector made
	 with vector_size.  For debugging purposes we pretend a vector
	 is an array within a structure.  */
      index = build_int_2 (TYPE_VECTOR_SUBPARTS (new_type) - 1, 0);
      array = build_array_type (type, build_index_type (index));
      rt = make_node (RECORD_TYPE);

      TYPE_FIELDS (rt) = build_decl (FIELD_DECL, get_identifier ("f"), array);
      DECL_CONTEXT (TYPE_FIELDS (rt)) = rt;
      layout_type (rt);
      TYPE_DEBUG_REPRESENTATION_TYPE (new_type) = rt;

      list_node = build_tree_list (NULL, new_type);
      TREE_CHAIN (list_node) = vector_type_node_list;
      vector_type_node_list = list_node;
    }

  /* Build back pointers if needed.  */
  *node = vector_size_helper (*node, new_type);

  return NULL_TREE;
}

/* HACK.  GROSS.  This is absolutely disgusting.  I wish there was a
   better way.

   If we requested a pointer to a vector, build up the pointers that
   we stripped off while looking for the inner type.  Similarly for
   return values from functions.

   The argument "type" is the top of the chain, and "bottom" is the
   new type which we will point to.  */

static tree
vector_size_helper (type, bottom)
     tree type, bottom;
{
  tree inner, outer;

  if (POINTER_TYPE_P (type))
    {
      inner = vector_size_helper (TREE_TYPE (type), bottom);
      outer = build_pointer_type (inner);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      inner = vector_size_helper (TREE_TYPE (type), bottom);
      outer = build_array_type (inner, TYPE_VALUES (type));
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      inner = vector_size_helper (TREE_TYPE (type), bottom);
      outer = build_function_type (inner, TYPE_VALUES (type));
    }
  else
    return bottom;
  
  TREE_READONLY (outer) = TREE_READONLY (type);
  TREE_THIS_VOLATILE (outer) = TREE_THIS_VOLATILE (type);

  return outer;
}

/* Handle the "nonnull" attribute.  */
static tree
handle_nonnull_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  tree type = *node;
  unsigned HOST_WIDE_INT attr_arg_num;

  /* If no arguments are specified, all pointer arguments should be
     non-null.  Veryify a full prototype is given so that the arguments
     will have the correct types when we actually check them later.  */
  if (! args)
    {
      if (! TYPE_ARG_TYPES (type))
	{
	  error ("nonnull attribute without arguments on a non-prototype");
          *no_add_attrs = true;
	}
      return NULL_TREE;
    }

  /* Argument list specified.  Verify that each argument number references
     a pointer argument.  */
  for (attr_arg_num = 1; args; args = TREE_CHAIN (args))
    {
      tree argument;
      unsigned HOST_WIDE_INT arg_num, ck_num;

      if (! get_nonnull_operand (TREE_VALUE (args), &arg_num))
	{
	  error ("nonnull argument has invalid operand number (arg %lu)",
		 (unsigned long) attr_arg_num);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      argument = TYPE_ARG_TYPES (type);
      if (argument)
	{
	  for (ck_num = 1; ; ck_num++)
	    {
	      if (! argument || ck_num == arg_num)
		break;
	      argument = TREE_CHAIN (argument);
	    }

          if (! argument
	      || TREE_CODE (TREE_VALUE (argument)) == VOID_TYPE)
	    {
	      error ("nonnull argument with out-of-range operand number (arg %lu, operand %lu)",
		     (unsigned long) attr_arg_num, (unsigned long) arg_num);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }

          if (TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE)
	    {
	      error ("nonnull argument references non-pointer operand (arg %lu, operand %lu)",
		   (unsigned long) attr_arg_num, (unsigned long) arg_num);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}
    }

  return NULL_TREE;
}

/* Check the argument list of a function call for null in argument slots
   that are marked as requiring a non-null pointer argument.  */

static void
check_function_nonnull (attrs, params)
     tree attrs;
     tree params;
{
  tree a, args, param;
  int param_num;

  for (a = attrs; a; a = TREE_CHAIN (a))
    {
      if (is_attribute_p ("nonnull", TREE_PURPOSE (a)))
	{
          args = TREE_VALUE (a);

          /* Walk the argument list.  If we encounter an argument number we
             should check for non-null, do it.  If the attribute has no args,
             then every pointer argument is checked (in which case the check
	     for pointer type is done in check_nonnull_arg).  */
          for (param = params, param_num = 1; ;
               param_num++, param = TREE_CHAIN (param))
            {
              if (! param)
        	break;
              if (! args || nonnull_check_p (args, param_num))
        	check_function_arguments_recurse (check_nonnull_arg, NULL,
        					  TREE_VALUE (param),
        					  param_num);
            }
	}
    }
}

/* Helper for check_function_nonnull; given a list of operands which
   must be non-null in ARGS, determine if operand PARAM_NUM should be
   checked.  */

static bool
nonnull_check_p (args, param_num)
     tree args;
     unsigned HOST_WIDE_INT param_num;
{
  unsigned HOST_WIDE_INT arg_num;

  for (; args; args = TREE_CHAIN (args))
    {
      if (! get_nonnull_operand (TREE_VALUE (args), &arg_num))
        abort ();

      if (arg_num == param_num)
	return true;
    }
  return false;
}

/* Check that the function argument PARAM (which is operand number
   PARAM_NUM) is non-null.  This is called by check_function_nonnull
   via check_function_arguments_recurse.  */

static void
check_nonnull_arg (ctx, param, param_num)
     void *ctx ATTRIBUTE_UNUSED;
     tree param;
     unsigned HOST_WIDE_INT param_num;
{
  /* Just skip checking the argument if it's not a pointer.  This can
     happen if the "nonnull" attribute was given without an operand
     list (which means to check every pointer argument).  */

  if (TREE_CODE (TREE_TYPE (param)) != POINTER_TYPE)
    return;

  if (integer_zerop (param))
    warning ("null argument where non-null required (arg %lu)",
             (unsigned long) param_num);
}

/* Helper for nonnull attribute handling; fetch the operand number
   from the attribute argument list.  */

static bool
get_nonnull_operand (arg_num_expr, valp)
     tree arg_num_expr;
     unsigned HOST_WIDE_INT *valp;
{
  /* Strip any conversions from the arg number and verify they
     are constants.  */
  while (TREE_CODE (arg_num_expr) == NOP_EXPR
	 || TREE_CODE (arg_num_expr) == CONVERT_EXPR
	 || TREE_CODE (arg_num_expr) == NON_LVALUE_EXPR)
    arg_num_expr = TREE_OPERAND (arg_num_expr, 0);

  if (TREE_CODE (arg_num_expr) != INTEGER_CST
      || TREE_INT_CST_HIGH (arg_num_expr) != 0)
    return false;

  *valp = TREE_INT_CST_LOW (arg_num_expr);
  return true;
}

/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nothrow_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_NOTHROW (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Check for valid arguments being passed to a function.  */
void
check_function_arguments (attrs, params)
     tree attrs;
     tree params;
{
  /* Check for null being passed in a pointer argument that must be
     non-null.  We also need to do this if format checking is enabled.  */

  if (warn_nonnull)
    check_function_nonnull (attrs, params);

  /* Check for errors in format strings.  */

  if (warn_format)
    check_function_format (NULL, attrs, params);
}

/* Generic argument checking recursion routine.  PARAM is the argument to
   be checked.  PARAM_NUM is the number of the argument.  CALLBACK is invoked
   once the argument is resolved.  CTX is context for the callback.  */
void
check_function_arguments_recurse (callback, ctx, param, param_num)
     void (*callback) PARAMS ((void *, tree, unsigned HOST_WIDE_INT));
     void *ctx;
     tree param;
     unsigned HOST_WIDE_INT param_num;
{
  if (TREE_CODE (param) == NOP_EXPR)
    {
      /* Strip coercion.  */
      check_function_arguments_recurse (callback, ctx,
				        TREE_OPERAND (param, 0), param_num);
      return;
    }

  if (TREE_CODE (param) == CALL_EXPR)
    {
      tree type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (param, 0)));
      tree attrs;
      bool found_format_arg = false;

      /* See if this is a call to a known internationalization function
	 that modifies a format arg.  Such a function may have multiple
	 format_arg attributes (for example, ngettext).  */

      for (attrs = TYPE_ATTRIBUTES (type);
	   attrs;
	   attrs = TREE_CHAIN (attrs))
	if (is_attribute_p ("format_arg", TREE_PURPOSE (attrs)))
	  {
	    tree inner_args;
	    tree format_num_expr;
	    int format_num;
	    int i;

	    /* Extract the argument number, which was previously checked
	       to be valid.  */
	    format_num_expr = TREE_VALUE (TREE_VALUE (attrs));
	    while (TREE_CODE (format_num_expr) == NOP_EXPR
		   || TREE_CODE (format_num_expr) == CONVERT_EXPR
		   || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	      format_num_expr = TREE_OPERAND (format_num_expr, 0);

	    if (TREE_CODE (format_num_expr) != INTEGER_CST
		|| TREE_INT_CST_HIGH (format_num_expr) != 0)
	      abort ();

	    format_num = TREE_INT_CST_LOW (format_num_expr);

	    for (inner_args = TREE_OPERAND (param, 1), i = 1;
		 inner_args != 0;
		 inner_args = TREE_CHAIN (inner_args), i++)
	      if (i == format_num)
		{
		  check_function_arguments_recurse (callback, ctx,
						    TREE_VALUE (inner_args),
						    param_num);
		  found_format_arg = true;
		  break;
		}
	  }

      /* If we found a format_arg attribute and did a recursive check,
	 we are done with checking this argument.  Otherwise, we continue
	 and this will be considered a non-literal.  */
      if (found_format_arg)
	return;
    }

  if (TREE_CODE (param) == COND_EXPR)
    {
      /* Check both halves of the conditional expression.  */
      check_function_arguments_recurse (callback, ctx,
				        TREE_OPERAND (param, 1), param_num);
      check_function_arguments_recurse (callback, ctx,
				        TREE_OPERAND (param, 2), param_num);
      return;
    }

  (*callback) (ctx, param, param_num);
}

#include "gt-c-common.h"
