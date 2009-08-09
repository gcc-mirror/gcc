/* Subroutines shared by all languages that are variants of C.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "intl.h"
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "c-pragma.h"
#include "rtl.h"
#include "ggc.h"
#include "varray.h"
#include "expr.h"
#include "c-common.h"
#include "tm_p.h"
#include "obstack.h"
#include "cpplib.h"
#include "target.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "c-tree.h"
#include "toplev.h"
#include "diagnostic.h"
#include "tree-iterator.h"
#include "hashtab.h"
#include "tree-mudflap.h"
#include "opts.h"
#include "real.h"
#include "cgraph.h"
#include "target-def.h"
#include "gimple.h"
#include "fixed-value.h"
#include "libfuncs.h"

cpp_reader *parse_in;		/* Declared in c-pragma.h.  */

/* The following symbols are subsumed in the c_global_trees array, and
   listed here individually for documentation purposes.

   INTEGER_TYPE and REAL_TYPE nodes for the standard data types.

	tree short_integer_type_node;
	tree long_integer_type_node;
	tree long_long_integer_type_node;

	tree short_unsigned_type_node;
	tree long_unsigned_type_node;
	tree long_long_unsigned_type_node;

	tree truthvalue_type_node;
	tree truthvalue_false_node;
	tree truthvalue_true_node;

	tree ptrdiff_type_node;

	tree unsigned_char_type_node;
	tree signed_char_type_node;
	tree wchar_type_node;

	tree char16_type_node;
	tree char32_type_node;

	tree float_type_node;
	tree double_type_node;
	tree long_double_type_node;

	tree complex_integer_type_node;
	tree complex_float_type_node;
	tree complex_double_type_node;
	tree complex_long_double_type_node;

	tree dfloat32_type_node;
	tree dfloat64_type_node;
	tree_dfloat128_type_node;

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

   Type `char16_t[SOMENUMBER]' or something like it.
   Used when a UTF-16 string literal is created.

	tree char16_array_type_node;

   Type `char32_t[SOMENUMBER]' or something like it.
   Used when a UTF-32 string literal is created.

	tree char32_array_type_node;

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

/* Nonzero if preprocessing only.  */

int flag_preprocess_only;

/* Nonzero means don't output line number information.  */

char flag_no_line_commands;

/* Nonzero causes -E output not to be done, but directives such as
   #define that have side effects are still obeyed.  */

char flag_no_output;

/* Nonzero means dump macros in some fashion.  */

char flag_dump_macros;

/* Nonzero means pass #include lines through to the output.  */

char flag_dump_includes;

/* Nonzero means process PCH files while preprocessing.  */

bool flag_pch_preprocess;

/* The file name to which we should write a precompiled header, or
   NULL if no header will be written in this compile.  */

const char *pch_file;

/* Nonzero if an ISO standard was selected.  It rejects macros in the
   user's namespace.  */
int flag_iso;

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

/* Nonzero means allow implicit conversions between vectors with
   differing numbers of subparts and/or differing element types.  */
int flag_lax_vector_conversions;

/* Nonzero means allow Microsoft extensions without warnings or errors.  */
int flag_ms_extensions;

/* Nonzero means don't recognize the keyword `asm'.  */

int flag_no_asm;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */

int flag_signed_bitfields = 1;

/* Warn about #pragma directives that are not recognized.  */

int warn_unknown_pragmas; /* Tri state variable.  */

/* Warn about format/argument anomalies in calls to formatted I/O functions
   (*printf, *scanf, strftime, strfmon, etc.).  */

int warn_format;

/* Warn about using __null (as NULL in C++) as sentinel.  For code compiled
   with GCC this doesn't matter as __null is guaranteed to have the right
   size.  */

int warn_strict_null_sentinel;

/* Zero means that faster, ...NonNil variants of objc_msgSend...
   calls will be used in ObjC; passing nil receivers to such calls
   will most likely result in crashes.  */
int flag_nil_receivers = 1;

/* Nonzero means that code generation will be altered to support
   "zero-link" execution.  This currently affects ObjC only, but may
   affect other languages in the future.  */
int flag_zero_link = 0;

/* Nonzero means emit an '__OBJC, __image_info' for the current translation
   unit.  It will inform the ObjC runtime that class definition(s) herein
   contained are to replace one(s) previously loaded.  */
int flag_replace_objc_classes = 0;

/* C/ObjC language option variables.  */


/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.  */

int flag_cond_mismatch;

/* Nonzero means enable C89 Amendment 1 features.  */

int flag_isoc94;

/* Nonzero means use the ISO C99 dialect of C.  */

int flag_isoc99;

/* Nonzero means that we have builtin functions, and main is an int.  */

int flag_hosted = 1;


/* ObjC language option variables.  */


/* Open and close the file for outputting class declarations, if
   requested (ObjC).  */

int flag_gen_declaration;

/* Tells the compiler that this is a special run.  Do not perform any
   compiling, instead we are to test some platform dependent features
   and output a C header file with appropriate definitions.  */

int print_struct_values;

/* Tells the compiler what is the constant string class for ObjC.  */

const char *constant_string_class_name;


/* C++ language option variables.  */


/* Nonzero means don't recognize any extension keywords.  */

int flag_no_gnu_keywords;

/* Nonzero means do emit exported implementations of functions even if
   they can be inlined.  */

int flag_implement_inlines = 1;

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

/* The C++ dialect being used. C++98 is the default.  */

enum cxx_dialect cxx_dialect = cxx98;

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

/* 0 means we want the preprocessor to not emit line directives for
   the current working directory.  1 means we want it to do it.  -1
   means we should decide depending on whether debugging information
   is being emitted or not.  */

int flag_working_directory = -1;

/* Nonzero to use __cxa_atexit, rather than atexit, to register
   destructors for local statics and global objects.  '2' means it has been
   set nonzero as a default, not by a command-line flag.  */

int flag_use_cxa_atexit = DEFAULT_USE_CXA_ATEXIT;

/* Nonzero to use __cxa_get_exception_ptr in C++ exception-handling
   code.  '2' means it has not been set explicitly on the command line.  */

int flag_use_cxa_get_exception_ptr = 2;

/* Nonzero means to implement standard semantics for exception
   specifications, calling unexpected if an exception is thrown that
   doesn't match the specification.  Zero means to treat them as
   assertions and optimize accordingly, but not check them.  */

int flag_enforce_eh_specs = 1;

/* Nonzero means to generate thread-safe code for initializing local
   statics.  */

int flag_threadsafe_statics = 1;

/* Nonzero if we want to pretty-print template specializations as the
   template signature followed by the arguments.  */

int flag_pretty_templates = 1;

/* Nonzero means warn about implicit declarations.  */

int warn_implicit = 1;

/* Maximum template instantiation depth.  This limit exists to limit the
   time it takes to notice infinite template instantiations; the default
   value of 1024 is likely to be in the next C++ standard.  */

int max_tinst_depth = 1024;



/* The elements of `ridpointers' are identifier nodes for the reserved
   type names and storage classes.  It is indexed by a RID_... value.  */
tree *ridpointers;

tree (*make_fname_decl) (location_t, tree, int);

/* Nonzero means don't warn about problems that occur when the code is
   executed.  */
int c_inhibit_evaluation_warnings;

/* Whether lexing has been completed, so subsequent preprocessor
   errors should use the compiler's input_location.  */
bool done_lexing = false;

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

static tree c_fully_fold_internal (tree expr, bool, bool *, bool *);
static tree check_case_value (tree);
static bool check_case_bounds (tree, tree, tree *, tree *);

static tree handle_packed_attribute (tree *, tree, tree, int, bool *);
static tree handle_nocommon_attribute (tree *, tree, tree, int, bool *);
static tree handle_common_attribute (tree *, tree, tree, int, bool *);
static tree handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree handle_hot_attribute (tree *, tree, tree, int, bool *);
static tree handle_cold_attribute (tree *, tree, tree, int, bool *);
static tree handle_noinline_attribute (tree *, tree, tree, int, bool *);
static tree handle_noclone_attribute (tree *, tree, tree, int, bool *);
static tree handle_always_inline_attribute (tree *, tree, tree, int,
					    bool *);
static tree handle_gnu_inline_attribute (tree *, tree, tree, int, bool *);
static tree handle_artificial_attribute (tree *, tree, tree, int, bool *);
static tree handle_flatten_attribute (tree *, tree, tree, int, bool *);
static tree handle_error_attribute (tree *, tree, tree, int, bool *);
static tree handle_used_attribute (tree *, tree, tree, int, bool *);
static tree handle_unused_attribute (tree *, tree, tree, int, bool *);
static tree handle_externally_visible_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_transparent_union_attribute (tree *, tree, tree,
						int, bool *);
static tree handle_constructor_attribute (tree *, tree, tree, int, bool *);
static tree handle_destructor_attribute (tree *, tree, tree, int, bool *);
static tree handle_mode_attribute (tree *, tree, tree, int, bool *);
static tree handle_section_attribute (tree *, tree, tree, int, bool *);
static tree handle_aligned_attribute (tree *, tree, tree, int, bool *);
static tree handle_weak_attribute (tree *, tree, tree, int, bool *) ;
static tree handle_alias_attribute (tree *, tree, tree, int, bool *);
static tree handle_weakref_attribute (tree *, tree, tree, int, bool *) ;
static tree handle_visibility_attribute (tree *, tree, tree, int,
					 bool *);
static tree handle_tls_model_attribute (tree *, tree, tree, int,
					bool *);
static tree handle_no_instrument_function_attribute (tree *, tree,
						     tree, int, bool *);
static tree handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_limit_stack_attribute (tree *, tree, tree, int,
					     bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree handle_deprecated_attribute (tree *, tree, tree, int,
					 bool *);
static tree handle_vector_size_attribute (tree *, tree, tree, int,
					  bool *);
static tree handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_cleanup_attribute (tree *, tree, tree, int, bool *);
static tree handle_warn_unused_result_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_sentinel_attribute (tree *, tree, tree, int, bool *);
static tree handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree handle_alloc_size_attribute (tree *, tree, tree, int, bool *);
static tree handle_target_attribute (tree *, tree, tree, int, bool *);
static tree handle_optimize_attribute (tree *, tree, tree, int, bool *);

static void check_function_nonnull (tree, int, tree *);
static void check_nonnull_arg (void *, tree, unsigned HOST_WIDE_INT);
static bool nonnull_check_p (tree, unsigned HOST_WIDE_INT);
static bool get_nonnull_operand (tree, unsigned HOST_WIDE_INT *);
static int resort_field_decl_cmp (const void *, const void *);

/* Reserved words.  The third field is a mask: keywords are disabled
   if they match the mask.

   Masks for languages:
   C --std=c89: D_C99 | D_CXXONLY | D_OBJC | D_CXX_OBJC
   C --std=c99: D_CXXONLY | D_OBJC
   ObjC is like C except that D_OBJC and D_CXX_OBJC are not set
   C++ --std=c98: D_CONLY | D_CXXOX | D_OBJC
   C++ --std=c0x: D_CONLY | D_OBJC
   ObjC++ is like C++ except that D_OBJC is not set

   If -fno-asm is used, D_ASM is added to the mask.  If
   -fno-gnu-keywords is used, D_EXT is added.  If -fno-asm and C in
   C89 mode, D_EXT89 is added for both -fno-asm and -fno-gnu-keywords.
   In C with -Wc++-compat, we warn if D_CXXWARN is set.  */

const struct c_common_resword c_common_reswords[] =
{
  { "_Bool",		RID_BOOL,      D_CONLY },
  { "_Complex",		RID_COMPLEX,	0 },
  { "_Imaginary",	RID_IMAGINARY, D_CONLY },
  { "_Decimal32",       RID_DFLOAT32,  D_CONLY | D_EXT },
  { "_Decimal64",       RID_DFLOAT64,  D_CONLY | D_EXT },
  { "_Decimal128",      RID_DFLOAT128, D_CONLY | D_EXT },
  { "_Fract",           RID_FRACT,     D_CONLY | D_EXT },
  { "_Accum",           RID_ACCUM,     D_CONLY | D_EXT },
  { "_Sat",             RID_SAT,       D_CONLY | D_EXT },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof",	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__builtin_choose_expr", RID_CHOOSE_EXPR, D_CONLY },
  { "__builtin_offsetof", RID_OFFSETOF, 0 },
  { "__builtin_types_compatible_p", RID_TYPES_COMPATIBLE_P, D_CONLY },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__decltype",       RID_DECLTYPE,   D_CXXONLY },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME, 0 },
  { "__has_nothrow_assign", RID_HAS_NOTHROW_ASSIGN, D_CXXONLY },
  { "__has_nothrow_constructor", RID_HAS_NOTHROW_CONSTRUCTOR, D_CXXONLY },
  { "__has_nothrow_copy", RID_HAS_NOTHROW_COPY, D_CXXONLY },
  { "__has_trivial_assign", RID_HAS_TRIVIAL_ASSIGN, D_CXXONLY },
  { "__has_trivial_constructor", RID_HAS_TRIVIAL_CONSTRUCTOR, D_CXXONLY },
  { "__has_trivial_copy", RID_HAS_TRIVIAL_COPY, D_CXXONLY },
  { "__has_trivial_destructor", RID_HAS_TRIVIAL_DESTRUCTOR, D_CXXONLY },
  { "__has_virtual_destructor", RID_HAS_VIRTUAL_DESTRUCTOR, D_CXXONLY },
  { "__is_abstract",	RID_IS_ABSTRACT, D_CXXONLY },
  { "__is_base_of",	RID_IS_BASE_OF, D_CXXONLY },
  { "__is_class",	RID_IS_CLASS,	D_CXXONLY },
  { "__is_convertible_to", RID_IS_CONVERTIBLE_TO, D_CXXONLY },
  { "__is_empty",	RID_IS_EMPTY,	D_CXXONLY },
  { "__is_enum",	RID_IS_ENUM,	D_CXXONLY },
  { "__is_pod",		RID_IS_POD,	D_CXXONLY },
  { "__is_polymorphic",	RID_IS_POLYMORPHIC, D_CXXONLY },
  { "__is_standard_layout", RID_IS_STD_LAYOUT, D_CXXONLY },
  { "__is_trivial",     RID_IS_TRIVIAL, D_CXXONLY },
  { "__is_union",	RID_IS_UNION,	D_CXXONLY },
  { "__imag",		RID_IMAGPART,	0 },
  { "__imag__",		RID_IMAGPART,	0 },
  { "__inline",		RID_INLINE,	0 },
  { "__inline__",	RID_INLINE,	0 },
  { "__label__",	RID_LABEL,	0 },
  { "__null",		RID_NULL,	0 },
  { "__real",		RID_REALPART,	0 },
  { "__real__",		RID_REALPART,	0 },
  { "__restrict",	RID_RESTRICT,	0 },
  { "__restrict__",	RID_RESTRICT,	0 },
  { "__signed",		RID_SIGNED,	0 },
  { "__signed__",	RID_SIGNED,	0 },
  { "__thread",		RID_THREAD,	0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	D_ASM },
  { "auto",		RID_AUTO,	0 },
  { "bool",		RID_BOOL,	D_CXXONLY | D_CXXWARN },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "catch",		RID_CATCH,	D_CXX_OBJC | D_CXXWARN },
  { "char",		RID_CHAR,	0 },
  { "char16_t",		RID_CHAR16,	D_CXXONLY | D_CXX0X | D_CXXWARN },
  { "char32_t",		RID_CHAR32,	D_CXXONLY | D_CXX0X | D_CXXWARN },
  { "class",		RID_CLASS,	D_CXX_OBJC | D_CXXWARN },
  { "const",		RID_CONST,	0 },
  { "const_cast",	RID_CONSTCAST,	D_CXXONLY | D_CXXWARN },
  { "continue",		RID_CONTINUE,	0 },
  { "decltype",         RID_DECLTYPE,   D_CXXONLY | D_CXX0X | D_CXXWARN },
  { "default",		RID_DEFAULT,	0 },
  { "delete",		RID_DELETE,	D_CXXONLY | D_CXXWARN },
  { "do",		RID_DO,		0 },
  { "double",		RID_DOUBLE,	0 },
  { "dynamic_cast",	RID_DYNCAST,	D_CXXONLY | D_CXXWARN },
  { "else",		RID_ELSE,	0 },
  { "enum",		RID_ENUM,	0 },
  { "explicit",		RID_EXPLICIT,	D_CXXONLY | D_CXXWARN },
  { "export",		RID_EXPORT,	D_CXXONLY | D_CXXWARN },
  { "extern",		RID_EXTERN,	0 },
  { "false",		RID_FALSE,	D_CXXONLY | D_CXXWARN },
  { "float",		RID_FLOAT,	0 },
  { "for",		RID_FOR,	0 },
  { "friend",		RID_FRIEND,	D_CXXONLY | D_CXXWARN },
  { "goto",		RID_GOTO,	0 },
  { "if",		RID_IF,		0 },
  { "inline",		RID_INLINE,	D_EXT89 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "mutable",		RID_MUTABLE,	D_CXXONLY | D_CXXWARN },
  { "namespace",	RID_NAMESPACE,	D_CXXONLY | D_CXXWARN },
  { "new",		RID_NEW,	D_CXXONLY | D_CXXWARN },
  { "operator",		RID_OPERATOR,	D_CXXONLY | D_CXXWARN },
  { "private",		RID_PRIVATE,	D_CXX_OBJC | D_CXXWARN },
  { "protected",	RID_PROTECTED,	D_CXX_OBJC | D_CXXWARN },
  { "public",		RID_PUBLIC,	D_CXX_OBJC | D_CXXWARN },
  { "register",		RID_REGISTER,	0 },
  { "reinterpret_cast",	RID_REINTCAST,	D_CXXONLY | D_CXXWARN },
  { "restrict",		RID_RESTRICT,	D_CONLY | D_C99 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	0 },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "static_assert",    RID_STATIC_ASSERT, D_CXXONLY | D_CXX0X | D_CXXWARN },
  { "static_cast",	RID_STATCAST,	D_CXXONLY | D_CXXWARN },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "template",		RID_TEMPLATE,	D_CXXONLY | D_CXXWARN },
  { "this",		RID_THIS,	D_CXXONLY | D_CXXWARN },
  { "throw",		RID_THROW,	D_CXX_OBJC | D_CXXWARN },
  { "true",		RID_TRUE,	D_CXXONLY | D_CXXWARN },
  { "try",		RID_TRY,	D_CXX_OBJC | D_CXXWARN },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typename",		RID_TYPENAME,	D_CXXONLY | D_CXXWARN },
  { "typeid",		RID_TYPEID,	D_CXXONLY | D_CXXWARN },
  { "typeof",		RID_TYPEOF,	D_ASM | D_EXT },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "using",		RID_USING,	D_CXXONLY | D_CXXWARN },
  { "virtual",		RID_VIRTUAL,	D_CXXONLY | D_CXXWARN },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	0 },
  { "wchar_t",		RID_WCHAR,	D_CXXONLY },
  { "while",		RID_WHILE,	0 },
  /* These Objective-C keywords are recognized only immediately after
     an '@'.  */
  { "compatibility_alias", RID_AT_ALIAS,	D_OBJC },
  { "defs",		RID_AT_DEFS,		D_OBJC },
  { "encode",		RID_AT_ENCODE,		D_OBJC },
  { "end",		RID_AT_END,		D_OBJC },
  { "implementation",	RID_AT_IMPLEMENTATION,	D_OBJC },
  { "interface",	RID_AT_INTERFACE,	D_OBJC },
  { "protocol",		RID_AT_PROTOCOL,	D_OBJC },
  { "selector",		RID_AT_SELECTOR,	D_OBJC },
  { "finally",		RID_AT_FINALLY,		D_OBJC },
  { "synchronized",	RID_AT_SYNCHRONIZED,	D_OBJC },
  /* These are recognized only in protocol-qualifier context
     (see above) */
  { "bycopy",		RID_BYCOPY,		D_OBJC },
  { "byref",		RID_BYREF,		D_OBJC },
  { "in",		RID_IN,			D_OBJC },
  { "inout",		RID_INOUT,		D_OBJC },
  { "oneway",		RID_ONEWAY,		D_OBJC },
  { "out",		RID_OUT,		D_OBJC },
};

const unsigned int num_c_common_reswords =
  sizeof c_common_reswords / sizeof (struct c_common_resword);

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
  { "noclone",                0, 0, true,  false, false,
			      handle_noclone_attribute },
  { "always_inline",          0, 0, true,  false, false,
			      handle_always_inline_attribute },
  { "gnu_inline",             0, 0, true,  false, false,
			      handle_gnu_inline_attribute },
  { "artificial",             0, 0, true,  false, false,
			      handle_artificial_attribute },
  { "flatten",                0, 0, true,  false, false,
			      handle_flatten_attribute },
  { "used",                   0, 0, true,  false, false,
			      handle_used_attribute },
  { "unused",                 0, 0, false, false, false,
			      handle_unused_attribute },
  { "externally_visible",     0, 0, true,  false, false,
			      handle_externally_visible_attribute },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",                  0, 0, true,  false, false,
			      handle_const_attribute },
  { "transparent_union",      0, 0, false, false, false,
			      handle_transparent_union_attribute },
  { "constructor",            0, 1, true,  false, false,
			      handle_constructor_attribute },
  { "destructor",             0, 1, true,  false, false,
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
  { "weakref",                0, 1, true,  false, false,
			      handle_weakref_attribute },
  { "no_instrument_function", 0, 0, true,  false, false,
			      handle_no_instrument_function_attribute },
  { "malloc",                 0, 0, true,  false, false,
			      handle_malloc_attribute },
  { "returns_twice",          0, 0, true,  false, false,
			      handle_returns_twice_attribute },
  { "no_stack_limit",         0, 0, true,  false, false,
			      handle_no_limit_stack_attribute },
  { "pure",                   0, 0, true,  false, false,
			      handle_pure_attribute },
  /* For internal use (marking of builtins) only.  The name contains space
     to prevent its usage in source code.  */
  { "no vops",                0, 0, true,  false, false,
			      handle_novops_attribute },
  { "deprecated",             0, 1, false, false, false,
			      handle_deprecated_attribute },
  { "vector_size",	      1, 1, false, true, false,
			      handle_vector_size_attribute },
  { "visibility",	      1, 1, false, false, false,
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
  { "warn_unused_result",     0, 0, false, true, true,
			      handle_warn_unused_result_attribute },
  { "sentinel",               0, 1, false, true, true,
			      handle_sentinel_attribute },
  /* For internal use (marking of builtins) only.  The name contains space
     to prevent its usage in source code.  */
  { "type generic",           0, 0, false, true, true,
			      handle_type_generic_attribute },
  { "alloc_size",	      1, 2, false, true, true,
			      handle_alloc_size_attribute },
  { "cold",                   0, 0, true,  false, false,
			      handle_cold_attribute },
  { "hot",                    0, 0, true,  false, false,
			      handle_hot_attribute },
  { "warning",		      1, 1, true,  false, false,
			      handle_error_attribute },
  { "error",		      1, 1, true,  false, false,
			      handle_error_attribute },
  { "target",                 1, -1, true, false, false,
			      handle_target_attribute },
  { "optimize",               1, -1, true, false, false,
			      handle_optimize_attribute },
  { NULL,                     0, 0, false, false, false, NULL }
};

/* Give the specifications for the format attributes, used by C and all
   descendants.  */

const struct attribute_spec c_common_format_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "format",                 3, 3, false, true,  true,
			      handle_format_attribute },
  { "format_arg",             1, 1, false, true,  true,
			      handle_format_arg_attribute },
  { NULL,                     0, 0, false, false, false, NULL }
};

/* Push current bindings for the function name VAR_DECLS.  */

void
start_fname_decls (void)
{
  unsigned ix;
  tree saved = NULL_TREE;

  for (ix = 0; fname_vars[ix].decl; ix++)
    {
      tree decl = *fname_vars[ix].decl;

      if (decl)
	{
	  saved = tree_cons (decl, build_int_cst (NULL_TREE, ix), saved);
	  *fname_vars[ix].decl = NULL_TREE;
	}
    }
  if (saved || saved_function_name_decls)
    /* Normally they'll have been NULL, so only push if we've got a
       stack, or they are non-NULL.  */
    saved_function_name_decls = tree_cons (saved, NULL_TREE,
					   saved_function_name_decls);
}

/* Finish up the current bindings, adding them into the current function's
   statement tree.  This must be done _before_ finish_stmt_tree is called.
   If there is no current function, we must be at file scope and no statements
   are involved. Pop the previous bindings.  */

void
finish_fname_decls (void)
{
  unsigned ix;
  tree stmts = NULL_TREE;
  tree stack = saved_function_name_decls;

  for (; stack && TREE_VALUE (stack); stack = TREE_CHAIN (stack))
    append_to_statement_list (TREE_VALUE (stack), &stmts);

  if (stmts)
    {
      tree *bodyp = &DECL_SAVED_TREE (current_function_decl);

      if (TREE_CODE (*bodyp) == BIND_EXPR)
	bodyp = &BIND_EXPR_BODY (*bodyp);

      append_to_statement_list_force (*bodyp, &stmts);
      *bodyp = stmts;
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

/* Return the text name of the current function, suitably prettified
   by PRETTY_P.  Return string must be freed by caller.  */

const char *
fname_as_string (int pretty_p)
{
  const char *name = "top level";
  char *namep;
  int vrb = 2, len;
  cpp_string cstr = { 0, 0 }, strname;

  if (!pretty_p)
    {
      name = "";
      vrb = 0;
    }

  if (current_function_decl)
    name = lang_hooks.decl_printable_name (current_function_decl, vrb);

  len = strlen (name) + 3; /* Two for '"'s.  One for NULL.  */

  namep = XNEWVEC (char, len);
  snprintf (namep, len, "\"%s\"", name);
  strname.text = (unsigned char *) namep;
  strname.len = len - 1;

  if (cpp_interpret_string (parse_in, &strname, 1, &cstr, CPP_STRING))
    {
      XDELETEVEC (namep);
      return (const char *) cstr.text;
    }

  return namep;
}

/* Return the VAR_DECL for a const char array naming the current
   function. If the VAR_DECL has not yet been created, create it
   now. RID indicates how it should be formatted and IDENTIFIER_NODE
   ID is its name (unfortunately C and C++ hold the RID values of
   keywords in different places, so we can't derive RID from ID in
   this language independent code. LOC is the location of the
   function.  */

tree
fname_decl (location_t loc, unsigned int rid, tree id)
{
  unsigned ix;
  tree decl = NULL_TREE;

  for (ix = 0; fname_vars[ix].decl; ix++)
    if (fname_vars[ix].rid == rid)
      break;

  decl = *fname_vars[ix].decl;
  if (!decl)
    {
      /* If a tree is built here, it would normally have the lineno of
	 the current statement.  Later this tree will be moved to the
	 beginning of the function and this line number will be wrong.
	 To avoid this problem set the lineno to 0 here; that prevents
	 it from appearing in the RTL.  */
      tree stmts;
      location_t saved_location = input_location;
      input_location = UNKNOWN_LOCATION;

      stmts = push_stmt_list ();
      decl = (*make_fname_decl) (loc, id, fname_vars[ix].pretty);
      stmts = pop_stmt_list (stmts);
      if (!IS_EMPTY_STMT (stmts))
	saved_function_name_decls
	  = tree_cons (decl, stmts, saved_function_name_decls);
      *fname_vars[ix].decl = decl;
      input_location = saved_location;
    }
  if (!ix && !current_function_decl)
    pedwarn (loc, 0, "%qD is not defined outside of function scope", decl);

  return decl;
}

/* Given a STRING_CST, give it a suitable array-of-chars data type.  */

tree
fix_string_type (tree value)
{
  int length = TREE_STRING_LENGTH (value);
  int nchars;
  tree e_type, i_type, a_type;

  /* Compute the number of elements, for the array type.  */
  if (TREE_TYPE (value) == char_array_type_node || !TREE_TYPE (value))
    {
      nchars = length;
      e_type = char_type_node;
    }
  else if (TREE_TYPE (value) == char16_array_type_node)
    {
      nchars = length / (TYPE_PRECISION (char16_type_node) / BITS_PER_UNIT);
      e_type = char16_type_node;
    }
  else if (TREE_TYPE (value) == char32_array_type_node)
    {
      nchars = length / (TYPE_PRECISION (char32_type_node) / BITS_PER_UNIT);
      e_type = char32_type_node;
    }
  else
    {
      nchars = length / (TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT);
      e_type = wchar_type_node;
    }

  /* C89 2.2.4.1, C99 5.2.4.1 (Translation limits).  The analogous
     limit in C++98 Annex B is very large (65536) and is not normative,
     so we do not diagnose it (warn_overlength_strings is forced off
     in c_common_post_options).  */
  if (warn_overlength_strings)
    {
      const int nchars_max = flag_isoc99 ? 4095 : 509;
      const int relevant_std = flag_isoc99 ? 99 : 90;
      if (nchars - 1 > nchars_max)
	/* Translators: The %d after 'ISO C' will be 90 or 99.  Do not
	   separate the %d from the 'C'.  'ISO' should not be
	   translated, but it may be moved after 'C%d' in languages
	   where modifiers follow nouns.  */
	pedwarn (input_location, OPT_Woverlength_strings,
		 "string length %qd is greater than the length %qd "
		 "ISO C%d compilers are required to support",
		 nchars - 1, nchars_max, relevant_std);
    }

  /* Create the array type for the string constant.  The ISO C++
     standard says that a string literal has type `const char[N]' or
     `const wchar_t[N]'.  We use the same logic when invoked as a C
     front-end with -Wwrite-strings.
     ??? We should change the type of an expression depending on the
     state of a warning flag.  We should just be warning -- see how
     this is handled in the C++ front-end for the deprecated implicit
     conversion from string literals to `char*' or `wchar_t*'.

     The C++ front end relies on TYPE_MAIN_VARIANT of a cv-qualified
     array type being the unqualified version of that type.
     Therefore, if we are constructing an array of const char, we must
     construct the matching unqualified array type first.  The C front
     end does not require this, but it does no harm, so we do it
     unconditionally.  */
  i_type = build_index_type (build_int_cst (NULL_TREE, nchars - 1));
  a_type = build_array_type (e_type, i_type);
  if (c_dialect_cxx() || warn_write_strings)
    a_type = c_build_qualified_type (a_type, TYPE_QUAL_CONST);

  TREE_TYPE (value) = a_type;
  TREE_CONSTANT (value) = 1;
  TREE_READONLY (value) = 1;
  TREE_STATIC (value) = 1;
  return value;
}

/* Fully fold EXPR, an expression that was not folded (beyond integer
   constant expressions and null pointer constants) when being built
   up.  If IN_INIT, this is in a static initializer and certain
   changes are made to the folding done.  Clear *MAYBE_CONST if
   MAYBE_CONST is not NULL and EXPR is definitely not a constant
   expression because it contains an evaluated operator (in C99) or an
   operator outside of sizeof returning an integer constant (in C90)
   not permitted in constant expressions, or because it contains an
   evaluated arithmetic overflow.  (*MAYBE_CONST should typically be
   set to true by callers before calling this function.)  Return the
   folded expression.  Function arguments have already been folded
   before calling this function, as have the contents of SAVE_EXPR,
   TARGET_EXPR, BIND_EXPR, VA_ARG_EXPR, OBJ_TYPE_REF and
   C_MAYBE_CONST_EXPR.  */

tree
c_fully_fold (tree expr, bool in_init, bool *maybe_const)
{
  tree ret;
  tree eptype = NULL_TREE;
  bool dummy = true;
  bool maybe_const_itself = true;
  location_t loc = EXPR_LOCATION (expr);

  /* This function is not relevant to C++ because C++ folds while
     parsing, and may need changes to be correct for C++ when C++
     stops folding while parsing.  */
  if (c_dialect_cxx ())
    gcc_unreachable ();

  if (!maybe_const)
    maybe_const = &dummy;
  if (TREE_CODE (expr) == EXCESS_PRECISION_EXPR)
    {
      eptype = TREE_TYPE (expr);
      expr = TREE_OPERAND (expr, 0);
    }
  ret = c_fully_fold_internal (expr, in_init, maybe_const,
			       &maybe_const_itself);
  if (eptype)
    ret = fold_convert_loc (loc, eptype, ret);
  *maybe_const &= maybe_const_itself;
  return ret;
}

/* Internal helper for c_fully_fold.  EXPR and IN_INIT are as for
   c_fully_fold.  *MAYBE_CONST_OPERANDS is cleared because of operands
   not permitted, while *MAYBE_CONST_ITSELF is cleared because of
   arithmetic overflow (for C90, *MAYBE_CONST_OPERANDS is carried from
   both evaluated and unevaluated subexpressions while
   *MAYBE_CONST_ITSELF is carried from only evaluated
   subexpressions).  */

static tree
c_fully_fold_internal (tree expr, bool in_init, bool *maybe_const_operands,
		       bool *maybe_const_itself)
{
  tree ret = expr;
  enum tree_code code = TREE_CODE (expr);
  enum tree_code_class kind = TREE_CODE_CLASS (code);
  location_t loc = EXPR_LOCATION (expr);
  tree op0, op1, op2, op3;
  tree orig_op0, orig_op1, orig_op2;
  bool op0_const = true, op1_const = true, op2_const = true;
  bool op0_const_self = true, op1_const_self = true, op2_const_self = true;
  bool nowarning = TREE_NO_WARNING (expr);
  int unused_p;

  /* This function is not relevant to C++ because C++ folds while
     parsing, and may need changes to be correct for C++ when C++
     stops folding while parsing.  */
  if (c_dialect_cxx ())
    gcc_unreachable ();

  /* Constants, declarations, statements, errors, SAVE_EXPRs and
     anything else not counted as an expression cannot usefully be
     folded further at this point.  */
  if (!IS_EXPR_CODE_CLASS (kind)
      || kind == tcc_statement
      || code == SAVE_EXPR)
    return expr;

  /* Operands of variable-length expressions (function calls) have
     already been folded, as have __builtin_* function calls, and such
     expressions cannot occur in constant expressions.  */
  if (kind == tcc_vl_exp)
    {
      *maybe_const_operands = false;
      ret = fold (expr);
      goto out;
    }

  if (code == C_MAYBE_CONST_EXPR)
    {
      tree pre = C_MAYBE_CONST_EXPR_PRE (expr);
      tree inner = C_MAYBE_CONST_EXPR_EXPR (expr);
      if (C_MAYBE_CONST_EXPR_NON_CONST (expr))
	*maybe_const_operands = false;
      if (C_MAYBE_CONST_EXPR_INT_OPERANDS (expr))
	*maybe_const_itself = false;
      if (pre && !in_init)
	ret = build2 (COMPOUND_EXPR, TREE_TYPE (expr), pre, inner);
      else
	ret = inner;
      goto out;
    }

  /* Assignment, increment, decrement, function call and comma
     operators, and statement expressions, cannot occur in constant
     expressions if evaluated / outside of sizeof.  (Function calls
     were handled above, though VA_ARG_EXPR is treated like a function
     call here, and statement expressions are handled through
     C_MAYBE_CONST_EXPR to avoid folding inside them.)  */
  switch (code)
    {
    case MODIFY_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case COMPOUND_EXPR:
      *maybe_const_operands = false;
      break;

    case VA_ARG_EXPR:
    case TARGET_EXPR:
    case BIND_EXPR:
    case OBJ_TYPE_REF:
      *maybe_const_operands = false;
      ret = fold (expr);
      goto out;

    default:
      break;
    }

  /* Fold individual tree codes as appropriate.  */
  switch (code)
    {
    case COMPOUND_LITERAL_EXPR:
      /* Any non-constancy will have been marked in a containing
	 C_MAYBE_CONST_EXPR; there is no more folding to do here.  */
      goto out;

    case COMPONENT_REF:
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);
      op2 = TREE_OPERAND (expr, 2);
      op0 = c_fully_fold_internal (op0, in_init, maybe_const_operands,
				   maybe_const_itself);
      if (op0 != orig_op0)
	ret = build3 (COMPONENT_REF, TREE_TYPE (expr), op0, op1, op2);
      if (ret != expr)
	{
	  TREE_READONLY (ret) = TREE_READONLY (expr);
	  TREE_THIS_VOLATILE (ret) = TREE_THIS_VOLATILE (expr);
	}
      goto out;

    case ARRAY_REF:
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      orig_op1 = op1 = TREE_OPERAND (expr, 1);
      op2 = TREE_OPERAND (expr, 2);
      op3 = TREE_OPERAND (expr, 3);
      op0 = c_fully_fold_internal (op0, in_init, maybe_const_operands,
				   maybe_const_itself);
      op1 = c_fully_fold_internal (op1, in_init, maybe_const_operands,
				   maybe_const_itself);
      op1 = decl_constant_value_for_optimization (op1);
      if (op0 != orig_op0 || op1 != orig_op1)
	ret = build4 (ARRAY_REF, TREE_TYPE (expr), op0, op1, op2, op3);
      if (ret != expr)
	{
	  TREE_READONLY (ret) = TREE_READONLY (expr);
	  TREE_SIDE_EFFECTS (ret) = TREE_SIDE_EFFECTS (expr);
	  TREE_THIS_VOLATILE (ret) = TREE_THIS_VOLATILE (expr);
	}
      ret = fold (ret);
      goto out;

    case COMPOUND_EXPR:
    case MODIFY_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case POINTER_PLUS_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case COMPLEX_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      /* Binary operations evaluating both arguments (increment and
	 decrement are binary internally in GCC).  */
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      orig_op1 = op1 = TREE_OPERAND (expr, 1);
      op0 = c_fully_fold_internal (op0, in_init, maybe_const_operands,
				   maybe_const_itself);
      if (code != MODIFY_EXPR
	  && code != PREDECREMENT_EXPR
	  && code != PREINCREMENT_EXPR
	  && code != POSTDECREMENT_EXPR
	  && code != POSTINCREMENT_EXPR)
	op0 = decl_constant_value_for_optimization (op0);
      /* The RHS of a MODIFY_EXPR was fully folded when building that
	 expression for the sake of conversion warnings.  */
      if (code != MODIFY_EXPR)
	op1 = c_fully_fold_internal (op1, in_init, maybe_const_operands,
				     maybe_const_itself);
      op1 = decl_constant_value_for_optimization (op1);
      if (op0 != orig_op0 || op1 != orig_op1 || in_init)
	ret = in_init
	  ? fold_build2_initializer_loc (loc, code, TREE_TYPE (expr), op0, op1)
	  : fold_build2_loc (loc, code, TREE_TYPE (expr), op0, op1);
      else
	ret = fold (expr);
      if (TREE_OVERFLOW_P (ret)
	  && !TREE_OVERFLOW_P (op0)
	  && !TREE_OVERFLOW_P (op1))
	overflow_warning (EXPR_LOCATION (expr), ret);
      goto out;

    case INDIRECT_REF:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case CONJ_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* Unary operations.  */
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      op0 = c_fully_fold_internal (op0, in_init, maybe_const_operands,
				   maybe_const_itself);
      if (code != ADDR_EXPR && code != REALPART_EXPR && code != IMAGPART_EXPR)
	op0 = decl_constant_value_for_optimization (op0);
      if (op0 != orig_op0 || in_init)
	ret = in_init
	  ? fold_build1_initializer_loc (loc, code, TREE_TYPE (expr), op0)
	  : fold_build1_loc (loc, code, TREE_TYPE (expr), op0);
      else
	ret = fold (expr);
      if (code == INDIRECT_REF
	  && ret != expr
	  && TREE_CODE (ret) == INDIRECT_REF)
	{
	  TREE_READONLY (ret) = TREE_READONLY (expr);
	  TREE_SIDE_EFFECTS (ret) = TREE_SIDE_EFFECTS (expr);
	  TREE_THIS_VOLATILE (ret) = TREE_THIS_VOLATILE (expr);
	}
      switch (code)
	{
	case FIX_TRUNC_EXPR:
	case FLOAT_EXPR:
	CASE_CONVERT:
	  /* Don't warn about explicit conversions.  We will already
	     have warned about suspect implicit conversions.  */
	  break;

	default:
	  if (TREE_OVERFLOW_P (ret) && !TREE_OVERFLOW_P (op0))
	    overflow_warning (EXPR_LOCATION (expr), ret);
	  break;
	}
      goto out;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      /* Binary operations not necessarily evaluating both
	 arguments.  */
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      orig_op1 = op1 = TREE_OPERAND (expr, 1);
      op0 = c_fully_fold_internal (op0, in_init, &op0_const, &op0_const_self);

      unused_p = (op0 == (code == TRUTH_ANDIF_EXPR
			  ? truthvalue_false_node
			  : truthvalue_true_node));
      c_inhibit_evaluation_warnings += unused_p;
      op1 = c_fully_fold_internal (op1, in_init, &op1_const, &op1_const_self);
      c_inhibit_evaluation_warnings -= unused_p;

      if (op0 != orig_op0 || op1 != orig_op1 || in_init)
	ret = in_init
	  ? fold_build2_initializer_loc (loc, code, TREE_TYPE (expr), op0, op1)
	  : fold_build2_loc (loc, code, TREE_TYPE (expr), op0, op1);
      else
	ret = fold (expr);
      *maybe_const_operands &= op0_const;
      *maybe_const_itself &= op0_const_self;
      if (!(flag_isoc99
	    && op0_const
	    && op0_const_self
	    && (code == TRUTH_ANDIF_EXPR
		? op0 == truthvalue_false_node
		: op0 == truthvalue_true_node)))
	*maybe_const_operands &= op1_const;
      if (!(op0_const
	    && op0_const_self
	    && (code == TRUTH_ANDIF_EXPR
		? op0 == truthvalue_false_node
		: op0 == truthvalue_true_node)))
	*maybe_const_itself &= op1_const_self;
      goto out;

    case COND_EXPR:
      orig_op0 = op0 = TREE_OPERAND (expr, 0);
      orig_op1 = op1 = TREE_OPERAND (expr, 1);
      orig_op2 = op2 = TREE_OPERAND (expr, 2);
      op0 = c_fully_fold_internal (op0, in_init, &op0_const, &op0_const_self);

      c_inhibit_evaluation_warnings += (op0 == truthvalue_false_node);
      op1 = c_fully_fold_internal (op1, in_init, &op1_const, &op1_const_self);
      c_inhibit_evaluation_warnings -= (op0 == truthvalue_false_node);

      c_inhibit_evaluation_warnings += (op0 == truthvalue_true_node);
      op2 = c_fully_fold_internal (op2, in_init, &op2_const, &op2_const_self);
      c_inhibit_evaluation_warnings -= (op0 == truthvalue_true_node);

      if (op0 != orig_op0 || op1 != orig_op1 || op2 != orig_op2)
	ret = fold_build3_loc (loc, code, TREE_TYPE (expr), op0, op1, op2);
      else
	ret = fold (expr);
      *maybe_const_operands &= op0_const;
      *maybe_const_itself &= op0_const_self;
      if (!(flag_isoc99
	    && op0_const
	    && op0_const_self
	    && op0 == truthvalue_false_node))
	*maybe_const_operands &= op1_const;
      if (!(op0_const
	    && op0_const_self
	    && op0 == truthvalue_false_node))
	*maybe_const_itself &= op1_const_self;
      if (!(flag_isoc99
	    && op0_const
	    && op0_const_self
	    && op0 == truthvalue_true_node))
	*maybe_const_operands &= op2_const;
      if (!(op0_const
	    && op0_const_self
	    && op0 == truthvalue_true_node))
	*maybe_const_itself &= op2_const_self;
      goto out;

    case EXCESS_PRECISION_EXPR:
      /* Each case where an operand with excess precision may be
	 encountered must remove the EXCESS_PRECISION_EXPR around
	 inner operands and possibly put one around the whole
	 expression or possibly convert to the semantic type (which
	 c_fully_fold does); we cannot tell at this stage which is
	 appropriate in any particular case.  */
      gcc_unreachable ();

    default:
      /* Various codes may appear through folding built-in functions
	 and their arguments.  */
      goto out;
    }

 out:
  /* Some folding may introduce NON_LVALUE_EXPRs; all lvalue checks
     have been done by this point, so remove them again.  */
  nowarning |= TREE_NO_WARNING (ret);
  STRIP_TYPE_NOPS (ret);
  if (nowarning && !TREE_NO_WARNING (ret))
    {
      if (!CAN_HAVE_LOCATION_P (ret))
	ret = build1 (NOP_EXPR, TREE_TYPE (ret), ret);
      TREE_NO_WARNING (ret) = 1;
    }
  if (ret != expr)
    protected_set_expr_location (ret, loc);
  return ret;
}

/* If not optimizing, EXP is not a VAR_DECL, or EXP has array type,
   return EXP.  Otherwise, return either EXP or its known constant
   value (if it has one), but return EXP if EXP has mode BLKmode.  ???
   Is the BLKmode test appropriate?  */

tree
decl_constant_value_for_optimization (tree exp)
{
  tree ret;

  /* This function is only used by C, for c_fully_fold and other
     optimization, and may not be correct for C++.  */
  if (c_dialect_cxx ())
    gcc_unreachable ();

  if (!optimize
      || TREE_CODE (exp) != VAR_DECL
      || TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE
      || DECL_MODE (exp) == BLKmode)
    return exp;

  ret = decl_constant_value (exp);
  /* Avoid unwanted tree sharing between the initializer and current
     function's body where the tree can be modified e.g. by the
     gimplifier.  */
  if (ret != exp && TREE_STATIC (exp))
    ret = unshare_expr (ret);
  return ret;
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (tree value)
{
  if (warn_overflow && pedantic 
      && (TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
	  || TREE_CODE (value) == FIXED_CST
	  || TREE_CODE (value) == VECTOR_CST
	  || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    pedwarn (input_location, OPT_Woverflow, "overflow in constant expression");
}

/* The same as above but print an unconditional error.  */
void
constant_expression_error (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == FIXED_CST
       || TREE_CODE (value) == VECTOR_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    error ("overflow in constant expression");
}

/* Print a warning if an expression had overflow in folding and its
   operands hadn't.

   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) is a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts
   or when the expression is the result of an operator and any operand
   already overflowed.  */

void
overflow_warning (location_t loc, tree value)
{
  if (c_inhibit_evaluation_warnings != 0)
    return;

  switch (TREE_CODE (value))
    {
    case INTEGER_CST:
      warning_at (loc, OPT_Woverflow, "integer overflow in expression");
      break;
      
    case REAL_CST:
      warning_at (loc, OPT_Woverflow,
		  "floating point overflow in expression");
      break;
      
    case FIXED_CST:
      warning_at (loc, OPT_Woverflow, "fixed-point overflow in expression");
      break;

    case VECTOR_CST:
      warning_at (loc, OPT_Woverflow, "vector overflow in expression");
      break;
      
    case COMPLEX_CST:
      if (TREE_CODE (TREE_REALPART (value)) == INTEGER_CST)
	warning_at (loc, OPT_Woverflow,
		    "complex integer overflow in expression");
      else if (TREE_CODE (TREE_REALPART (value)) == REAL_CST)
	warning_at (loc, OPT_Woverflow,
		    "complex floating point overflow in expression");
      break;

    default:
      break;
    }
}

/* Warn about uses of logical || / && operator in a context where it
   is likely that the bitwise equivalent was intended by the
   programmer.  We have seen an expression in which CODE is a binary
   operator used to combine expressions OP_LEFT and OP_RIGHT, which before folding
   had CODE_LEFT and CODE_RIGHT, into an expression of type TYPE.  */
void
warn_logical_operator (location_t location, enum tree_code code, tree type,
		       enum tree_code code_left, tree op_left, 
		       enum tree_code ARG_UNUSED (code_right), tree op_right)
{
  int or_op = (code == TRUTH_ORIF_EXPR || code == TRUTH_OR_EXPR);
  int in0_p, in1_p, in_p;
  tree low0, low1, low, high0, high1, high, lhs, rhs, tem;
  bool strict_overflow_p = false;

  if (code != TRUTH_ANDIF_EXPR
      && code != TRUTH_AND_EXPR
      && code != TRUTH_ORIF_EXPR
      && code != TRUTH_OR_EXPR)
    return;

  /* Warn if &&/|| are being used in a context where it is
     likely that the bitwise equivalent was intended by the
     programmer. That is, an expression such as op && MASK
     where op should not be any boolean expression, nor a
     constant, and mask seems to be a non-boolean integer constant.  */
  if (!truth_value_p (code_left)
      && INTEGRAL_TYPE_P (TREE_TYPE (op_left))
      && !CONSTANT_CLASS_P (op_left)
      && !TREE_NO_WARNING (op_left)
      && TREE_CODE (op_right) == INTEGER_CST
      && !integer_zerop (op_right)
      && !integer_onep (op_right))
    {
      if (or_op)
	warning_at (location, OPT_Wlogical_op, "logical %<or%>"
		    " applied to non-boolean constant");
      else
	warning_at (location, OPT_Wlogical_op, "logical %<and%>"
		    " applied to non-boolean constant");
      TREE_NO_WARNING (op_left) = true;
      return;
    }

  /* We do not warn for constants because they are typical of macro
     expansions that test for features.  */
  if (CONSTANT_CLASS_P (op_left) || CONSTANT_CLASS_P (op_right))
    return;

  /* This warning only makes sense with logical operands.  */
  if (!(truth_value_p (TREE_CODE (op_left))
	|| INTEGRAL_TYPE_P (TREE_TYPE (op_left)))
      || !(truth_value_p (TREE_CODE (op_right))
	   || INTEGRAL_TYPE_P (TREE_TYPE (op_right))))
    return;

  lhs = make_range (op_left, &in0_p, &low0, &high0, &strict_overflow_p);
  rhs = make_range (op_right, &in1_p, &low1, &high1, &strict_overflow_p);

  if (lhs && TREE_CODE (lhs) == C_MAYBE_CONST_EXPR)
    lhs = C_MAYBE_CONST_EXPR_EXPR (lhs);

  if (rhs && TREE_CODE (rhs) == C_MAYBE_CONST_EXPR)
    rhs = C_MAYBE_CONST_EXPR_EXPR (rhs);
  
  /* If this is an OR operation, invert both sides; we will invert
     again at the end.  */
  if (or_op)
    in0_p = !in0_p, in1_p = !in1_p;
  
  /* If both expressions are the same, if we can merge the ranges, and we
     can build the range test, return it or it inverted.  */
  if (lhs && rhs && operand_equal_p (lhs, rhs, 0)
      && merge_ranges (&in_p, &low, &high, in0_p, low0, high0,
		       in1_p, low1, high1)
      && 0 != (tem = build_range_check (UNKNOWN_LOCATION,
					type, lhs, in_p, low, high)))
    {
      if (TREE_CODE (tem) != INTEGER_CST)
	return;

      if (or_op)
        warning_at (location, OPT_Wlogical_op,
                    "logical %<or%> "
                    "of collectively exhaustive tests is always true");
      else
        warning_at (location, OPT_Wlogical_op,
                    "logical %<and%> "
                    "of mutually exclusive tests is always false");
    }
}


/* Print a warning about casts that might indicate violation
   of strict aliasing rules if -Wstrict-aliasing is used and
   strict aliasing mode is in effect. OTYPE is the original
   TREE_TYPE of EXPR, and TYPE the type we're casting to. */

bool
strict_aliasing_warning (tree otype, tree type, tree expr)
{
  /* Strip pointer conversion chains and get to the correct original type.  */
  STRIP_NOPS (expr);
  otype = TREE_TYPE (expr);

  if (!(flag_strict_aliasing
	&& POINTER_TYPE_P (type)
	&& POINTER_TYPE_P (otype)
	&& !VOID_TYPE_P (TREE_TYPE (type)))
      /* If the type we are casting to is a ref-all pointer
         dereferencing it is always valid.  */
      || TYPE_REF_CAN_ALIAS_ALL (type))
    return false;

  if ((warn_strict_aliasing > 1) && TREE_CODE (expr) == ADDR_EXPR
      && (DECL_P (TREE_OPERAND (expr, 0))
          || handled_component_p (TREE_OPERAND (expr, 0))))
    {
      /* Casting the address of an object to non void pointer. Warn
         if the cast breaks type based aliasing.  */
      if (!COMPLETE_TYPE_P (TREE_TYPE (type)) && warn_strict_aliasing == 2)
	{
	  warning (OPT_Wstrict_aliasing, "type-punning to incomplete type "
		   "might break strict-aliasing rules");
	  return true;
	}
      else
        {
          /* warn_strict_aliasing >= 3.   This includes the default (3).  
             Only warn if the cast is dereferenced immediately.  */
          alias_set_type set1 =
	    get_alias_set (TREE_TYPE (TREE_OPERAND (expr, 0)));
          alias_set_type set2 = get_alias_set (TREE_TYPE (type));

          if (set1 != set2 && set2 != 0
	      && (set1 == 0 || !alias_sets_conflict_p (set1, set2)))
	    {
	      warning (OPT_Wstrict_aliasing, "dereferencing type-punned "
		       "pointer will break strict-aliasing rules");
	      return true;
	    }
          else if (warn_strict_aliasing == 2
		   && !alias_sets_must_conflict_p (set1, set2))
	    {
	      warning (OPT_Wstrict_aliasing, "dereferencing type-punned "
		       "pointer might break strict-aliasing rules");
	      return true;
	    }
        }
    }
  else
    if ((warn_strict_aliasing == 1) && !VOID_TYPE_P (TREE_TYPE (otype)))
      {
        /* At this level, warn for any conversions, even if an address is
           not taken in the same statement.  This will likely produce many
           false positives, but could be useful to pinpoint problems that
           are not revealed at higher levels.  */
        alias_set_type set1 = get_alias_set (TREE_TYPE (otype));
        alias_set_type set2 = get_alias_set (TREE_TYPE (type));
        if (!COMPLETE_TYPE_P (type)
            || !alias_sets_must_conflict_p (set1, set2))
	  {
            warning (OPT_Wstrict_aliasing, "dereferencing type-punned "
                     "pointer might break strict-aliasing rules");
            return true;
          }
      }

  return false;
}

/* Warn for unlikely, improbable, or stupid DECL declarations
   of `main'.  */

void
check_main_parameter_types (tree decl)
{
  tree args;
  int argct = 0;

  for (args = TYPE_ARG_TYPES (TREE_TYPE (decl)); args;
      args = TREE_CHAIN (args))
   {
     tree type = args ? TREE_VALUE (args) : 0;

     if (type == void_type_node || type == error_mark_node )
       break;

     ++argct;
     switch (argct)
       {
       case 1:
         if (TYPE_MAIN_VARIANT (type) != integer_type_node)
           pedwarn (input_location, OPT_Wmain, "first argument of %q+D should be %<int%>", 
		    decl);
         break;

       case 2:
         if (TREE_CODE (type) != POINTER_TYPE
             || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
             || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
                 != char_type_node))
           pedwarn (input_location, OPT_Wmain, "second argument of %q+D should be %<char **%>",
		    decl);
         break;

       case 3:
         if (TREE_CODE (type) != POINTER_TYPE
             || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
             || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
                 != char_type_node))
	   pedwarn (input_location, OPT_Wmain, "third argument of %q+D should probably be "
		    "%<char **%>", decl);
         break;
       }
   }

  /* It is intentional that this message does not mention the third
    argument because it's only mentioned in an appendix of the
    standard.  */
  if (argct > 0 && (argct < 2 || argct > 3))
    pedwarn (input_location, OPT_Wmain, "%q+D takes only zero or two arguments", decl);
}

/* True if pointers to distinct types T1 and T2 can be converted to
   each other without an explicit cast.  Only returns true for opaque
   vector types.  */
bool
vector_targets_convertible_p (const_tree t1, const_tree t2)
{
  if (TREE_CODE (t1) == VECTOR_TYPE && TREE_CODE (t2) == VECTOR_TYPE
      && (TYPE_VECTOR_OPAQUE (t1) || TYPE_VECTOR_OPAQUE (t2))
      && tree_int_cst_equal (TYPE_SIZE (t1), TYPE_SIZE (t2)))
    return true;

  return false;
}

/* True if vector types T1 and T2 can be converted to each other
   without an explicit cast.  If EMIT_LAX_NOTE is true, and T1 and T2
   can only be converted with -flax-vector-conversions yet that is not
   in effect, emit a note telling the user about that option if such
   a note has not previously been emitted.  */
bool
vector_types_convertible_p (const_tree t1, const_tree t2, bool emit_lax_note)
{
  static bool emitted_lax_note = false;
  bool convertible_lax;

  if ((TYPE_VECTOR_OPAQUE (t1) || TYPE_VECTOR_OPAQUE (t2))
      && tree_int_cst_equal (TYPE_SIZE (t1), TYPE_SIZE (t2)))
    return true;

  convertible_lax =
    (tree_int_cst_equal (TYPE_SIZE (t1), TYPE_SIZE (t2))
     && (TREE_CODE (TREE_TYPE (t1)) != REAL_TYPE ||
	 TYPE_PRECISION (t1) == TYPE_PRECISION (t2))
     && (INTEGRAL_TYPE_P (TREE_TYPE (t1))
	 == INTEGRAL_TYPE_P (TREE_TYPE (t2))));

  if (!convertible_lax || flag_lax_vector_conversions)
    return convertible_lax;

  if (TYPE_VECTOR_SUBPARTS (t1) == TYPE_VECTOR_SUBPARTS (t2)
      && lang_hooks.types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return true;

  if (emit_lax_note && !emitted_lax_note)
    {
      emitted_lax_note = true;
      inform (input_location, "use -flax-vector-conversions to permit "
              "conversions between vectors with differing "
              "element types or numbers of subparts");
    }

  return false;
}

/* This is a helper function of build_binary_op.

   For certain operations if both args were extended from the same
   smaller type, do the arithmetic in that type and then extend.

   BITWISE indicates a bitwise operation.
   For them, this optimization is safe only if
   both args are zero-extended or both are sign-extended.
   Otherwise, we might change the result.
   Eg, (short)-1 | (unsigned short)-1 is (int)-1
   but calculated in (unsigned short) it would be (unsigned short)-1.  
*/
tree shorten_binary_op (tree result_type, tree op0, tree op1, bool bitwise)
{
  int unsigned0, unsigned1;
  tree arg0, arg1;
  int uns;
  tree type;

  /* Cast OP0 and OP1 to RESULT_TYPE.  Doing so prevents
     excessive narrowing when we call get_narrower below.  For
     example, suppose that OP0 is of unsigned int extended
     from signed char and that RESULT_TYPE is long long int.
     If we explicitly cast OP0 to RESULT_TYPE, OP0 would look
     like
     
     (long long int) (unsigned int) signed_char

     which get_narrower would narrow down to
     
     (unsigned int) signed char
     
     If we do not cast OP0 first, get_narrower would return
     signed_char, which is inconsistent with the case of the
     explicit cast.  */
  op0 = convert (result_type, op0);
  op1 = convert (result_type, op1);

  arg0 = get_narrower (op0, &unsigned0);
  arg1 = get_narrower (op1, &unsigned1);

  /* UNS is 1 if the operation to be done is an unsigned one.  */
  uns = TYPE_UNSIGNED (result_type);

  /* Handle the case that OP0 (or OP1) does not *contain* a conversion
     but it *requires* conversion to FINAL_TYPE.  */
  
  if ((TYPE_PRECISION (TREE_TYPE (op0))
       == TYPE_PRECISION (TREE_TYPE (arg0)))
      && TREE_TYPE (op0) != result_type)
    unsigned0 = TYPE_UNSIGNED (TREE_TYPE (op0));
  if ((TYPE_PRECISION (TREE_TYPE (op1))
       == TYPE_PRECISION (TREE_TYPE (arg1)))
      && TREE_TYPE (op1) != result_type)
    unsigned1 = TYPE_UNSIGNED (TREE_TYPE (op1));
  
  /* Now UNSIGNED0 is 1 if ARG0 zero-extends to FINAL_TYPE.  */
  
  /* For bitwise operations, signedness of nominal type
     does not matter.  Consider only how operands were extended.  */
  if (bitwise)
    uns = unsigned0;
  
  /* Note that in all three cases below we refrain from optimizing
     an unsigned operation on sign-extended args.
     That would not be valid.  */
  
  /* Both args variable: if both extended in same way
     from same width, do it in that width.
     Do it unsigned if args were zero-extended.  */
  if ((TYPE_PRECISION (TREE_TYPE (arg0))
       < TYPE_PRECISION (result_type))
      && (TYPE_PRECISION (TREE_TYPE (arg1))
	  == TYPE_PRECISION (TREE_TYPE (arg0)))
      && unsigned0 == unsigned1
      && (unsigned0 || !uns))
    return c_common_signed_or_unsigned_type
      (unsigned0, common_type (TREE_TYPE (arg0), TREE_TYPE (arg1)));

  else if (TREE_CODE (arg0) == INTEGER_CST
	   && (unsigned1 || !uns)
	   && (TYPE_PRECISION (TREE_TYPE (arg1))
	       < TYPE_PRECISION (result_type))
	   && (type
	       = c_common_signed_or_unsigned_type (unsigned1,
						   TREE_TYPE (arg1)))
	   && !POINTER_TYPE_P (type)
	   && int_fits_type_p (arg0, type))
    return type;

  else if (TREE_CODE (arg1) == INTEGER_CST
	   && (unsigned0 || !uns)
	   && (TYPE_PRECISION (TREE_TYPE (arg0))
	       < TYPE_PRECISION (result_type))
	   && (type
	       = c_common_signed_or_unsigned_type (unsigned0,
						   TREE_TYPE (arg0)))
	   && !POINTER_TYPE_P (type)
	   && int_fits_type_p (arg1, type))
    return type;

  return result_type;
}

/* Warns if the conversion of EXPR to TYPE may alter a value.
   This is a helper function for warnings_for_convert_and_check.  */

static void
conversion_warning (tree type, tree expr)
{
  bool give_warning = false;

  int i;
  const int expr_num_operands = TREE_OPERAND_LENGTH (expr);
  tree expr_type = TREE_TYPE (expr);

  if (!warn_conversion && !warn_sign_conversion)
    return;

  /* If any operand is artificial, then this expression was generated
     by the compiler and we do not warn.  */
  for (i = 0; i < expr_num_operands; i++)
    {
      tree op = TREE_OPERAND (expr, i);
      if (op && DECL_P (op) && DECL_ARTIFICIAL (op))
	return;
    }

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      /* Conversion from boolean to a signed:1 bit-field (which only
	 can hold the values 0 and -1) doesn't lose information - but
	 it does change the value.  */
      if (TYPE_PRECISION (type) == 1 && !TYPE_UNSIGNED (type)) 
	warning (OPT_Wconversion,
                 "conversion to %qT from boolean expression", type);
      return;

    case REAL_CST:
    case INTEGER_CST:

      /* Warn for real constant that is not an exact integer converted
         to integer type.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
          && TREE_CODE (type) == INTEGER_TYPE)
        {
          if (!real_isinteger (TREE_REAL_CST_PTR (expr), TYPE_MODE (expr_type)))
            give_warning = true;
        }
      /* Warn for an integer constant that does not fit into integer type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
               && TREE_CODE (type) == INTEGER_TYPE
               && !int_fits_type_p (expr, type))
        {
          if (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type) 
	      && tree_int_cst_sgn (expr) < 0)
	    warning (OPT_Wsign_conversion,
		     "negative integer implicitly converted to unsigned type");
          else if (!TYPE_UNSIGNED (type) && TYPE_UNSIGNED (expr_type))
	    warning (OPT_Wsign_conversion,  "conversion of unsigned constant "
		     "value to negative integer");
	  else
	    give_warning = true;
        }
      else if (TREE_CODE (type) == REAL_TYPE)
        {
          /* Warn for an integer constant that does not fit into real type.  */
          if (TREE_CODE (expr_type) == INTEGER_TYPE)
            {
              REAL_VALUE_TYPE a = real_value_from_int_cst (0, expr);
              if (!exact_real_truncate (TYPE_MODE (type), &a))
                give_warning = true;
            }
          /* Warn for a real constant that does not fit into a smaller
             real type.  */
          else if (TREE_CODE (expr_type) == REAL_TYPE
                   && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
            {
              REAL_VALUE_TYPE a = TREE_REAL_CST (expr);
              if (!exact_real_truncate (TYPE_MODE (type), &a))
                give_warning = true;
            }
        }

      if (give_warning)
        warning (OPT_Wconversion,
                 "conversion to %qT alters %qT constant value",
                 type, expr_type);

      return;

    case COND_EXPR:
      {
	/* In case of COND_EXPR, if both operands are constants or
	   COND_EXPR, then we do not care about the type of COND_EXPR,
	   only about the conversion of each operand.  */
	tree op1 = TREE_OPERAND (expr, 1);
	tree op2 = TREE_OPERAND (expr, 2);

	if ((TREE_CODE (op1) == REAL_CST || TREE_CODE (op1) == INTEGER_CST 
	     || TREE_CODE (op1) == COND_EXPR)
	    && (TREE_CODE (op2) == REAL_CST || TREE_CODE (op2) == INTEGER_CST
		|| TREE_CODE (op2) == COND_EXPR))
	  {
	    conversion_warning (type, op1);
	    conversion_warning (type, op2);
	    return;
	  }
	/* Fall through.  */
      }

    default: /* 'expr' is not a constant.  */

      /* Warn for real types converted to integer types.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
          && TREE_CODE (type) == INTEGER_TYPE)
        give_warning = true;

      else if (TREE_CODE (expr_type) == INTEGER_TYPE
               && TREE_CODE (type) == INTEGER_TYPE)
        {
	  /* Don't warn about unsigned char y = 0xff, x = (int) y;  */
	  expr = get_unwidened (expr, 0);
	  expr_type = TREE_TYPE (expr);

	  /* Don't warn for short y; short x = ((int)y & 0xff);  */
	  if (TREE_CODE (expr) == BIT_AND_EXPR 
		|| TREE_CODE (expr) == BIT_IOR_EXPR 
	      || TREE_CODE (expr) == BIT_XOR_EXPR)
	    {
	      /* If both args were extended from a shortest type,
		 use that type if that is safe.  */
	      expr_type = shorten_binary_op (expr_type, 
					     TREE_OPERAND (expr, 0), 
					     TREE_OPERAND (expr, 1), 
					     /* bitwise */1);

	      if (TREE_CODE (expr) == BIT_AND_EXPR)
		{
		  tree op0 = TREE_OPERAND (expr, 0);
		  tree op1 = TREE_OPERAND (expr, 1);
		  bool unsigned0 = TYPE_UNSIGNED (TREE_TYPE (op0));
		  bool unsigned1 = TYPE_UNSIGNED (TREE_TYPE (op1));

		  /* If one of the operands is a non-negative constant
		     that fits in the target type, then the type of the
		     other operand does not matter. */
		  if ((TREE_CODE (op0) == INTEGER_CST
		       && int_fits_type_p (op0, c_common_signed_type (type))
		       && int_fits_type_p (op0, c_common_unsigned_type (type)))
		      || (TREE_CODE (op1) == INTEGER_CST
			  && int_fits_type_p (op1, c_common_signed_type (type))
			  && int_fits_type_p (op1, 
					      c_common_unsigned_type (type))))
		    return;
		  /* If constant is unsigned and fits in the target
		     type, then the result will also fit.  */
		  else if ((TREE_CODE (op0) == INTEGER_CST
			    && unsigned0 
			    && int_fits_type_p (op0, type))
			   || (TREE_CODE (op1) == INTEGER_CST
			       && unsigned1
			       && int_fits_type_p (op1, type)))
		    return;
		}
	    }
          /* Warn for integer types converted to smaller integer types.  */
	  if (TYPE_PRECISION (type) < TYPE_PRECISION (expr_type)) 
	    give_warning = true;

	  /* When they are the same width but different signedness,
	     then the value may change.  */
	  else if ((TYPE_PRECISION (type) == TYPE_PRECISION (expr_type)
		    && TYPE_UNSIGNED (expr_type) != TYPE_UNSIGNED (type))
		   /* Even when converted to a bigger type, if the type is
		      unsigned but expr is signed, then negative values
		      will be changed.  */
		   || (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type)))
	    warning (OPT_Wsign_conversion, "conversion to %qT from %qT "
		     "may change the sign of the result",
		     type, expr_type);
        }

      /* Warn for integer types converted to real types if and only if
         all the range of values of the integer type cannot be
         represented by the real type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
               && TREE_CODE (type) == REAL_TYPE)
        {
          tree type_low_bound = TYPE_MIN_VALUE (expr_type);
          tree type_high_bound = TYPE_MAX_VALUE (expr_type);
          REAL_VALUE_TYPE real_low_bound 
	    = real_value_from_int_cst (0, type_low_bound);
          REAL_VALUE_TYPE real_high_bound 
	    = real_value_from_int_cst (0, type_high_bound);

          if (!exact_real_truncate (TYPE_MODE (type), &real_low_bound)
              || !exact_real_truncate (TYPE_MODE (type), &real_high_bound))
            give_warning = true;
        }

      /* Warn for real types converted to smaller real types.  */
      else if (TREE_CODE (expr_type) == REAL_TYPE
               && TREE_CODE (type) == REAL_TYPE
               && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
        give_warning = true;


      if (give_warning)
        warning (OPT_Wconversion,
                 "conversion to %qT from %qT may alter its value",
                 type, expr_type);
    }
}

/* Produce warnings after a conversion. RESULT is the result of
   converting EXPR to TYPE.  This is a helper function for
   convert_and_check and cp_convert_and_check.  */

void
warnings_for_convert_and_check (tree type, tree expr, tree result)
{
  if (TREE_CODE (expr) == INTEGER_CST
      && (TREE_CODE (type) == INTEGER_TYPE
          || TREE_CODE (type) == ENUMERAL_TYPE)
      && !int_fits_type_p (expr, type))
    {
      /* Do not diagnose overflow in a constant expression merely
         because a conversion overflowed.  */
      if (TREE_OVERFLOW (result))
        TREE_OVERFLOW (result) = TREE_OVERFLOW (expr);

      if (TYPE_UNSIGNED (type))
        {
          /* This detects cases like converting -129 or 256 to
             unsigned char.  */
          if (!int_fits_type_p (expr, c_common_signed_type (type)))
            warning (OPT_Woverflow,
                     "large integer implicitly truncated to unsigned type");
          else
            conversion_warning (type, expr);
        }
      else if (!int_fits_type_p (expr, c_common_unsigned_type (type))) 
	warning (OPT_Woverflow,
		 "overflow in implicit constant conversion");
      /* No warning for converting 0x80000000 to int.  */
      else if (pedantic
	       && (TREE_CODE (TREE_TYPE (expr)) != INTEGER_TYPE
		   || TYPE_PRECISION (TREE_TYPE (expr))
		   != TYPE_PRECISION (type)))
	warning (OPT_Woverflow,
		 "overflow in implicit constant conversion");

      else
	conversion_warning (type, expr);
    }
  else if ((TREE_CODE (result) == INTEGER_CST
	    || TREE_CODE (result) == FIXED_CST) && TREE_OVERFLOW (result))
    warning (OPT_Woverflow,
             "overflow in implicit constant conversion");
  else
    conversion_warning (type, expr);
}


/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (tree type, tree expr)
{
  tree result;
  tree expr_for_warning;

  /* Convert from a value with possible excess precision rather than
     via the semantic type, but do not warn about values not fitting
     exactly in the semantic type.  */
  if (TREE_CODE (expr) == EXCESS_PRECISION_EXPR)
    {
      tree orig_type = TREE_TYPE (expr);
      expr = TREE_OPERAND (expr, 0);
      expr_for_warning = convert (orig_type, expr);
      if (orig_type == type)
	return expr_for_warning;
    }
  else
    expr_for_warning = expr;

  if (TREE_TYPE (expr) == type)
    return expr;
  
  result = convert (type, expr);

  if (c_inhibit_evaluation_warnings == 0
      && !TREE_OVERFLOW_P (expr)
      && result != error_mark_node)
    warnings_for_convert_and_check (type, expr_for_warning, result);

  return result;
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

static void add_tlist (struct tlist **, struct tlist *, tree, int);
static void merge_tlist (struct tlist **, struct tlist *, int);
static void verify_tree (tree, struct tlist **, struct tlist **, tree);
static int warning_candidate_p (tree);
static bool candidate_equal_p (const_tree, const_tree);
static void warn_for_collisions (struct tlist *);
static void warn_for_collisions_1 (tree, tree, struct tlist *, int);
static struct tlist *new_tlist (struct tlist *, tree, tree);

/* Create a new struct tlist and fill in its fields.  */
static struct tlist *
new_tlist (struct tlist *next, tree t, tree writer)
{
  struct tlist *l;
  l = XOBNEW (&tlist_obstack, struct tlist);
  l->next = next;
  l->expr = t;
  l->writer = writer;
  return l;
}

/* Add duplicates of the nodes found in ADD to the list *TO.  If EXCLUDE_WRITER
   is nonnull, we ignore any node we find which has a writer equal to it.  */

static void
add_tlist (struct tlist **to, struct tlist *add, tree exclude_writer, int copy)
{
  while (add)
    {
      struct tlist *next = add->next;
      if (!copy)
	add->next = *to;
      if (!exclude_writer || !candidate_equal_p (add->writer, exclude_writer))
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
merge_tlist (struct tlist **to, struct tlist *add, int copy)
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
	if (candidate_equal_p (tmp2->expr, add->expr))
	  {
	    found = 1;
	    if (!tmp2->writer)
	      tmp2->writer = add->writer;
	  }
      if (!found)
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
warn_for_collisions_1 (tree written, tree writer, struct tlist *list,
		       int only_writes)
{
  struct tlist *tmp;

  /* Avoid duplicate warnings.  */
  for (tmp = warned_ids; tmp; tmp = tmp->next)
    if (candidate_equal_p (tmp->expr, written))
      return;

  while (list)
    {
      if (candidate_equal_p (list->expr, written)
	  && !candidate_equal_p (list->writer, writer)
	  && (!only_writes || list->writer))
	{
	  warned_ids = new_tlist (warned_ids, written, NULL_TREE);
	  warning_at (EXPR_HAS_LOCATION (writer)
		      ? EXPR_LOCATION (writer) : input_location,
		      OPT_Wsequence_point, "operation on %qE may be undefined",
		      list->expr);
	}
      list = list->next;
    }
}

/* Given a list LIST of references to variables, find whether any of these
   can cause conflicts due to missing sequence points.  */

static void
warn_for_collisions (struct tlist *list)
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
warning_candidate_p (tree x)
{
  /* !VOID_TYPE_P (TREE_TYPE (x)) is workaround for cp/tree.c
     (lvalue_p) crash on TRY/CATCH. */
  return !(DECL_P (x) && DECL_ARTIFICIAL (x))
    && TREE_TYPE (x) && !VOID_TYPE_P (TREE_TYPE (x)) && lvalue_p (x);
}

/* Return nonzero if X and Y appear to be the same candidate (or NULL) */
static bool
candidate_equal_p (const_tree x, const_tree y)
{
  return (x == y) || (x && y && operand_equal_p (x, y, 0));
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
verify_tree (tree x, struct tlist **pbefore_sp, struct tlist **pno_sp,
	     tree writer)
{
  struct tlist *tmp_before, *tmp_nosp, *tmp_list2, *tmp_list3;
  enum tree_code code;
  enum tree_code_class cl;

  /* X may be NULL if it is the operand of an empty statement expression
     ({ }).  */
  if (x == NULL)
    return;

 restart:
  code = TREE_CODE (x);
  cl = TREE_CODE_CLASS (code);

  if (warning_candidate_p (x))
    *pno_sp = new_tlist (*pno_sp, x, writer);

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
      {
	call_expr_arg_iterator iter;
	tree arg;
	tmp_before = tmp_nosp = 0; 
	verify_tree (CALL_EXPR_FN (x), &tmp_before, &tmp_nosp, NULL_TREE);
	FOR_EACH_CALL_EXPR_ARG (arg, iter, x)
	  {
	    tmp_list2 = tmp_list3 = 0;
	    verify_tree (arg, &tmp_list2, &tmp_list3, NULL_TREE);
	    merge_tlist (&tmp_list3, tmp_list2, 0);
	    add_tlist (&tmp_before, tmp_list3, NULL_TREE, 0);
	  }
	add_tlist (&tmp_before, tmp_nosp, NULL_TREE, 0);
	warn_for_collisions (tmp_before);
	add_tlist (pbefore_sp, tmp_before, NULL_TREE, 0);
	return;
      }

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
	  if (candidate_equal_p (t->expr, x))
	    break;

	if (!t)
	  {
	    t = XOBNEW (&tlist_obstack, struct tlist_cache);
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

    case ADDR_EXPR:
      x = TREE_OPERAND (x, 0);
      if (DECL_P (x))
	return;
      writer = 0;
      goto restart;

    default:
      /* For other expressions, simply recurse on their operands.
	 Manual tail recursion for unary expressions.
	 Other non-expressions need not be processed.  */
      if (cl == tcc_unary)
	{
	  x = TREE_OPERAND (x, 0);
	  writer = 0;
	  goto restart;
	}
      else if (IS_EXPR_CODE_CLASS (cl))
	{
	  int lp;
	  int max = TREE_OPERAND_LENGTH (x);
	  for (lp = 0; lp < max; lp++)
	    {
	      tmp_before = tmp_nosp = 0;
	      verify_tree (TREE_OPERAND (x, lp), &tmp_before, &tmp_nosp, 0);
	      merge_tlist (&tmp_nosp, tmp_before, 0);
	      add_tlist (pno_sp, tmp_nosp, NULL_TREE, 0);
	    }
	}
      return;
    }
}

/* Try to warn for undefined behavior in EXPR due to missing sequence
   points.  */

void
verify_sequence_points (tree expr)
{
  struct tlist *before_sp = 0, *after_sp = 0;

  warned_ids = 0;
  save_expr_cache = 0;
  if (tlist_firstobj == 0)
    {
      gcc_obstack_init (&tlist_obstack);
      tlist_firstobj = (char *) obstack_alloc (&tlist_obstack, 0);
    }

  verify_tree (expr, &before_sp, &after_sp, 0);
  warn_for_collisions (after_sp);
  obstack_free (&tlist_obstack, tlist_firstobj);
}

/* Validate the expression after `case' and apply default promotions.  */

static tree
check_case_value (tree value)
{
  if (value == NULL_TREE)
    return value;

  /* ??? Can we ever get nops here for a valid case value?  We
     shouldn't for C.  */
  STRIP_TYPE_NOPS (value);
  /* In C++, the following is allowed:

       const int i = 3;
       switch (...) { case i: ... }

     So, we try to reduce the VALUE to a constant that way.  */
  if (c_dialect_cxx ())
    {
      value = decl_constant_value (value);
      STRIP_TYPE_NOPS (value);
      value = fold (value);
    }

  if (TREE_CODE (value) == INTEGER_CST)
    /* Promote char or short to int.  */
    value = perform_integral_promotions (value);
  else if (value != error_mark_node)
    {
      error ("case label does not reduce to an integer constant");
      value = error_mark_node;
    }

  constant_expression_warning (value);

  return value;
}

/* See if the case values LOW and HIGH are in the range of the original
   type (i.e. before the default conversion to int) of the switch testing
   expression.
   TYPE is the promoted type of the testing expression, and ORIG_TYPE is
   the type before promoting it.  CASE_LOW_P is a pointer to the lower
   bound of the case label, and CASE_HIGH_P is the upper bound or NULL
   if the case is not a case range.
   The caller has to make sure that we are not called with NULL for
   CASE_LOW_P (i.e. the default case).
   Returns true if the case label is in range of ORIG_TYPE (saturated or
   untouched) or false if the label is out of range.  */

static bool
check_case_bounds (tree type, tree orig_type,
		   tree *case_low_p, tree *case_high_p)
{
  tree min_value, max_value;
  tree case_low = *case_low_p;
  tree case_high = case_high_p ? *case_high_p : case_low;

  /* If there was a problem with the original type, do nothing.  */
  if (orig_type == error_mark_node)
    return true;

  min_value = TYPE_MIN_VALUE (orig_type);
  max_value = TYPE_MAX_VALUE (orig_type);

  /* Case label is less than minimum for type.  */
  if (tree_int_cst_compare (case_low, min_value) < 0
      && tree_int_cst_compare (case_high, min_value) < 0)
    {
      warning (0, "case label value is less than minimum value for type");
      return false;
    }

  /* Case value is greater than maximum for type.  */
  if (tree_int_cst_compare (case_low, max_value) > 0
      && tree_int_cst_compare (case_high, max_value) > 0)
    {
      warning (0, "case label value exceeds maximum value for type");
      return false;
    }

  /* Saturate lower case label value to minimum.  */
  if (tree_int_cst_compare (case_high, min_value) >= 0
      && tree_int_cst_compare (case_low, min_value) < 0)
    {
      warning (0, "lower value in case label range"
	       " less than minimum value for type");
      case_low = min_value;
    }

  /* Saturate upper case label value to maximum.  */
  if (tree_int_cst_compare (case_low, max_value) <= 0
      && tree_int_cst_compare (case_high, max_value) > 0)
    {
      warning (0, "upper value in case label range"
	       " exceeds maximum value for type");
      case_high = max_value;
    }

  if (*case_low_p != case_low)
    *case_low_p = convert (type, case_low);
  if (case_high_p && *case_high_p != case_high)
    *case_high_p = convert (type, case_high);

  return true;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
c_common_type_for_size (unsigned int bits, int unsignedp)
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

/* Return a fixed-point type that has at least IBIT ibits and FBIT fbits
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed;
   and saturating if SATP is nonzero, otherwise not saturating.  */

tree
c_common_fixed_point_type_for_size (unsigned int ibit, unsigned int fbit,
				    int unsignedp, int satp)
{
  enum machine_mode mode;
  if (ibit == 0)
    mode = unsignedp ? UQQmode : QQmode;
  else
    mode = unsignedp ? UHAmode : HAmode;

  for (; mode != VOIDmode; mode = GET_MODE_WIDER_MODE (mode))
    if (GET_MODE_IBIT (mode) >= ibit && GET_MODE_FBIT (mode) >= fbit)
      break;

  if (mode == VOIDmode || !targetm.scalar_mode_supported_p (mode))
    {
      sorry ("GCC cannot support operators with integer types and "
	     "fixed-point types that have too many integral and "
	     "fractional bits together");
      return 0;
    }

  return c_common_type_for_mode (mode, satp);
}

/* Used for communication between c_common_type_for_mode and
   c_register_builtin_type.  */
static GTY(()) tree registered_builtin_types;

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.
   If the mode is a fixed-point mode,
   then UNSIGNEDP selects between saturating and nonsaturating types.  */

tree
c_common_type_for_mode (enum machine_mode mode, int unsignedp)
{
  tree t;

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

  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return (unsignedp
	    ? make_unsigned_type (GET_MODE_PRECISION (mode))
	    : make_signed_type (GET_MODE_PRECISION (mode)));

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return (unsignedp
	    ? make_unsigned_type (GET_MODE_PRECISION (mode))
	    : make_signed_type (GET_MODE_PRECISION (mode)));

  if (COMPLEX_MODE_P (mode))
    {
      enum machine_mode inner_mode;
      tree inner_type;

      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;

      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;

      inner_mode = GET_MODE_INNER (mode);
      inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_complex_type (inner_type);
    }
  else if (VECTOR_MODE_P (mode))
    {
      enum machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_vector_type_for_mode (inner_type, mode);
    }

  if (mode == TYPE_MODE (dfloat32_type_node))
    return dfloat32_type_node;
  if (mode == TYPE_MODE (dfloat64_type_node))
    return dfloat64_type_node;
  if (mode == TYPE_MODE (dfloat128_type_node))
    return dfloat128_type_node;

  if (ALL_SCALAR_FIXED_POINT_MODE_P (mode))
    {
      if (mode == TYPE_MODE (short_fract_type_node))
	return unsignedp ? sat_short_fract_type_node : short_fract_type_node;
      if (mode == TYPE_MODE (fract_type_node))
	return unsignedp ? sat_fract_type_node : fract_type_node;
      if (mode == TYPE_MODE (long_fract_type_node))
	return unsignedp ? sat_long_fract_type_node : long_fract_type_node;
      if (mode == TYPE_MODE (long_long_fract_type_node))
	return unsignedp ? sat_long_long_fract_type_node
			 : long_long_fract_type_node;

      if (mode == TYPE_MODE (unsigned_short_fract_type_node))
	return unsignedp ? sat_unsigned_short_fract_type_node
			 : unsigned_short_fract_type_node;
      if (mode == TYPE_MODE (unsigned_fract_type_node))
	return unsignedp ? sat_unsigned_fract_type_node
			 : unsigned_fract_type_node;
      if (mode == TYPE_MODE (unsigned_long_fract_type_node))
	return unsignedp ? sat_unsigned_long_fract_type_node
			 : unsigned_long_fract_type_node;
      if (mode == TYPE_MODE (unsigned_long_long_fract_type_node))
	return unsignedp ? sat_unsigned_long_long_fract_type_node
			 : unsigned_long_long_fract_type_node;

      if (mode == TYPE_MODE (short_accum_type_node))
	return unsignedp ? sat_short_accum_type_node : short_accum_type_node;
      if (mode == TYPE_MODE (accum_type_node))
	return unsignedp ? sat_accum_type_node : accum_type_node;
      if (mode == TYPE_MODE (long_accum_type_node))
	return unsignedp ? sat_long_accum_type_node : long_accum_type_node;
      if (mode == TYPE_MODE (long_long_accum_type_node))
	return unsignedp ? sat_long_long_accum_type_node
			 : long_long_accum_type_node;

      if (mode == TYPE_MODE (unsigned_short_accum_type_node))
	return unsignedp ? sat_unsigned_short_accum_type_node
			 : unsigned_short_accum_type_node;
      if (mode == TYPE_MODE (unsigned_accum_type_node))
	return unsignedp ? sat_unsigned_accum_type_node
			 : unsigned_accum_type_node;
      if (mode == TYPE_MODE (unsigned_long_accum_type_node))
	return unsignedp ? sat_unsigned_long_accum_type_node
			 : unsigned_long_accum_type_node;
      if (mode == TYPE_MODE (unsigned_long_long_accum_type_node))
	return unsignedp ? sat_unsigned_long_long_accum_type_node
			 : unsigned_long_long_accum_type_node;

      if (mode == QQmode)
	return unsignedp ? sat_qq_type_node : qq_type_node;
      if (mode == HQmode)
	return unsignedp ? sat_hq_type_node : hq_type_node;
      if (mode == SQmode)
	return unsignedp ? sat_sq_type_node : sq_type_node;
      if (mode == DQmode)
	return unsignedp ? sat_dq_type_node : dq_type_node;
      if (mode == TQmode)
	return unsignedp ? sat_tq_type_node : tq_type_node;

      if (mode == UQQmode)
	return unsignedp ? sat_uqq_type_node : uqq_type_node;
      if (mode == UHQmode)
	return unsignedp ? sat_uhq_type_node : uhq_type_node;
      if (mode == USQmode)
	return unsignedp ? sat_usq_type_node : usq_type_node;
      if (mode == UDQmode)
	return unsignedp ? sat_udq_type_node : udq_type_node;
      if (mode == UTQmode)
	return unsignedp ? sat_utq_type_node : utq_type_node;

      if (mode == HAmode)
	return unsignedp ? sat_ha_type_node : ha_type_node;
      if (mode == SAmode)
	return unsignedp ? sat_sa_type_node : sa_type_node;
      if (mode == DAmode)
	return unsignedp ? sat_da_type_node : da_type_node;
      if (mode == TAmode)
	return unsignedp ? sat_ta_type_node : ta_type_node;

      if (mode == UHAmode)
	return unsignedp ? sat_uha_type_node : uha_type_node;
      if (mode == USAmode)
	return unsignedp ? sat_usa_type_node : usa_type_node;
      if (mode == UDAmode)
	return unsignedp ? sat_uda_type_node : uda_type_node;
      if (mode == UTAmode)
	return unsignedp ? sat_uta_type_node : uta_type_node;
    }

  for (t = registered_builtin_types; t; t = TREE_CHAIN (t))
    if (TYPE_MODE (TREE_VALUE (t)) == mode)
      return TREE_VALUE (t);

  return 0;
}

tree
c_common_unsigned_type (tree type)
{
  return c_common_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
c_common_signed_type (tree type)
{
  return c_common_signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
c_common_signed_or_unsigned_type (int unsignedp, tree type)
{
  tree type1;

  /* This block of code emulates the behavior of the old
     c_common_unsigned_type. In particular, it returns
     long_unsigned_type_node if passed a long, even when a int would
     have the same size. This is necessary for warnings to work
     correctly in archs where sizeof(int) == sizeof(long) */

  type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node || type1 == unsigned_char_type_node)
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (type1 == integer_type_node || type1 == unsigned_type_node)
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (type1 == short_integer_type_node || type1 == short_unsigned_type_node)
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (type1 == long_integer_type_node || type1 == long_unsigned_type_node)
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (type1 == long_long_integer_type_node || type1 == long_long_unsigned_type_node)
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;
  if (type1 == widest_integer_literal_type_node || type1 == widest_unsigned_literal_type_node)
    return unsignedp ? widest_unsigned_literal_type_node : widest_integer_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node || type1 == unsigned_intTI_type_node)
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (type1 == intDI_type_node || type1 == unsigned_intDI_type_node)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (type1 == intSI_type_node || type1 == unsigned_intSI_type_node)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (type1 == intHI_type_node || type1 == unsigned_intHI_type_node)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (type1 == intQI_type_node || type1 == unsigned_intQI_type_node)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

#define C_COMMON_FIXED_TYPES(NAME)	    \
  if (type1 == short_ ## NAME ## _type_node \
      || type1 == unsigned_short_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_short_ ## NAME ## _type_node \
		     : short_ ## NAME ## _type_node; \
  if (type1 == NAME ## _type_node \
      || type1 == unsigned_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_ ## NAME ## _type_node \
		     : NAME ## _type_node; \
  if (type1 == long_ ## NAME ## _type_node \
      || type1 == unsigned_long_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_long_ ## NAME ## _type_node \
		     : long_ ## NAME ## _type_node; \
  if (type1 == long_long_ ## NAME ## _type_node \
      || type1 == unsigned_long_long_ ## NAME ## _type_node) \
    return unsignedp ? unsigned_long_long_ ## NAME ## _type_node \
		     : long_long_ ## NAME ## _type_node;

#define C_COMMON_FIXED_MODE_TYPES(NAME) \
  if (type1 == NAME ## _type_node \
      || type1 == u ## NAME ## _type_node) \
    return unsignedp ? u ## NAME ## _type_node \
		     : NAME ## _type_node;

#define C_COMMON_FIXED_TYPES_SAT(NAME) \
  if (type1 == sat_ ## short_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_short_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_short_ ## NAME ## _type_node \
		     : sat_ ## short_ ## NAME ## _type_node; \
  if (type1 == sat_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_ ## NAME ## _type_node \
		     : sat_ ## NAME ## _type_node; \
  if (type1 == sat_ ## long_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_long_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_long_ ## NAME ## _type_node \
		     : sat_ ## long_ ## NAME ## _type_node; \
  if (type1 == sat_ ## long_long_ ## NAME ## _type_node \
      || type1 == sat_ ## unsigned_long_long_ ## NAME ## _type_node) \
    return unsignedp ? sat_ ## unsigned_long_long_ ## NAME ## _type_node \
		     : sat_ ## long_long_ ## NAME ## _type_node;

#define C_COMMON_FIXED_MODE_TYPES_SAT(NAME)	\
  if (type1 == sat_ ## NAME ## _type_node \
      || type1 == sat_ ## u ## NAME ## _type_node) \
    return unsignedp ? sat_ ## u ## NAME ## _type_node \
		     : sat_ ## NAME ## _type_node;

  C_COMMON_FIXED_TYPES (fract);
  C_COMMON_FIXED_TYPES_SAT (fract);
  C_COMMON_FIXED_TYPES (accum);
  C_COMMON_FIXED_TYPES_SAT (accum);

  C_COMMON_FIXED_MODE_TYPES (qq);
  C_COMMON_FIXED_MODE_TYPES (hq);
  C_COMMON_FIXED_MODE_TYPES (sq);
  C_COMMON_FIXED_MODE_TYPES (dq);
  C_COMMON_FIXED_MODE_TYPES (tq);
  C_COMMON_FIXED_MODE_TYPES_SAT (qq);
  C_COMMON_FIXED_MODE_TYPES_SAT (hq);
  C_COMMON_FIXED_MODE_TYPES_SAT (sq);
  C_COMMON_FIXED_MODE_TYPES_SAT (dq);
  C_COMMON_FIXED_MODE_TYPES_SAT (tq);
  C_COMMON_FIXED_MODE_TYPES (ha);
  C_COMMON_FIXED_MODE_TYPES (sa);
  C_COMMON_FIXED_MODE_TYPES (da);
  C_COMMON_FIXED_MODE_TYPES (ta);
  C_COMMON_FIXED_MODE_TYPES_SAT (ha);
  C_COMMON_FIXED_MODE_TYPES_SAT (sa);
  C_COMMON_FIXED_MODE_TYPES_SAT (da);
  C_COMMON_FIXED_MODE_TYPES_SAT (ta);

  /* For ENUMERAL_TYPEs in C++, must check the mode of the types, not
     the precision; they have precision set to match their range, but
     may use a wider mode to match an ABI.  If we change modes, we may
     wind up with bad conversions.  For INTEGER_TYPEs in C, must check
     the precision as well, so as to yield correct results for
     bit-field types.  C++ does not have these separate bit-field
     types, and producing a signed or unsigned variant of an
     ENUMERAL_TYPE may cause other problems as well.  */

  if (!INTEGRAL_TYPE_P (type)
      || TYPE_UNSIGNED (type) == unsignedp)
    return type;

#define TYPE_OK(node)							    \
  (TYPE_MODE (type) == TYPE_MODE (node)					    \
   && TYPE_PRECISION (type) == TYPE_PRECISION (node))
  if (TYPE_OK (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_OK (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_OK (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_OK (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_OK (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  if (TYPE_OK (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);

#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_OK (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_OK (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_OK (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_OK (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_OK (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
#undef TYPE_OK

  return build_nonstandard_integer_type (TYPE_PRECISION (type), unsignedp);
}

/* Build a bit-field integer type for the given WIDTH and UNSIGNEDP.  */

tree
c_build_bitfield_integer_type (unsigned HOST_WIDE_INT width, int unsignedp)
{
  /* Extended integer types of the same width as a standard type have
     lesser rank, so those of the same width as int promote to int or
     unsigned int and are valid for printf formats expecting int or
     unsigned int.  To avoid such special cases, avoid creating
     extended integer types for bit-fields if a standard integer type
     is available.  */
  if (width == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (width == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (width == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (width == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (width == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  return build_nonstandard_integer_type (width, unsignedp);
}

/* The C version of the register_builtin_type langhook.  */

void
c_register_builtin_type (tree type, const char* name)
{
  tree decl;

  decl = build_decl (UNKNOWN_LOCATION,
		     TYPE_DECL, get_identifier (name), type);
  DECL_ARTIFICIAL (decl) = 1;
  if (!TYPE_NAME (type))
    TYPE_NAME (type) = decl;
  pushdecl (decl);

  registered_builtin_types = tree_cons (0, type, registered_builtin_types);
}

/* Print an error message for invalid operands to arith operation
   CODE with TYPE0 for operand 0, and TYPE1 for operand 1.
   LOCATION is the location of the message.  */

void
binary_op_error (location_t location, enum tree_code code,
		 tree type0, tree type1)
{
  const char *opname;

  switch (code)
    {
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
    default:
      gcc_unreachable ();
    }
  error_at (location,
	    "invalid operands to binary %s (have %qT and %qT)", opname,
	    type0, type1);
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
shorten_compare (tree *op0_ptr, tree *op1_ptr, tree *restype_ptr,
		 enum tree_code *rescode_ptr)
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
    unsignedp0 = TYPE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TYPE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  Don't do this if
     the second arg is 0.  */

  if (TREE_CONSTANT (primop0)
      && !integer_zerop (primop1) && !real_zerop (primop1)
      && !fixed_zerop (primop1))
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
      && TREE_CODE (TREE_TYPE (primop0)) != FIXED_POINT_TYPE
      && TREE_CODE (primop1) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = TYPE_UNSIGNED (*restype_ptr);
      tree val;

      type = c_common_signed_or_unsigned_type (unsignedp0,
					       TREE_TYPE (primop0));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = c_common_signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
	{
	  /* Convert primop1 to target type, but do not introduce
	     additional overflow.  We know primop1 is an int_cst.  */
	  primop1 = force_fit_type_double (*restype_ptr,
					   TREE_INT_CST_LOW (primop1),
					   TREE_INT_CST_HIGH (primop1), 0,
					   TREE_OVERFLOW (primop1));
	}
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
	    val = truthvalue_true_node;
	}
      else if (code == EQ_EXPR)
	{
	  if (max_lt || min_gt)
	    val = truthvalue_false_node;
	}
      else if (code == LT_EXPR)
	{
	  if (max_lt)
	    val = truthvalue_true_node;
	  if (!min_lt)
	    val = truthvalue_false_node;
	}
      else if (code == GT_EXPR)
	{
	  if (min_gt)
	    val = truthvalue_true_node;
	  if (!max_gt)
	    val = truthvalue_false_node;
	}
      else if (code == LE_EXPR)
	{
	  if (!max_gt)
	    val = truthvalue_true_node;
	  if (min_gt)
	    val = truthvalue_false_node;
	}
      else if (code == GE_EXPR)
	{
	  if (!min_lt)
	    val = truthvalue_true_node;
	  if (max_lt)
	    val = truthvalue_false_node;
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
	  if (val == truthvalue_false_node)
	    warning (OPT_Wtype_limits, "comparison is always false due to limited range of data type");
	  if (val == truthvalue_true_node)
	    warning (OPT_Wtype_limits, "comparison is always true due to limited range of data type");
	}

      if (val != 0)
	{
	  /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	  if (TREE_SIDE_EFFECTS (primop0))
	    return build2 (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
	  return val;
	}

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }

  /* If either arg is decimal float and the other is float, find the
     proper common type to use for comparison.  */
  else if (real1 && real2
	   && (DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop0)))
	       || DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop1)))))
    type = common_type (TREE_TYPE (primop0), TREE_TYPE (primop1));

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
					       || TYPE_UNSIGNED (*restype_ptr),
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
	  && TYPE_UNSIGNED (*restype_ptr))
	{
	  tree value = 0;
	  switch (code)
	    {
	    case GE_EXPR:
	      /* All unsigned values are >= 0, so we warn.  However,
		 if OP0 is a constant that is >= 0, the signedness of
		 the comparison isn't an issue, so suppress the
		 warning.  */
	      if (warn_type_limits && !in_system_header
		  && !(TREE_CODE (primop0) == INTEGER_CST
		       && !TREE_OVERFLOW (convert (c_common_signed_type (type),
						   primop0))))
		warning (OPT_Wtype_limits, 
			 "comparison of unsigned expression >= 0 is always true");
	      value = truthvalue_true_node;
	      break;

	    case LT_EXPR:
	      if (warn_type_limits && !in_system_header
		  && !(TREE_CODE (primop0) == INTEGER_CST
		       && !TREE_OVERFLOW (convert (c_common_signed_type (type),
						   primop0))))
		warning (OPT_Wtype_limits, 
			 "comparison of unsigned expression < 0 is always false");
	      value = truthvalue_false_node;
	      break;

	    default:
	      break;
	    }

	  if (value != 0)
	    {
	      /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	      if (TREE_SIDE_EFFECTS (primop0))
		return build2 (COMPOUND_EXPR, TREE_TYPE (value),
			       primop0, value);
	      return value;
	    }
	}
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = truthvalue_type_node;

  return 0;
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

tree
pointer_int_sum (location_t loc, enum tree_code resultcode,
		 tree ptrop, tree intop)
{
  tree size_exp, ret;

  /* The result is a pointer of the same type that is being added.  */
  tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      pedwarn (loc, pedantic ? OPT_pedantic : OPT_Wpointer_arith, 
	       "pointer of type %<void *%> used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      pedwarn (loc, pedantic ? OPT_pedantic : OPT_Wpointer_arith, 
	       "pointer to a function used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == METHOD_TYPE)
    {
      pedwarn (loc, pedantic ? OPT_pedantic : OPT_Wpointer_arith, 
	       "pointer to member function used in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = size_in_bytes (TREE_TYPE (result_type));

  /* We are manipulating pointer values, so we don't need to warn
     about relying on undefined signed overflow.  We disable the
     warning here because we use integer types so fold won't know that
     they are really pointers.  */
  fold_defer_overflow_warnings ();

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */
  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && !TREE_CONSTANT (intop)
      && TREE_CONSTANT (TREE_OPERAND (intop, 1))
      && TREE_CONSTANT (size_exp)
      /* If the constant comes from pointer subtraction,
	 skip this optimization--it would cause an error.  */
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (intop, 0))) == INTEGER_TYPE
      /* If the constant is unsigned, and smaller than the pointer size,
	 then we must skip this optimization.  This is because it could cause
	 an overflow error if the constant is negative but INTOP is not.  */
      && (!TYPE_UNSIGNED (TREE_TYPE (intop))
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
      ptrop = build_binary_op (EXPR_LOCATION (TREE_OPERAND (intop, 1)),
			       subcode, ptrop,
			       convert (int_type, TREE_OPERAND (intop, 1)), 1);
      intop = convert (int_type, TREE_OPERAND (intop, 0));
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */
  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype)
      || TYPE_UNSIGNED (TREE_TYPE (intop)) != TYPE_UNSIGNED (sizetype))
    intop = convert (c_common_type_for_size (TYPE_PRECISION (sizetype),
					     TYPE_UNSIGNED (sizetype)), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate
     type for the pointer operation.  */
  intop = convert (sizetype,
		   build_binary_op (loc,
				    MULT_EXPR, intop,
				    convert (TREE_TYPE (intop), size_exp), 1));

  /* Create the sum or difference.  */
  if (resultcode == MINUS_EXPR)
    intop = fold_build1_loc (loc, NEGATE_EXPR, sizetype, intop);

  ret = fold_build2_loc (loc, POINTER_PLUS_EXPR, result_type, ptrop, intop);

  fold_undefer_and_ignore_overflow_warnings ();

  return ret;
}

/* Wrap a SAVE_EXPR around EXPR, if appropriate.  Like save_expr, but
   for C folds the inside expression and wraps a C_MAYBE_CONST_EXPR
   around the SAVE_EXPR if needed so that c_fully_fold does not need
   to look inside SAVE_EXPRs.  */

tree
c_save_expr (tree expr)
{
  bool maybe_const = true;
  if (c_dialect_cxx ())
    return save_expr (expr);
  expr = c_fully_fold (expr, false, &maybe_const);
  expr = save_expr (expr);
  if (!maybe_const)
    {
      expr = build2 (C_MAYBE_CONST_EXPR, TREE_TYPE (expr), NULL, expr);
      C_MAYBE_CONST_EXPR_NON_CONST (expr) = 1;
    }
  return expr;
}

/* Return whether EXPR is a declaration whose address can never be
   NULL.  */

bool
decl_with_nonnull_addr_p (const_tree expr)
{
  return (DECL_P (expr)
	  && (TREE_CODE (expr) == PARM_DECL
	      || TREE_CODE (expr) == LABEL_DECL
	      || !DECL_WEAK (expr)));
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or for an `if' or `while' statement or ?..: exp.  It should already
   have been validated to be of suitable type; otherwise, a bad
   diagnostic may result.

   The EXPR is located at LOCATION.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, truthvalue_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `truthvalue_type_node'.  */

tree
c_common_truthvalue_conversion (location_t location, tree expr)
{
  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:   case NE_EXPR:   case UNEQ_EXPR: case LTGT_EXPR:
    case LE_EXPR:   case GE_EXPR:   case LT_EXPR:   case GT_EXPR:
    case UNLE_EXPR: case UNGE_EXPR: case UNLT_EXPR: case UNGT_EXPR:
    case ORDERED_EXPR: case UNORDERED_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
	return expr;
      expr = build2 (TREE_CODE (expr), truthvalue_type_node,
		     TREE_OPERAND (expr, 0), TREE_OPERAND (expr, 1));
      goto ret;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
	return expr;
      expr = build2 (TREE_CODE (expr), truthvalue_type_node,
		     c_common_truthvalue_conversion (location, 
						     TREE_OPERAND (expr, 0)),
		     c_common_truthvalue_conversion (location,
						     TREE_OPERAND (expr, 1)));
      goto ret;

    case TRUTH_NOT_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
	return expr;
      expr = build1 (TREE_CODE (expr), truthvalue_type_node,
		     c_common_truthvalue_conversion (location,
						     TREE_OPERAND (expr, 0)));
      goto ret;

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? truthvalue_false_node
				  : truthvalue_true_node;

    case REAL_CST:
      return real_compare (NE_EXPR, &TREE_REAL_CST (expr), &dconst0)
	     ? truthvalue_true_node
	     : truthvalue_false_node;

    case FIXED_CST:
      return fixed_compare (NE_EXPR, &TREE_FIXED_CST (expr),
			    &FCONST0 (TYPE_MODE (TREE_TYPE (expr))))
	     ? truthvalue_true_node
	     : truthvalue_false_node;

    case FUNCTION_DECL:
      expr = build_unary_op (location, ADDR_EXPR, expr, 0);
      /* Fall through.  */

    case ADDR_EXPR:
      {
 	tree inner = TREE_OPERAND (expr, 0);
	if (decl_with_nonnull_addr_p (inner))
	  {
	    /* Common Ada/Pascal programmer's mistake.  */
	    warning_at (location,
			OPT_Waddress,
			"the address of %qD will always evaluate as %<true%>",
			inner);
	    return truthvalue_true_node;
	  }

	/* If we still have a decl, it is possible for its address to
	   be NULL, so we cannot optimize.  */
	if (DECL_P (inner))
	  {
	    gcc_assert (DECL_WEAK (inner));
	    break;
	  }

	if (TREE_SIDE_EFFECTS (inner))
	  {
	    expr = build2 (COMPOUND_EXPR, truthvalue_type_node,
			   inner, truthvalue_true_node);
	    goto ret;
	  }
	else
	  return truthvalue_true_node;
      }

    case COMPLEX_EXPR:
      expr = build_binary_op (EXPR_LOCATION (expr),
			      (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			       ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
		c_common_truthvalue_conversion (location,
						TREE_OPERAND (expr, 0)),
		c_common_truthvalue_conversion (location,
						TREE_OPERAND (expr, 1)),
			      0);
      goto ret;

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case EXCESS_PRECISION_EXPR:
      /* These don't change whether an object is nonzero or zero.  */
      return c_common_truthvalue_conversion (location, TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or nonzero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	{
	  expr = build2 (COMPOUND_EXPR, truthvalue_type_node,
			 TREE_OPERAND (expr, 1),
			 c_common_truthvalue_conversion 
			 (location, TREE_OPERAND (expr, 0)));
	  goto ret;
	}
      else
	return c_common_truthvalue_conversion (location,
					       TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      if (c_dialect_cxx ())
	{
	  expr = fold_build3_loc (location, COND_EXPR, truthvalue_type_node,
			      TREE_OPERAND (expr, 0),
			      c_common_truthvalue_conversion (location,
							      TREE_OPERAND (expr,
									    1)),
			      c_common_truthvalue_conversion (location,
							      TREE_OPERAND (expr,
									    2)));
	  goto ret;
	}
      else
	{
	  /* Folding will happen later for C.  */
	  expr = build3 (COND_EXPR, truthvalue_type_node,
			 TREE_OPERAND (expr, 0),
			 c_common_truthvalue_conversion (location,
							 TREE_OPERAND (expr, 1)),
			 c_common_truthvalue_conversion (location,
							 TREE_OPERAND (expr, 2)));
	  goto ret;
	}

    CASE_CONVERT:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return c_common_truthvalue_conversion (location,
					       TREE_OPERAND (expr, 0));
      break;

    case MODIFY_EXPR:
      if (!TREE_NO_WARNING (expr)
	  && warn_parentheses)
	{
	  warning (OPT_Wparentheses,
		   "suggest parentheses around assignment used as truth value");
	  TREE_NO_WARNING (expr) = 1;
	}
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree t = c_save_expr (expr);
      expr = (build_binary_op
	      (EXPR_LOCATION (expr),
	       (TREE_SIDE_EFFECTS (expr)
		? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	c_common_truthvalue_conversion
	       (location,
		build_unary_op (location, REALPART_EXPR, t, 0)),
	c_common_truthvalue_conversion
	       (location,
		build_unary_op (location, IMAGPART_EXPR, t, 0)),
	       0));
      goto ret;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == FIXED_POINT_TYPE)
    {
      tree fixed_zero_node = build_fixed (TREE_TYPE (expr),
					  FCONST0 (TYPE_MODE
						   (TREE_TYPE (expr))));
      return build_binary_op (location, NE_EXPR, expr, fixed_zero_node, 1);
    }
  else
    return build_binary_op (location, NE_EXPR, expr, integer_zero_node, 1);

 ret:
  protected_set_expr_location (expr, location);
  return expr;
}

static void def_builtin_1  (enum built_in_function fncode,
			    const char *name,
			    enum built_in_class fnclass,
			    tree fntype, tree libtype,
			    bool both_p, bool fallback_p, bool nonansi_p,
			    tree fnattrs, bool implicit_p);


/* Apply the TYPE_QUALS to the new DECL.  */

void
c_apply_type_quals_to_decl (int type_quals, tree decl)
{
  tree type = TREE_TYPE (decl);

  if (type == error_mark_node)
    return;

  if (((type_quals & TYPE_QUAL_CONST)
       || (type && TREE_CODE (type) == REFERENCE_TYPE))
      /* An object declared 'const' is only readonly after it is
	 initialized.  We don't have any way of expressing this currently,
	 so we need to be conservative and unset TREE_READONLY for types
	 with constructors.  Otherwise aliasing code will ignore stores in
	 an inline constructor.  */
      && !(type && TYPE_NEEDS_CONSTRUCTING (type)))
    TREE_READONLY (decl) = 1;
  if (type_quals & TYPE_QUAL_VOLATILE)
    {
      TREE_SIDE_EFFECTS (decl) = 1;
      TREE_THIS_VOLATILE (decl) = 1;
    }
  if (type_quals & TYPE_QUAL_RESTRICT)
    {
      while (type && TREE_CODE (type) == ARRAY_TYPE)
	/* Allow 'restrict' on arrays of pointers.
	   FIXME currently we just ignore it.  */
	type = TREE_TYPE (type);
      if (!type
	  || !POINTER_TYPE_P (type)
	  || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (type)))
	error ("invalid use of %<restrict%>");
    }
}

/* Hash function for the problem of multiple type definitions in
   different files.  This must hash all types that will compare
   equal via comptypes to the same value.  In practice it hashes
   on some of the simple stuff and leaves the details to comptypes.  */

static hashval_t
c_type_hash (const void *p)
{
  int i = 0;
  int shift, size;
  const_tree const t = (const_tree) p;
  tree t2;
  switch (TREE_CODE (t))
    {
    /* For pointers, hash on pointee type plus some swizzling.  */
    case POINTER_TYPE:
      return c_type_hash (TREE_TYPE (t)) ^ 0x3003003;
    /* Hash on number of elements and total size.  */
    case ENUMERAL_TYPE:
      shift = 3;
      t2 = TYPE_VALUES (t);
      break;
    case RECORD_TYPE:
      shift = 0;
      t2 = TYPE_FIELDS (t);
      break;
    case QUAL_UNION_TYPE:
      shift = 1;
      t2 = TYPE_FIELDS (t);
      break;
    case UNION_TYPE:
      shift = 2;
      t2 = TYPE_FIELDS (t);
      break;
    default:
      gcc_unreachable ();
    }
  for (; t2; t2 = TREE_CHAIN (t2))
    i++;
  /* We might have a VLA here.  */
  if (TREE_CODE (TYPE_SIZE (t)) != INTEGER_CST)
    size = 0;
  else
    size = TREE_INT_CST_LOW (TYPE_SIZE (t));
  return ((size << 24) | (i << shift));
}

static GTY((param_is (union tree_node))) htab_t type_hash_table;

/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

alias_set_type
c_common_get_alias_set (tree t)
{
  tree u;
  PTR *slot;

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
  if (!TYPE_P (t))
    return -1;

  /* The C standard guarantees that any object may be accessed via an
     lvalue that has character type.  */
  if (t == char_type_node
      || t == signed_char_type_node
      || t == unsigned_char_type_node)
    return 0;

  /* The C standard specifically allows aliasing between signed and
     unsigned variants of the same type.  We treat the signed
     variant as canonical.  */
  if (TREE_CODE (t) == INTEGER_TYPE && TYPE_UNSIGNED (t))
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
	    const int* const* cipp = ipp;

	 And, it doesn't make sense for that to be legal unless you
	 can dereference IPP and CIPP.  So, we ignore cv-qualifiers on
	 the pointed-to types.  This issue has been reported to the
	 C++ committee.  */
      t1 = build_type_no_quals (t);
      if (t1 != t)
	return get_alias_set (t1);
    }

  /* Handle the case of multiple type nodes referring to "the same" type,
     which occurs with IMA.  These share an alias set.  FIXME:  Currently only
     C90 is handled.  (In C99 type compatibility is not transitive, which
     complicates things mightily. The alias set splay trees can theoretically
     represent this, but insertion is tricky when you consider all the
     different orders things might arrive in.) */

  if (c_language != clk_c || flag_isoc99)
    return -1;

  /* Save time if there's only one input file.  */
  if (num_in_fnames == 1)
    return -1;

  /* Pointers need special handling if they point to any type that
     needs special handling (below).  */
  if (TREE_CODE (t) == POINTER_TYPE)
    {
      tree t2;
      /* Find bottom type under any nested POINTERs.  */
      for (t2 = TREE_TYPE (t);
	   TREE_CODE (t2) == POINTER_TYPE;
	   t2 = TREE_TYPE (t2))
	;
      if (TREE_CODE (t2) != RECORD_TYPE
	  && TREE_CODE (t2) != ENUMERAL_TYPE
	  && TREE_CODE (t2) != QUAL_UNION_TYPE
	  && TREE_CODE (t2) != UNION_TYPE)
	return -1;
      if (TYPE_SIZE (t2) == 0)
	return -1;
    }
  /* These are the only cases that need special handling.  */
  if (TREE_CODE (t) != RECORD_TYPE
      && TREE_CODE (t) != ENUMERAL_TYPE
      && TREE_CODE (t) != QUAL_UNION_TYPE
      && TREE_CODE (t) != UNION_TYPE
      && TREE_CODE (t) != POINTER_TYPE)
    return -1;
  /* Undefined? */
  if (TYPE_SIZE (t) == 0)
    return -1;

  /* Look up t in hash table.  Only one of the compatible types within each
     alias set is recorded in the table.  */
  if (!type_hash_table)
    type_hash_table = htab_create_ggc (1021, c_type_hash,
	    (htab_eq) lang_hooks.types_compatible_p,
	    NULL);
  slot = htab_find_slot (type_hash_table, t, INSERT);
  if (*slot != NULL)
    {
      TYPE_ALIAS_SET (t) = TYPE_ALIAS_SET ((tree)*slot);
      return TYPE_ALIAS_SET ((tree)*slot);
    }
  else
    /* Our caller will assign and record (in t) a new alias set; all we need
       to do is remember t in the hash table.  */
    *slot = t;

  return -1;
}

/* Compute the value of 'sizeof (TYPE)' or '__alignof__ (TYPE)', where
   the second parameter indicates which OPERATOR is being applied.
   The COMPLAIN flag controls whether we should diagnose possibly
   ill-formed constructs or not.  LOC is the location of the SIZEOF or
   TYPEOF operator.  */

tree
c_sizeof_or_alignof_type (location_t loc,
			  tree type, bool is_sizeof, int complain)
{
  const char *op_name;
  tree value = NULL;
  enum tree_code type_code = TREE_CODE (type);

  op_name = is_sizeof ? "sizeof" : "__alignof__";

  if (type_code == FUNCTION_TYPE)
    {
      if (is_sizeof)
	{
	  if (complain && (pedantic || warn_pointer_arith))
	    pedwarn (loc, pedantic ? OPT_pedantic : OPT_Wpointer_arith, 
		     "invalid application of %<sizeof%> to a function type");
          else if (!complain)
            return error_mark_node;
	  value = size_one_node;
	}
      else
	value = size_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);
    }
  else if (type_code == VOID_TYPE || type_code == ERROR_MARK)
    {
      if (type_code == VOID_TYPE
	  && complain && (pedantic || warn_pointer_arith))
	pedwarn (loc, pedantic ? OPT_pedantic : OPT_Wpointer_arith, 
		 "invalid application of %qs to a void type", op_name);
      else if (!complain)
        return error_mark_node;
      value = size_one_node;
    }
  else if (!COMPLETE_TYPE_P (type))
    {
      if (complain)
	error_at (loc, "invalid application of %qs to incomplete type %qT ",
		  op_name, type);
      value = size_zero_node;
    }
  else
    {
      if (is_sizeof)
	/* Convert in case a char is more than one unit.  */
	value = size_binop_loc (loc, CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
				size_int (TYPE_PRECISION (char_type_node)
					  / BITS_PER_UNIT));
      else
	value = size_int (TYPE_ALIGN_UNIT (type));
    }

  /* VALUE will have an integer type with TYPE_IS_SIZETYPE set.
     TYPE_IS_SIZETYPE means that certain things (like overflow) will
     never happen.  However, this node should really have type
     `size_t', which is just a typedef for an ordinary integer type.  */
  value = fold_convert_loc (loc, size_type_node, value);
  gcc_assert (!TYPE_IS_SIZETYPE (TREE_TYPE (value)));

  return value;
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of EXPR, measured in bytes.  For VAR_DECLs,
   FUNCTION_DECLs and FIELD_DECLs return DECL_ALIGN (which can be set
   from an "aligned" __attribute__ specification).  LOC is the
   location of the ALIGNOF operator.  */

tree
c_alignof_expr (location_t loc, tree expr)
{
  tree t;

  if (VAR_OR_FUNCTION_DECL_P (expr))
    t = size_int (DECL_ALIGN_UNIT (expr));

  else if (TREE_CODE (expr) == COMPONENT_REF
	   && DECL_C_BIT_FIELD (TREE_OPERAND (expr, 1)))
    {
      error_at (loc, "%<__alignof%> applied to a bit-field");
      t = size_one_node;
    }
  else if (TREE_CODE (expr) == COMPONENT_REF
	   && TREE_CODE (TREE_OPERAND (expr, 1)) == FIELD_DECL)
    t = size_int (DECL_ALIGN_UNIT (TREE_OPERAND (expr, 1)));

  else if (TREE_CODE (expr) == INDIRECT_REF)
    {
      tree t = TREE_OPERAND (expr, 0);
      tree best = t;
      int bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));

      while (CONVERT_EXPR_P (t)
	     && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
	{
	  int thisalign;

	  t = TREE_OPERAND (t, 0);
	  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
	  if (thisalign > bestalign)
	    best = t, bestalign = thisalign;
	}
      return c_alignof (loc, TREE_TYPE (TREE_TYPE (best)));
    }
  else
    return c_alignof (loc, TREE_TYPE (expr));

  return fold_convert_loc (loc, size_type_node, t);
}

/* Handle C and C++ default attributes.  */

enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
  ATTR_LAST
};

static GTY(()) tree built_in_attributes[(int) ATTR_LAST];

static void c_init_attributes (void);

enum c_builtin_type
{
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG6) \
  NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_POINTER_TYPE
  BT_LAST
};

typedef enum c_builtin_type builtin_type;

/* A temporary array for c_common_nodes_and_builtins.  Used in
   communication with def_fn_type.  */
static tree builtin_types[(int) BT_LAST + 1];

/* A helper function for c_common_nodes_and_builtins.  Build function type
   for DEF with return type RET and N arguments.  If VAR is true, then the
   function should be variadic after those N arguments.

   Takes special care not to ICE if any of the types involved are
   error_mark_node, which indicates that said type is not in fact available
   (see builtin_type_for_size).  In which case the function type as a whole
   should be error_mark_node.  */

static void
def_fn_type (builtin_type def, builtin_type ret, bool var, int n, ...)
{
  tree args = NULL, t;
  va_list list;
  int i;

  va_start (list, n);
  for (i = 0; i < n; ++i)
    {
      builtin_type a = (builtin_type) va_arg (list, int);
      t = builtin_types[a];
      if (t == error_mark_node)
	goto egress;
      args = tree_cons (NULL_TREE, t, args);
    }
  va_end (list);

  args = nreverse (args);
  if (!var)
    args = chainon (args, void_list_node);

  t = builtin_types[ret];
  if (t == error_mark_node)
    goto egress;
  t = build_function_type (t, args);

 egress:
  builtin_types[def] = t;
}

/* Build builtin functions common to both C and C++ language
   frontends.  */

static void
c_define_builtins (tree va_list_ref_type_node, tree va_list_arg_type_node)
{
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 0, 1, ARG1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 0, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 0, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 0, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5)	\
  def_fn_type (ENUM, RETURN, 0, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  def_fn_type (ENUM, RETURN, 0, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  def_fn_type (ENUM, RETURN, 0, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 1, 1, ARG1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 1, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 1, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 1, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 1, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
  builtin_types[(int) ENUM] = build_pointer_type (builtin_types[(int) TYPE]);

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  c_init_attributes ();

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P, \
		    NONANSI_P, ATTRS, IMPLICIT, COND)			\
  if (NAME && COND)							\
    def_builtin_1 (ENUM, NAME, CLASS,                                   \
		   builtin_types[(int) TYPE],                           \
		   builtin_types[(int) LIBTYPE],                        \
		   BOTH_P, FALLBACK_P, NONANSI_P,                       \
		   built_in_attributes[(int) ATTRS], IMPLICIT);
#include "builtins.def"
#undef DEF_BUILTIN

  targetm.init_builtins ();

  build_common_builtin_nodes ();

  if (flag_mudflap)
    mudflap_init ();
}

/* Like get_identifier, but avoid warnings about null arguments when
   the argument may be NULL for targets where GCC lacks stdint.h type
   information.  */

static inline tree
c_get_ident (const char *id)
{
  return get_identifier (id);
}

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.  */

void
c_common_nodes_and_builtins (void)
{
  int char16_type_size;
  int char32_type_size;
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
  if (c_dialect_cxx ())
    record_builtin_type (RID_SIGNED, NULL, integer_type_node);
  record_builtin_type (RID_LONG, "long int", long_integer_type_node);
  record_builtin_type (RID_UNSIGNED, "unsigned int", unsigned_type_node);
  record_builtin_type (RID_MAX, "long unsigned int",
		       long_unsigned_type_node);
  if (c_dialect_cxx ())
    record_builtin_type (RID_MAX, "unsigned long", long_unsigned_type_node);
  record_builtin_type (RID_MAX, "long long int",
		       long_long_integer_type_node);
  record_builtin_type (RID_MAX, "long long unsigned int",
		       long_long_unsigned_type_node);
  if (c_dialect_cxx ())
    record_builtin_type (RID_MAX, "long long unsigned",
			 long_long_unsigned_type_node);
  record_builtin_type (RID_SHORT, "short int", short_integer_type_node);
  record_builtin_type (RID_MAX, "short unsigned int",
		       short_unsigned_type_node);
  if (c_dialect_cxx ())
    record_builtin_type (RID_MAX, "unsigned short",
			 short_unsigned_type_node);

  /* Define both `signed char' and `unsigned char'.  */
  record_builtin_type (RID_MAX, "signed char", signed_char_type_node);
  record_builtin_type (RID_MAX, "unsigned char", unsigned_char_type_node);

  /* These are types that c_common_type_for_size and
     c_common_type_for_mode use.  */
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 intQI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 intHI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 intSI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  if (targetm.scalar_mode_supported_p (TImode))
    lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					   TYPE_DECL,
					   get_identifier ("__int128_t"),
					   intTI_type_node));
#endif
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 unsigned_intQI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 unsigned_intHI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 unsigned_intSI_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 unsigned_intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  if (targetm.scalar_mode_supported_p (TImode))
    lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					   TYPE_DECL,
					   get_identifier ("__uint128_t"),
					   unsigned_intTI_type_node));
#endif

  /* Create the widest literal types.  */
  widest_integer_literal_type_node
    = make_signed_type (HOST_BITS_PER_WIDE_INT * 2);
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 widest_integer_literal_type_node));

  widest_unsigned_literal_type_node
    = make_unsigned_type (HOST_BITS_PER_WIDE_INT * 2);
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL, NULL_TREE,
					 widest_unsigned_literal_type_node));

  /* `unsigned long' is the standard type for sizeof.
     Note that stddef.h uses `unsigned long',
     and this must agree, even if long and int are the same size.  */
  size_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (SIZE_TYPE)));
  signed_size_type_node = c_common_signed_type (size_type_node);
  set_sizetype (size_type_node);

  pid_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (PID_TYPE)));

  build_common_tree_nodes_2 (flag_short_double);

  record_builtin_type (RID_FLOAT, NULL, float_type_node);
  record_builtin_type (RID_DOUBLE, NULL, double_type_node);
  record_builtin_type (RID_MAX, "long double", long_double_type_node);

  /* Only supported decimal floating point extension if the target
     actually supports underlying modes. */
  if (targetm.scalar_mode_supported_p (SDmode) 
      && targetm.scalar_mode_supported_p (DDmode)
      && targetm.scalar_mode_supported_p (TDmode))
    {
      record_builtin_type (RID_DFLOAT32, NULL, dfloat32_type_node);
      record_builtin_type (RID_DFLOAT64, NULL, dfloat64_type_node);
      record_builtin_type (RID_DFLOAT128, NULL, dfloat128_type_node);
    }

  if (targetm.fixed_point_supported_p ())
    {
      record_builtin_type (RID_MAX, "short _Fract", short_fract_type_node);
      record_builtin_type (RID_FRACT, NULL, fract_type_node);
      record_builtin_type (RID_MAX, "long _Fract", long_fract_type_node);
      record_builtin_type (RID_MAX, "long long _Fract",
			   long_long_fract_type_node);
      record_builtin_type (RID_MAX, "unsigned short _Fract",
			   unsigned_short_fract_type_node);
      record_builtin_type (RID_MAX, "unsigned _Fract",
			   unsigned_fract_type_node);
      record_builtin_type (RID_MAX, "unsigned long _Fract",
			   unsigned_long_fract_type_node);
      record_builtin_type (RID_MAX, "unsigned long long _Fract",
			   unsigned_long_long_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat short _Fract",
			   sat_short_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat _Fract", sat_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat long _Fract",
			   sat_long_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat long long _Fract",
			   sat_long_long_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned short _Fract",
			   sat_unsigned_short_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned _Fract",
			   sat_unsigned_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned long _Fract",
			   sat_unsigned_long_fract_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned long long _Fract",
			   sat_unsigned_long_long_fract_type_node);
      record_builtin_type (RID_MAX, "short _Accum", short_accum_type_node);
      record_builtin_type (RID_ACCUM, NULL, accum_type_node);
      record_builtin_type (RID_MAX, "long _Accum", long_accum_type_node);
      record_builtin_type (RID_MAX, "long long _Accum",
			   long_long_accum_type_node);
      record_builtin_type (RID_MAX, "unsigned short _Accum",
			   unsigned_short_accum_type_node);
      record_builtin_type (RID_MAX, "unsigned _Accum",
			   unsigned_accum_type_node);
      record_builtin_type (RID_MAX, "unsigned long _Accum",
			   unsigned_long_accum_type_node);
      record_builtin_type (RID_MAX, "unsigned long long _Accum",
			   unsigned_long_long_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat short _Accum",
			   sat_short_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat _Accum", sat_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat long _Accum",
			   sat_long_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat long long _Accum",
			  sat_long_long_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned short _Accum",
			   sat_unsigned_short_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned _Accum",
			   sat_unsigned_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned long _Accum",
			   sat_unsigned_long_accum_type_node);
      record_builtin_type (RID_MAX, "_Sat unsigned long long _Accum",
			   sat_unsigned_long_long_accum_type_node);

    }

  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL,
					 get_identifier ("complex int"),
					 complex_integer_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL,
					 get_identifier ("complex float"),
					 complex_float_type_node));
  lang_hooks.decls.pushdecl (build_decl (UNKNOWN_LOCATION,
					 TYPE_DECL,
					 get_identifier ("complex double"),
					 complex_double_type_node));
  lang_hooks.decls.pushdecl
    (build_decl (UNKNOWN_LOCATION,
		 TYPE_DECL, get_identifier ("complex long double"),
		 complex_long_double_type_node));

  if (c_dialect_cxx ())
    /* For C++, make fileptr_type_node a distinct void * type until
       FILE type is defined.  */
    fileptr_type_node = build_variant_type_copy (ptr_type_node);

  record_builtin_type (RID_VOID, NULL, void_type_node);

  /* Set the TYPE_NAME for any variants that were built before
     record_builtin_type gave names to the built-in types. */
  {
    tree void_name = TYPE_NAME (void_type_node);
    TYPE_NAME (void_type_node) = NULL_TREE;
    TYPE_NAME (build_qualified_type (void_type_node, TYPE_QUAL_CONST))
      = void_name;
    TYPE_NAME (void_type_node) = void_name;
  }

  /* This node must not be shared.  */
  void_zero_node = make_node (INTEGER_CST);
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
  underlying_wchar_type_node = wchar_type_node;
  if (c_dialect_cxx ())
    {
      if (TYPE_UNSIGNED (wchar_type_node))
	wchar_type_node = make_unsigned_type (wchar_type_size);
      else
	wchar_type_node = make_signed_type (wchar_type_size);
      record_builtin_type (RID_WCHAR, "wchar_t", wchar_type_node);
    }

  /* This is for wide string constants.  */
  wchar_array_type_node
    = build_array_type (wchar_type_node, array_domain_type);

  /* Define 'char16_t'.  */
  char16_type_node = get_identifier (CHAR16_TYPE);
  char16_type_node = TREE_TYPE (identifier_global_value (char16_type_node));
  char16_type_size = TYPE_PRECISION (char16_type_node);
  if (c_dialect_cxx ())
    {
      char16_type_node = make_unsigned_type (char16_type_size);

      if (cxx_dialect == cxx0x)
	record_builtin_type (RID_CHAR16, "char16_t", char16_type_node);
    }

  /* This is for UTF-16 string constants.  */
  char16_array_type_node
    = build_array_type (char16_type_node, array_domain_type);

  /* Define 'char32_t'.  */
  char32_type_node = get_identifier (CHAR32_TYPE);
  char32_type_node = TREE_TYPE (identifier_global_value (char32_type_node));
  char32_type_size = TYPE_PRECISION (char32_type_node);
  if (c_dialect_cxx ())
    {
      char32_type_node = make_unsigned_type (char32_type_size);

      if (cxx_dialect == cxx0x)
	record_builtin_type (RID_CHAR32, "char32_t", char32_type_node);
    }

  /* This is for UTF-32 string constants.  */
  char32_array_type_node
    = build_array_type (char32_type_node, array_domain_type);

  wint_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (WINT_TYPE)));

  intmax_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (INTMAX_TYPE)));
  uintmax_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (UINTMAX_TYPE)));

  if (SIG_ATOMIC_TYPE)
    sig_atomic_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (SIG_ATOMIC_TYPE)));
  if (INT8_TYPE)
    int8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT8_TYPE)));
  if (INT16_TYPE)
    int16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT16_TYPE)));
  if (INT32_TYPE)
    int32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT32_TYPE)));
  if (INT64_TYPE)
    int64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT64_TYPE)));
  if (UINT8_TYPE)
    uint8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT8_TYPE)));
  if (UINT16_TYPE)
    uint16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT16_TYPE)));
  if (UINT32_TYPE)
    c_uint32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT32_TYPE)));
  if (UINT64_TYPE)
    c_uint64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT64_TYPE)));
  if (INT_LEAST8_TYPE)
    int_least8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_LEAST8_TYPE)));
  if (INT_LEAST16_TYPE)
    int_least16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_LEAST16_TYPE)));
  if (INT_LEAST32_TYPE)
    int_least32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_LEAST32_TYPE)));
  if (INT_LEAST64_TYPE)
    int_least64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_LEAST64_TYPE)));
  if (UINT_LEAST8_TYPE)
    uint_least8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_LEAST8_TYPE)));
  if (UINT_LEAST16_TYPE)
    uint_least16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_LEAST16_TYPE)));
  if (UINT_LEAST32_TYPE)
    uint_least32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_LEAST32_TYPE)));
  if (UINT_LEAST64_TYPE)
    uint_least64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_LEAST64_TYPE)));
  if (INT_FAST8_TYPE)
    int_fast8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_FAST8_TYPE)));
  if (INT_FAST16_TYPE)
    int_fast16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_FAST16_TYPE)));
  if (INT_FAST32_TYPE)
    int_fast32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_FAST32_TYPE)));
  if (INT_FAST64_TYPE)
    int_fast64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INT_FAST64_TYPE)));
  if (UINT_FAST8_TYPE)
    uint_fast8_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_FAST8_TYPE)));
  if (UINT_FAST16_TYPE)
    uint_fast16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_FAST16_TYPE)));
  if (UINT_FAST32_TYPE)
    uint_fast32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_FAST32_TYPE)));
  if (UINT_FAST64_TYPE)
    uint_fast64_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT_FAST64_TYPE)));
  if (INTPTR_TYPE)
    intptr_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (INTPTR_TYPE)));
  if (UINTPTR_TYPE)
    uintptr_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINTPTR_TYPE)));

  default_function_type = build_function_type (integer_type_node, NULL_TREE);
  ptrdiff_type_node
    = TREE_TYPE (identifier_global_value (get_identifier (PTRDIFF_TYPE)));
  unsigned_ptrdiff_type_node = c_common_unsigned_type (ptrdiff_type_node);

  lang_hooks.decls.pushdecl
    (build_decl (UNKNOWN_LOCATION,
		 TYPE_DECL, get_identifier ("__builtin_va_list"),
		 va_list_type_node));
#ifdef TARGET_ENUM_VA_LIST
  {
    int l;
    const char *pname;
    tree ptype;
    for (l = 0; TARGET_ENUM_VA_LIST (l, &pname, &ptype); ++l)
      {
	lang_hooks.decls.pushdecl
	  (build_decl (UNKNOWN_LOCATION,
		       TYPE_DECL, get_identifier (pname),
	  	       ptype));

      }
  }
#endif

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

  if (!flag_preprocess_only)
    c_define_builtins (va_list_ref_type_node, va_list_arg_type_node);

  main_identifier_node = get_identifier ("main");

  /* Create the built-in __null node.  It is important that this is
     not shared.  */
  null_node = make_node (INTEGER_CST);
  TREE_TYPE (null_node) = c_common_type_for_size (POINTER_SIZE, 0);

  /* Since builtin_types isn't gc'ed, don't export these nodes.  */
  memset (builtin_types, 0, sizeof (builtin_types));
}

/* Look up the function in built_in_decls that corresponds to DECL
   and set ASMSPEC as its user assembler name.  DECL must be a
   function decl that declares a builtin.  */

void
set_builtin_user_assembler_name (tree decl, const char *asmspec)
{
  tree builtin;
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
	      && asmspec != 0);

  builtin = built_in_decls [DECL_FUNCTION_CODE (decl)];
  set_user_assembler_name (builtin, asmspec);
  switch (DECL_FUNCTION_CODE (decl))
    {
    case BUILT_IN_MEMCPY:
      init_block_move_fn (asmspec);
      memcpy_libfunc = set_user_assembler_libfunc ("memcpy", asmspec);
      break;
    case BUILT_IN_MEMSET:
      init_block_clear_fn (asmspec);
      memset_libfunc = set_user_assembler_libfunc ("memset", asmspec);
      break;
    case BUILT_IN_MEMMOVE:
      memmove_libfunc = set_user_assembler_libfunc ("memmove", asmspec);
      break;
    case BUILT_IN_MEMCMP:
      memcmp_libfunc = set_user_assembler_libfunc ("memcmp", asmspec);
      break;
    case BUILT_IN_ABORT:
      abort_libfunc = set_user_assembler_libfunc ("abort", asmspec);
      break;
    default:
      break;
    }
}

/* The number of named compound-literals generated thus far.  */
static GTY(()) int compound_literal_number;

/* Set DECL_NAME for DECL, a VAR_DECL for a compound-literal.  */

void
set_compound_literal_name (tree decl)
{
  char *name;
  ASM_FORMAT_PRIVATE_NAME (name, "__compound_literal",
			   compound_literal_number);
  compound_literal_number++;
  DECL_NAME (decl) = get_identifier (name);
}

tree
build_va_arg (location_t loc, tree expr, tree type)
{
  expr = build1 (VA_ARG_EXPR, type, expr);
  SET_EXPR_LOCATION (expr, loc);
  return expr;
}


/* Linked list of disabled built-in functions.  */

typedef struct disabled_builtin
{
  const char *name;
  struct disabled_builtin *next;
} disabled_builtin;
static disabled_builtin *disabled_builtins = NULL;

static bool builtin_function_disabled_p (const char *);

/* Disable a built-in function specified by -fno-builtin-NAME.  If NAME
   begins with "__builtin_", give an error.  */

void
disable_builtin_function (const char *name)
{
  if (strncmp (name, "__builtin_", strlen ("__builtin_")) == 0)
    error ("cannot disable built-in function %qs", name);
  else
    {
      disabled_builtin *new_disabled_builtin = XNEW (disabled_builtin);
      new_disabled_builtin->name = name;
      new_disabled_builtin->next = disabled_builtins;
      disabled_builtins = new_disabled_builtin;
    }
}


/* Return true if the built-in function NAME has been disabled, false
   otherwise.  */

static bool
builtin_function_disabled_p (const char *name)
{
  disabled_builtin *p;
  for (p = disabled_builtins; p != NULL; p = p->next)
    {
      if (strcmp (name, p->name) == 0)
	return true;
    }
  return false;
}


/* Worker for DEF_BUILTIN.
   Possibly define a builtin function with one or two names.
   Does not declare a non-__builtin_ function if flag_no_builtin, or if
   nonansi_p and flag_no_nonansi_builtin.  */

static void
def_builtin_1 (enum built_in_function fncode,
	       const char *name,
	       enum built_in_class fnclass,
	       tree fntype, tree libtype,
	       bool both_p, bool fallback_p, bool nonansi_p,
	       tree fnattrs, bool implicit_p)
{
  tree decl;
  const char *libname;

  if (fntype == error_mark_node)
    return;

  gcc_assert ((!both_p && !fallback_p)
	      || !strncmp (name, "__builtin_",
			   strlen ("__builtin_")));

  libname = name + strlen ("__builtin_");
  decl = add_builtin_function (name, fntype, fncode, fnclass,
			       (fallback_p ? libname : NULL),
			       fnattrs);
  if (both_p
      && !flag_no_builtin && !builtin_function_disabled_p (libname)
      && !(nonansi_p && flag_no_nonansi_builtin))
    add_builtin_function (libname, libtype, fncode, fnclass,
			  NULL, fnattrs);

  built_in_decls[(int) fncode] = decl;
  if (implicit_p)
    implicit_built_in_decls[(int) fncode] = decl;
}

/* Nonzero if the type T promotes to int.  This is (nearly) the
   integral promotions defined in ISO C99 6.3.1.1/2.  */

bool
c_promoting_integer_type_p (const_tree t)
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
self_promoting_args_p (const_tree parms)
{
  const_tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);

      if (type == error_mark_node)
	continue;

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

/* Recursively remove any '*' or '&' operator from TYPE.  */
tree
strip_pointer_operator (tree t)
{
  while (POINTER_TYPE_P (t))
    t = TREE_TYPE (t);
  return t;
}

/* Recursively remove pointer or array type from TYPE. */
tree
strip_pointer_or_array_types (tree t)
{
  while (TREE_CODE (t) == ARRAY_TYPE || POINTER_TYPE_P (t))
    t = TREE_TYPE (t);
  return t;
}

/* Used to compare case labels.  K1 and K2 are actually tree nodes
   representing case labels, or NULL_TREE for a `default' label.
   Returns -1 if K1 is ordered before K2, -1 if K1 is ordered after
   K2, and 0 if K1 and K2 are equal.  */

int
case_compare (splay_tree_key k1, splay_tree_key k2)
{
  /* Consider a NULL key (such as arises with a `default' label) to be
     smaller than anything else.  */
  if (!k1)
    return k2 ? -1 : 0;
  else if (!k2)
    return k1 ? 1 : 0;

  return tree_int_cst_compare ((tree) k1, (tree) k2);
}

/* Process a case label, located at LOC, for the range LOW_VALUE
   ... HIGH_VALUE.  If LOW_VALUE and HIGH_VALUE are both NULL_TREE
   then this case label is actually a `default' label.  If only
   HIGH_VALUE is NULL_TREE, then case label was declared using the
   usual C/C++ syntax, rather than the GNU case range extension.
   CASES is a tree containing all the case ranges processed so far;
   COND is the condition for the switch-statement itself.  Returns the
   CASE_LABEL_EXPR created, or ERROR_MARK_NODE if no CASE_LABEL_EXPR
   is created.  */

tree
c_add_case_label (location_t loc, splay_tree cases, tree cond, tree orig_type,
		  tree low_value, tree high_value)
{
  tree type;
  tree label;
  tree case_label;
  splay_tree_node node;

  /* Create the LABEL_DECL itself.  */
  label = create_artificial_label (loc);

  /* If there was an error processing the switch condition, bail now
     before we get more confused.  */
  if (!cond || cond == error_mark_node)
    goto error_out;

  if ((low_value && TREE_TYPE (low_value)
       && POINTER_TYPE_P (TREE_TYPE (low_value)))
      || (high_value && TREE_TYPE (high_value)
	  && POINTER_TYPE_P (TREE_TYPE (high_value))))
    {
      error_at (loc, "pointers are not permitted as case values");
      goto error_out;
    }

  /* Case ranges are a GNU extension.  */
  if (high_value)
    pedwarn (loc, OPT_pedantic, 
	     "range expressions in switch statements are non-standard");

  type = TREE_TYPE (cond);
  if (low_value)
    {
      low_value = check_case_value (low_value);
      low_value = convert_and_check (type, low_value);
      if (low_value == error_mark_node)
	goto error_out;
    }
  if (high_value)
    {
      high_value = check_case_value (high_value);
      high_value = convert_and_check (type, high_value);
      if (high_value == error_mark_node)
	goto error_out;
    }

  if (low_value && high_value)
    {
      /* If the LOW_VALUE and HIGH_VALUE are the same, then this isn't
	 really a case range, even though it was written that way.
	 Remove the HIGH_VALUE to simplify later processing.  */
      if (tree_int_cst_equal (low_value, high_value))
	high_value = NULL_TREE;
      else if (!tree_int_cst_lt (low_value, high_value))
	warning_at (loc, 0, "empty range specified");
    }

  /* See if the case is in range of the type of the original testing
     expression.  If both low_value and high_value are out of range,
     don't insert the case label and return NULL_TREE.  */
  if (low_value
      && !check_case_bounds (type, orig_type,
			     &low_value, high_value ? &high_value : NULL))
    return NULL_TREE;

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
      tree duplicate = CASE_LABEL ((tree) node->value);

      if (high_value)
	{
	  error_at (loc, "duplicate (or overlapping) case value");
	  error_at (DECL_SOURCE_LOCATION (duplicate),
		    "this is the first entry overlapping that value");
	}
      else if (low_value)
	{
	  error_at (loc, "duplicate case value") ;
	  error_at (DECL_SOURCE_LOCATION (duplicate), "previously used here");
	}
      else
	{
	  error_at (loc, "multiple default labels in one switch");
	  error_at (DECL_SOURCE_LOCATION (duplicate),
		    "this is the first default label");
	}
      goto error_out;
    }

  /* Add a CASE_LABEL to the statement-tree.  */
  case_label = add_stmt (build_case_label (loc, low_value, high_value, label));
  /* Register this case label in the splay tree.  */
  splay_tree_insert (cases,
		     (splay_tree_key) low_value,
		     (splay_tree_value) case_label);

  return case_label;

 error_out:
  /* Add a label so that the back-end doesn't think that the beginning of
     the switch is unreachable.  Note that we do not add a case label, as
     that just leads to duplicates and thence to failure later on.  */
  if (!cases->root)
    {
      tree t = create_artificial_label (loc);
      add_stmt (build_stmt (loc, LABEL_EXPR, t));
    }
  return error_mark_node;
}

/* Subroutines of c_do_switch_warnings, called via splay_tree_foreach.
   Used to verify that case values match up with enumerator values.  */

static void
match_case_to_enum_1 (tree key, tree type, tree label)
{
  char buf[2 + 2*HOST_BITS_PER_WIDE_INT/4 + 1];

  /* ??? Not working too hard to print the double-word value.
     Should perhaps be done with %lwd in the diagnostic routines?  */
  if (TREE_INT_CST_HIGH (key) == 0)
    snprintf (buf, sizeof (buf), HOST_WIDE_INT_PRINT_UNSIGNED,
	      TREE_INT_CST_LOW (key));
  else if (!TYPE_UNSIGNED (type)
	   && TREE_INT_CST_HIGH (key) == -1
	   && TREE_INT_CST_LOW (key) != 0)
    snprintf (buf, sizeof (buf), "-" HOST_WIDE_INT_PRINT_UNSIGNED,
	      -TREE_INT_CST_LOW (key));
  else
    snprintf (buf, sizeof (buf), HOST_WIDE_INT_PRINT_DOUBLE_HEX,
	      (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (key),
	      (unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (key));

  if (TYPE_NAME (type) == 0)
    warning_at (DECL_SOURCE_LOCATION (CASE_LABEL (label)),
		warn_switch ? OPT_Wswitch : OPT_Wswitch_enum,
		"case value %qs not in enumerated type",
		buf);
  else
    warning_at (DECL_SOURCE_LOCATION (CASE_LABEL (label)),
		warn_switch ? OPT_Wswitch : OPT_Wswitch_enum,
		"case value %qs not in enumerated type %qT",
		buf, type);
}

/* Subroutine of c_do_switch_warnings, called via splay_tree_foreach.
   Used to verify that case values match up with enumerator values.  */

static int
match_case_to_enum (splay_tree_node node, void *data)
{
  tree label = (tree) node->value;
  tree type = (tree) data;

  /* Skip default case.  */
  if (!CASE_LOW (label))
    return 0;

  /* If CASE_LOW_SEEN is not set, that means CASE_LOW did not appear
     when we did our enum->case scan.  Reset our scratch bit after.  */
  if (!CASE_LOW_SEEN (label))
    match_case_to_enum_1 (CASE_LOW (label), type, label);
  else
    CASE_LOW_SEEN (label) = 0;

  /* If CASE_HIGH is non-null, we have a range.  If CASE_HIGH_SEEN is
     not set, that means that CASE_HIGH did not appear when we did our
     enum->case scan.  Reset our scratch bit after.  */
  if (CASE_HIGH (label))
    {
      if (!CASE_HIGH_SEEN (label))
	match_case_to_enum_1 (CASE_HIGH (label), type, label);
      else
	CASE_HIGH_SEEN (label) = 0;
    }

  return 0;
}

/* Handle -Wswitch*.  Called from the front end after parsing the
   switch construct.  */
/* ??? Should probably be somewhere generic, since other languages
   besides C and C++ would want this.  At the moment, however, C/C++
   are the only tree-ssa languages that support enumerations at all,
   so the point is moot.  */

void
c_do_switch_warnings (splay_tree cases, location_t switch_location,
		      tree type, tree cond)
{
  splay_tree_node default_node;
  splay_tree_node node;
  tree chain;

  if (!warn_switch && !warn_switch_enum && !warn_switch_default)
    return;

  default_node = splay_tree_lookup (cases, (splay_tree_key) NULL);
  if (!default_node)
    warning_at (switch_location, OPT_Wswitch_default,
		"switch missing default case");

  /* From here on, we only care about about enumerated types.  */
  if (!type || TREE_CODE (type) != ENUMERAL_TYPE)
    return;

  /* From here on, we only care about -Wswitch and -Wswitch-enum.  */
  if (!warn_switch_enum && !warn_switch)
    return;

  /* Check the cases.  Warn about case values which are not members of
     the enumerated type.  For -Wswitch-enum, or for -Wswitch when
     there is no default case, check that exactly all enumeration
     literals are covered by the cases.  */

  /* Clearing COND if it is not an integer constant simplifies
     the tests inside the loop below.  */
  if (TREE_CODE (cond) != INTEGER_CST)
    cond = NULL_TREE;

  /* The time complexity here is O(N*lg(N)) worst case, but for the
      common case of monotonically increasing enumerators, it is
      O(N), since the nature of the splay tree will keep the next
      element adjacent to the root at all times.  */

  for (chain = TYPE_VALUES (type); chain; chain = TREE_CHAIN (chain))
    {
      tree value = TREE_VALUE (chain);
      if (TREE_CODE (value) == CONST_DECL)
        value = DECL_INITIAL (value);
      node = splay_tree_lookup (cases, (splay_tree_key) value);
      if (node)
	{
	  /* Mark the CASE_LOW part of the case entry as seen.  */
	  tree label = (tree) node->value;
	  CASE_LOW_SEEN (label) = 1;
	  continue;
	}

      /* Even though there wasn't an exact match, there might be a
	 case range which includes the enumerator's value.  */
      node = splay_tree_predecessor (cases, (splay_tree_key) value);
      if (node && CASE_HIGH ((tree) node->value))
	{
	  tree label = (tree) node->value;
	  int cmp = tree_int_cst_compare (CASE_HIGH (label), value);
	  if (cmp >= 0)
	    {
	      /* If we match the upper bound exactly, mark the CASE_HIGH
		 part of the case entry as seen.  */
	      if (cmp == 0)
		CASE_HIGH_SEEN (label) = 1;
	      continue;
	    }
	}

      /* We've now determined that this enumerated literal isn't
	 handled by the case labels of the switch statement.  */

      /* If the switch expression is a constant, we only really care
	 about whether that constant is handled by the switch.  */
      if (cond && tree_int_cst_compare (cond, value))
	continue;

      /* If there is a default_node, the only relevant option is
	 Wswitch-enum.  Otherwise, if both are enabled then we prefer
	 to warn using -Wswitch because -Wswitch is enabled by -Wall
	 while -Wswitch-enum is explicit.  */
      warning_at (switch_location,
		  (default_node || !warn_switch
		   ? OPT_Wswitch_enum
		   : OPT_Wswitch),
		  "enumeration value %qE not handled in switch",
		  TREE_PURPOSE (chain));
    }

  /* Warn if there are case expressions that don't correspond to
     enumerators.  This can occur since C and C++ don't enforce
     type-checking of assignments to enumeration variables.

     The time complexity here is now always O(N) worst case, since
     we should have marked both the lower bound and upper bound of
     every disjoint case label, with CASE_LOW_SEEN and CASE_HIGH_SEEN
     above.  This scan also resets those fields.  */

  splay_tree_foreach (cases, match_case_to_enum, type);
}

/* Finish an expression taking the address of LABEL (an
   IDENTIFIER_NODE).  Returns an expression for the address.

   LOC is the location for the expression returned.  */

tree
finish_label_address_expr (tree label, location_t loc)
{
  tree result;

  pedwarn (input_location, OPT_pedantic, "taking the address of a label is non-standard");

  if (label == error_mark_node)
    return error_mark_node;

  label = lookup_label (label);
  if (label == NULL_TREE)
    result = null_pointer_node;
  else
    {
      TREE_USED (label) = 1;
      result = build1 (ADDR_EXPR, ptr_type_node, label);
      /* The current function in not necessarily uninlinable.
	 Computed gotos are incompatible with inlining, but the value
	 here could be used only in a diagnostic, for example.  */
      protected_set_expr_location (result, loc);
    }

  return result;
}


/* Given a boolean expression ARG, return a tree representing an increment
   or decrement (as indicated by CODE) of ARG.  The front end must check for
   invalid cases (e.g., decrement in C++).  */
tree
boolean_increment (enum tree_code code, tree arg)
{
  tree val;
  tree true_res = build_int_cst (TREE_TYPE (arg), 1);

  arg = stabilize_reference (arg);
  switch (code)
    {
    case PREINCREMENT_EXPR:
      val = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      break;
    case POSTINCREMENT_EXPR:
      val = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      arg = save_expr (arg);
      val = build2 (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build2 (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    case PREDECREMENT_EXPR:
      val = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg,
		    invert_truthvalue_loc (input_location, arg));
      break;
    case POSTDECREMENT_EXPR:
      val = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg,
		    invert_truthvalue_loc (input_location, arg));
      arg = save_expr (arg);
      val = build2 (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build2 (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    default:
      gcc_unreachable ();
    }
  TREE_SIDE_EFFECTS (val) = 1;
  return val;
}

/* Built-in macros for stddef.h and stdint.h, that require macros
   defined in this file.  */
void
c_stddef_cpp_builtins(void)
{
  builtin_define_with_value ("__SIZE_TYPE__", SIZE_TYPE, 0);
  builtin_define_with_value ("__PTRDIFF_TYPE__", PTRDIFF_TYPE, 0);
  builtin_define_with_value ("__WCHAR_TYPE__", MODIFIED_WCHAR_TYPE, 0);
  builtin_define_with_value ("__WINT_TYPE__", WINT_TYPE, 0);
  builtin_define_with_value ("__INTMAX_TYPE__", INTMAX_TYPE, 0);
  builtin_define_with_value ("__UINTMAX_TYPE__", UINTMAX_TYPE, 0);
  builtin_define_with_value ("__CHAR16_TYPE__", CHAR16_TYPE, 0);
  builtin_define_with_value ("__CHAR32_TYPE__", CHAR32_TYPE, 0);
  if (SIG_ATOMIC_TYPE)
    builtin_define_with_value ("__SIG_ATOMIC_TYPE__", SIG_ATOMIC_TYPE, 0);
  if (INT8_TYPE)
    builtin_define_with_value ("__INT8_TYPE__", INT8_TYPE, 0);
  if (INT16_TYPE)
    builtin_define_with_value ("__INT16_TYPE__", INT16_TYPE, 0);
  if (INT32_TYPE)
    builtin_define_with_value ("__INT32_TYPE__", INT32_TYPE, 0);
  if (INT64_TYPE)
    builtin_define_with_value ("__INT64_TYPE__", INT64_TYPE, 0);
  if (UINT8_TYPE)
    builtin_define_with_value ("__UINT8_TYPE__", UINT8_TYPE, 0);
  if (UINT16_TYPE)
    builtin_define_with_value ("__UINT16_TYPE__", UINT16_TYPE, 0);
  if (UINT32_TYPE)
    builtin_define_with_value ("__UINT32_TYPE__", UINT32_TYPE, 0);
  if (UINT64_TYPE)
    builtin_define_with_value ("__UINT64_TYPE__", UINT64_TYPE, 0);
  if (INT_LEAST8_TYPE)
    builtin_define_with_value ("__INT_LEAST8_TYPE__", INT_LEAST8_TYPE, 0);
  if (INT_LEAST16_TYPE)
    builtin_define_with_value ("__INT_LEAST16_TYPE__", INT_LEAST16_TYPE, 0);
  if (INT_LEAST32_TYPE)
    builtin_define_with_value ("__INT_LEAST32_TYPE__", INT_LEAST32_TYPE, 0);
  if (INT_LEAST64_TYPE)
    builtin_define_with_value ("__INT_LEAST64_TYPE__", INT_LEAST64_TYPE, 0);
  if (UINT_LEAST8_TYPE)
    builtin_define_with_value ("__UINT_LEAST8_TYPE__", UINT_LEAST8_TYPE, 0);
  if (UINT_LEAST16_TYPE)
    builtin_define_with_value ("__UINT_LEAST16_TYPE__", UINT_LEAST16_TYPE, 0);
  if (UINT_LEAST32_TYPE)
    builtin_define_with_value ("__UINT_LEAST32_TYPE__", UINT_LEAST32_TYPE, 0);
  if (UINT_LEAST64_TYPE)
    builtin_define_with_value ("__UINT_LEAST64_TYPE__", UINT_LEAST64_TYPE, 0);
  if (INT_FAST8_TYPE)
    builtin_define_with_value ("__INT_FAST8_TYPE__", INT_FAST8_TYPE, 0);
  if (INT_FAST16_TYPE)
    builtin_define_with_value ("__INT_FAST16_TYPE__", INT_FAST16_TYPE, 0);
  if (INT_FAST32_TYPE)
    builtin_define_with_value ("__INT_FAST32_TYPE__", INT_FAST32_TYPE, 0);
  if (INT_FAST64_TYPE)
    builtin_define_with_value ("__INT_FAST64_TYPE__", INT_FAST64_TYPE, 0);
  if (UINT_FAST8_TYPE)
    builtin_define_with_value ("__UINT_FAST8_TYPE__", UINT_FAST8_TYPE, 0);
  if (UINT_FAST16_TYPE)
    builtin_define_with_value ("__UINT_FAST16_TYPE__", UINT_FAST16_TYPE, 0);
  if (UINT_FAST32_TYPE)
    builtin_define_with_value ("__UINT_FAST32_TYPE__", UINT_FAST32_TYPE, 0);
  if (UINT_FAST64_TYPE)
    builtin_define_with_value ("__UINT_FAST64_TYPE__", UINT_FAST64_TYPE, 0);
  if (INTPTR_TYPE)
    builtin_define_with_value ("__INTPTR_TYPE__", INTPTR_TYPE, 0);
  if (UINTPTR_TYPE)
    builtin_define_with_value ("__UINTPTR_TYPE__", UINTPTR_TYPE, 0);
}

static void
c_init_attributes (void)
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)				\
  built_in_attributes[(int) ENUM] = NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)				\
  built_in_attributes[(int) ENUM] = build_int_cst (NULL_TREE, VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)				\
  built_in_attributes[(int) ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
  built_in_attributes[(int) ENUM]			\
    = tree_cons (built_in_attributes[(int) PURPOSE],	\
		 built_in_attributes[(int) VALUE],	\
		 built_in_attributes[(int) CHAIN]);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
}

/* Attribute handlers common to C front ends.  */

/* Handle a "packed" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_packed_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int flags, bool *no_add_attrs)
{
  if (TYPE_P (*node))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_variant_type_copy (*node);
      TYPE_PACKED (*node) = 1;
    }
  else if (TREE_CODE (*node) == FIELD_DECL)
    {
      if (TYPE_ALIGN (TREE_TYPE (*node)) <= BITS_PER_UNIT
	  /* Still pack bitfields.  */
	  && ! DECL_INITIAL (*node))
	warning (OPT_Wattributes,
		 "%qE attribute ignored for field of type %qT",
		 name, TREE_TYPE (*node));
      else
	DECL_PACKED (*node) = 1;
    }
  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
     used for DECL_REGISTER.  It wouldn't mean anything anyway.
     We can't set DECL_PACKED on the type of a TYPE_DECL, because
     that changes what the typedef is typing.  */
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nocommon" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nocommon_attribute (tree *node, tree name,
			   tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 0;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "common" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_common_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == VAR_DECL)
    DECL_COMMON (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
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
			     TYPE_READONLY (TREE_TYPE (type)), 1));
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "hot" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_hot_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		      int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      if (lookup_attribute ("cold", DECL_ATTRIBUTES (*node)) != NULL)
	{
	  warning (OPT_Wattributes, "%qE attribute conflicts with attribute %s",
		   name, "cold");
	  *no_add_attrs = true;
	}
      /* Most of the rest of the hot processing is done later with
	 lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}
/* Handle a "cold" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_cold_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      if (lookup_attribute ("hot", DECL_ATTRIBUTES (*node)) != NULL)
	{
	  warning (OPT_Wattributes, "%qE attribute conflicts with attribute %s",
		   name, "hot");
	  *no_add_attrs = true;
	}
      /* Most of the rest of the cold processing is done later with
	 lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noinline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noinline_attribute (tree *node, tree name,
			   tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_UNINLINABLE (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noclone" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noclone_attribute (tree *node, tree name,
			  tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "always_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_always_inline_attribute (tree *node, tree name,
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      /* Set the attribute and mark it for disregarding inline
	 limits.  */
      DECL_DISREGARD_INLINE_LIMITS (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "gnu_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_gnu_inline_attribute (tree *node, tree name,
			     tree ARG_UNUSED (args),
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (*node))
    {
      /* Do nothing else, just set the attribute.  We'll get at
	 it later with lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "artificial" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_artificial_attribute (tree *node, tree name,
			     tree ARG_UNUSED (args),
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (*node))
    {
      /* Do nothing else, just set the attribute.  We'll get at
	 it later with lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "flatten" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_flatten_attribute (tree *node, tree name,
			  tree args ATTRIBUTE_UNUSED,
			  int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "warning" or "error" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_error_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      || TREE_CODE (TREE_VALUE (args)) == STRING_CST)
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "used" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_used_attribute (tree *pnode, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree node = *pnode;

  if (TREE_CODE (node) == FUNCTION_DECL
      || (TREE_CODE (node) == VAR_DECL && TREE_STATIC (node)))
    {
      TREE_USED (node) = 1;
      DECL_PRESERVE_P (node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "unused" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_unused_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int flags, bool *no_add_attrs)
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
	  warning (OPT_Wattributes, "%qE attribute ignored", name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_variant_type_copy (*node);
      TREE_USED (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle a "externally_visible" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_externally_visible_attribute (tree *pnode, tree name,
				     tree ARG_UNUSED (args),
				     int ARG_UNUSED (flags),
				     bool *no_add_attrs)
{
  tree node = *pnode;

  if (TREE_CODE (node) == FUNCTION_DECL || TREE_CODE (node) == VAR_DECL)
    {
      if ((!TREE_STATIC (node) && TREE_CODE (node) != FUNCTION_DECL
	   && !DECL_EXTERNAL (node)) || !TREE_PUBLIC (node))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute have effect only on public objects", name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_const_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			int ARG_UNUSED (flags), bool *no_add_attrs)
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
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "transparent_union" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_transparent_union_attribute (tree *node, tree name,
				    tree ARG_UNUSED (args), int flags,
				    bool *no_add_attrs)
{
  tree type;

  *no_add_attrs = true;

  if (TREE_CODE (*node) == TYPE_DECL)
    node = &TREE_TYPE (*node);
  type = *node;

  if (TREE_CODE (type) == UNION_TYPE)
    {
      /* When IN_PLACE is set, leave the check for FIELDS and MODE to
	 the code in finish_struct.  */
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	{
	  if (TYPE_FIELDS (type) == NULL_TREE
	      || TYPE_MODE (type) != DECL_MODE (TYPE_FIELDS (type)))
	    goto ignored;

	  /* A type variant isn't good enough, since we don't a cast
	     to such a type removed as a no-op.  */
	  *node = type = build_duplicate_type (type);
	}

      TYPE_TRANSPARENT_UNION (type) = 1;
      return NULL_TREE;
    }

 ignored:
  warning (OPT_Wattributes, "%qE attribute ignored", name);
  return NULL_TREE;
}

/* Subroutine of handle_{con,de}structor_attribute.  Evaluate ARGS to
   get the requested priority for a constructor or destructor,
   possibly issuing diagnostics for invalid or reserved
   priorities.  */

static priority_type
get_priority (tree args, bool is_destructor)
{
  HOST_WIDE_INT pri;
  tree arg;

  if (!args)
    return DEFAULT_INIT_PRIORITY;
  
  if (!SUPPORTS_INIT_PRIORITY)
    {
      if (is_destructor)
	error ("destructor priorities are not supported");
      else
	error ("constructor priorities are not supported");
      return DEFAULT_INIT_PRIORITY;
    }

  arg = TREE_VALUE (args);
  if (!host_integerp (arg, /*pos=*/0)
      || !INTEGRAL_TYPE_P (TREE_TYPE (arg)))
    goto invalid;

  pri = tree_low_cst (TREE_VALUE (args), /*pos=*/0);
  if (pri < 0 || pri > MAX_INIT_PRIORITY)
    goto invalid;

  if (pri <= MAX_RESERVED_INIT_PRIORITY)
    {
      if (is_destructor)
	warning (0,
		 "destructor priorities from 0 to %d are reserved "
		 "for the implementation", 
		 MAX_RESERVED_INIT_PRIORITY);
      else
	warning (0,
		 "constructor priorities from 0 to %d are reserved "
		 "for the implementation", 
		 MAX_RESERVED_INIT_PRIORITY);
    }
  return pri;

 invalid:
  if (is_destructor)
    error ("destructor priorities must be integers from 0 to %d inclusive",
	   MAX_INIT_PRIORITY);
  else
    error ("constructor priorities must be integers from 0 to %d inclusive",
	   MAX_INIT_PRIORITY);
  return DEFAULT_INIT_PRIORITY;
}

/* Handle a "constructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_constructor_attribute (tree *node, tree name, tree args,
			      int ARG_UNUSED (flags),
			      bool *no_add_attrs)
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      priority_type priority;
      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      priority = get_priority (args, /*is_destructor=*/false);
      SET_DECL_INIT_PRIORITY (decl, priority);
      TREE_USED (decl) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "destructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_destructor_attribute (tree *node, tree name, tree args,
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      priority_type priority;
      DECL_STATIC_DESTRUCTOR (decl) = 1;
      priority = get_priority (args, /*is_destructor=*/true);
      SET_DECL_FINI_PRIORITY (decl, priority);
      TREE_USED (decl) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "mode" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_mode_attribute (tree *node, tree name, tree args,
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree type = *node;
  tree ident = TREE_VALUE (args);

  *no_add_attrs = true;

  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    {
      int j;
      const char *p = IDENTIFIER_POINTER (ident);
      int len = strlen (p);
      enum machine_mode mode = VOIDmode;
      tree typefm;
      bool valid_mode;

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
      if (!strcmp (p, "byte"))
	mode = byte_mode;
      else if (!strcmp (p, "word"))
	mode = word_mode;
      else if (!strcmp (p, "pointer"))
	mode = ptr_mode;
      else if (!strcmp (p, "libgcc_cmp_return"))
	mode = targetm.libgcc_cmp_return_mode ();
      else if (!strcmp (p, "libgcc_shift_count"))
	mode = targetm.libgcc_shift_count_mode ();
      else if (!strcmp (p, "unwind_word"))
	mode = targetm.unwind_word_mode ();
      else
	for (j = 0; j < NUM_MACHINE_MODES; j++)
	  if (!strcmp (p, GET_MODE_NAME (j)))
	    {
	      mode = (enum machine_mode) j;
	      break;
	    }

      if (mode == VOIDmode)
	{
	  error ("unknown machine mode %qE", ident);
	  return NULL_TREE;
	}

      valid_mode = false;
      switch (GET_MODE_CLASS (mode))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_FLOAT:
	case MODE_DECIMAL_FLOAT:
	case MODE_FRACT:
	case MODE_UFRACT:
	case MODE_ACCUM:
	case MODE_UACCUM:
	  valid_mode = targetm.scalar_mode_supported_p (mode);
	  break;

	case MODE_COMPLEX_INT:
	case MODE_COMPLEX_FLOAT:
	  valid_mode = targetm.scalar_mode_supported_p (GET_MODE_INNER (mode));
	  break;

	case MODE_VECTOR_INT:
	case MODE_VECTOR_FLOAT:
	case MODE_VECTOR_FRACT:
	case MODE_VECTOR_UFRACT:
	case MODE_VECTOR_ACCUM:
	case MODE_VECTOR_UACCUM:
	  warning (OPT_Wattributes, "specifying vector types with "
		   "__attribute__ ((mode)) is deprecated");
	  warning (OPT_Wattributes,
		   "use __attribute__ ((vector_size)) instead");
	  valid_mode = vector_mode_valid_p (mode);
	  break;

	default:
	  break;
	}
      if (!valid_mode)
	{
	  error ("unable to emulate %qs", p);
	  return NULL_TREE;
	}

      if (POINTER_TYPE_P (type))
	{
	  tree (*fn)(tree, enum machine_mode, bool);

	  if (!targetm.valid_pointer_mode (mode))
	    {
	      error ("invalid pointer mode %qs", p);
	      return NULL_TREE;
	    }

	  if (TREE_CODE (type) == POINTER_TYPE)
	    fn = build_pointer_type_for_mode;
	  else
	    fn = build_reference_type_for_mode;
	  typefm = fn (TREE_TYPE (type), mode, false);
	}
      else
	{
	  /* For fixed-point modes, we need to test if the signness of type
	     and the machine mode are consistent.  */
	  if (ALL_FIXED_POINT_MODE_P (mode)
	      && TYPE_UNSIGNED (type) != UNSIGNED_FIXED_POINT_MODE_P (mode))
	    {
	      error ("signness of type and machine mode %qs don't match", p);
	      return NULL_TREE;
	    }
	  /* For fixed-point modes, we need to pass saturating info.  */
	  typefm = lang_hooks.types.type_for_mode (mode,
			ALL_FIXED_POINT_MODE_P (mode) ? TYPE_SATURATING (type)
						      : TYPE_UNSIGNED (type));
	}

      if (typefm == NULL_TREE)
	{
	  error ("no data type for mode %qs", p);
	  return NULL_TREE;
	}
      else if (TREE_CODE (type) == ENUMERAL_TYPE)
	{
	  /* For enumeral types, copy the precision from the integer
	     type returned above.  If not an INTEGER_TYPE, we can't use
	     this mode for this type.  */
	  if (TREE_CODE (typefm) != INTEGER_TYPE)
	    {
	      error ("cannot use mode %qs for enumeral types", p);
	      return NULL_TREE;
	    }

	  if (flags & ATTR_FLAG_TYPE_IN_PLACE)
	    {
	      TYPE_PRECISION (type) = TYPE_PRECISION (typefm);
	      typefm = type;
	    }
	  else
	    {
	      /* We cannot build a type variant, as there's code that assumes
		 that TYPE_MAIN_VARIANT has the same mode.  This includes the
		 debug generators.  Instead, create a subrange type.  This
		 results in all of the enumeral values being emitted only once
		 in the original, and the subtype gets them by reference.  */
	      if (TYPE_UNSIGNED (type))
		typefm = make_unsigned_type (TYPE_PRECISION (typefm));
	      else
		typefm = make_signed_type (TYPE_PRECISION (typefm));
	      TREE_TYPE (typefm) = type;
	    }
	}
      else if (VECTOR_MODE_P (mode)
	       ? TREE_CODE (type) != TREE_CODE (TREE_TYPE (typefm))
	       : TREE_CODE (type) != TREE_CODE (typefm))
	{
	  error ("mode %qs applied to inappropriate type", p);
	  return NULL_TREE;
	}

      *node = typefm;
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_section_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (targetm.have_named_sections)
    {
      user_defined_section_attribute = true;

      if ((TREE_CODE (decl) == FUNCTION_DECL
	   || TREE_CODE (decl) == VAR_DECL)
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  if (TREE_CODE (decl) == VAR_DECL
	      && current_function_decl != NULL_TREE
	      && !TREE_STATIC (decl))
	    {
	      error_at (DECL_SOURCE_LOCATION (decl), 
			"section attribute cannot be specified for "
			"local variables");
	      *no_add_attrs = true;
	    }

	  /* The decl may have already been given a section attribute
	     from a previous declaration.  Ensure they match.  */
	  else if (DECL_SECTION_NAME (decl) != NULL_TREE
		   && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			      TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
	    {
	      error ("section of %q+D conflicts with previous declaration",
		     *node);
	      *no_add_attrs = true;
	    }
	  else if (TREE_CODE (decl) == VAR_DECL
		   && !targetm.have_tls && targetm.emutls.tmpl_section
		   && DECL_THREAD_LOCAL_P (decl))
	    {
	      error ("section of %q+D cannot be overridden", *node);
	      *no_add_attrs = true;
	    }
	  else
	    DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	}
      else
	{
	  error ("section attribute not allowed for %q+D", *node);
	  *no_add_attrs = true;
	}
    }
  else
    {
      error_at (DECL_SOURCE_LOCATION (*node),
		"section attributes are not supported for this target");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_aligned_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			  int flags, bool *no_add_attrs)
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  int is_type = 0;
  tree align_expr = (args ? TREE_VALUE (args)
		     : size_int (ATTRIBUTE_ALIGNED_VALUE / BITS_PER_UNIT));
  int i;

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = 1;

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
  else if (i >= HOST_BITS_PER_INT - BITS_PER_UNIT_LOG)
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
	  *type = build_variant_type_copy (*type);
	  DECL_ORIGINAL_TYPE (decl) = tt;
	  TYPE_NAME (*type) = decl;
	  TREE_USED (*type) = TREE_USED (decl);
	  TREE_TYPE (decl) = *type;
	}
      else if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*type = build_variant_type_copy (*type);

      TYPE_ALIGN (*type) = (1U << i) * BITS_PER_UNIT;
      TYPE_USER_ALIGN (*type) = 1;
    }
  else if (! VAR_OR_FUNCTION_DECL_P (decl)
	   && TREE_CODE (decl) != FIELD_DECL)
    {
      error ("alignment may not be specified for %q+D", decl);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && DECL_ALIGN (decl) > (1U << i) * BITS_PER_UNIT)
    {
      if (DECL_USER_ALIGN (decl))
	error ("alignment for %q+D was previously specified as %d "
	       "and may not be decreased", decl,
	       DECL_ALIGN (decl) / BITS_PER_UNIT);
      else
	error ("alignment for %q+D must be at least %d", decl,
	       DECL_ALIGN (decl) / BITS_PER_UNIT);
      *no_add_attrs = true;
    }
  else
    {
      DECL_ALIGN (decl) = (1U << i) * BITS_PER_UNIT;
      DECL_USER_ALIGN (decl) = 1;
    }

  return NULL_TREE;
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_weak_attribute (tree *node, tree name,
		       tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (*node))
    {
      error ("inline function %q+D cannot be declared weak", *node);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (*node) == FUNCTION_DECL
	   || TREE_CODE (*node) == VAR_DECL)
    declare_weak (*node);
  else
    warning (OPT_Wattributes, "%qE attribute ignored", name);

  return NULL_TREE;
}

/* Handle an "alias" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alias_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL && TREE_CODE (decl) != VAR_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
      || (TREE_CODE (decl) != FUNCTION_DECL 
	  && TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl))
      /* A static variable declaration is always a tentative definition,
	 but the alias is a non-tentative definition which overrides.  */
      || (TREE_CODE (decl) != FUNCTION_DECL 
	  && ! TREE_PUBLIC (decl) && DECL_INITIAL (decl)))
    {
      error ("%q+D defined both normally and as an alias", decl);
      *no_add_attrs = true;
    }

  /* Note that the very first time we process a nested declaration,
     decl_function_context will not be set.  Indeed, *would* never
     be set except for the DECL_INITIAL/DECL_EXTERNAL frobbery that
     we do below.  After such frobbery, pushdecl would set the context.
     In any case, this is never what we want.  */
  else if (decl_function_context (decl) == 0 && current_function_decl == NULL)
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("alias argument not a string");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      id = get_identifier (TREE_STRING_POINTER (id));
      /* This counts as a use of the object pointed to.  */
      TREE_USED (id) = 1;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	DECL_INITIAL (decl) = error_mark_node;
      else
	{
	  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
	    DECL_EXTERNAL (decl) = 1;
	  else
	    DECL_EXTERNAL (decl) = 0;
	  TREE_STATIC (decl) = 1;
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "weakref" attribute; arguments as in struct
   attribute_spec.handler.  */

static tree
handle_weakref_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			  int flags, bool *no_add_attrs)
{
  tree attr = NULL_TREE;

  /* We must ignore the attribute when it is associated with
     local-scoped decls, since attribute alias is ignored and many
     such symbols do not even have a DECL_WEAK field.  */
  if (decl_function_context (*node)
      || current_function_decl
      || (TREE_CODE (*node) != VAR_DECL && TREE_CODE (*node) != FUNCTION_DECL))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The idea here is that `weakref("name")' mutates into `weakref,
     alias("name")', and weakref without arguments, in turn,
     implicitly adds weak. */

  if (args)
    {
      attr = tree_cons (get_identifier ("alias"), args, attr);
      attr = tree_cons (get_identifier ("weakref"), NULL_TREE, attr);

      *no_add_attrs = true;

      decl_attributes (node, attr, flags);
    }
  else
    {
      if (lookup_attribute ("alias", DECL_ATTRIBUTES (*node)))
	error_at (DECL_SOURCE_LOCATION (*node),
		  "weakref attribute must appear before alias attribute");

      /* Can't call declare_weak because it wants this to be TREE_PUBLIC,
	 and that isn't supported; and because it wants to add it to
	 the list of weak decls, which isn't helpful.  */
      DECL_WEAK (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle an "visibility" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_visibility_attribute (tree *node, tree name, tree args,
			     int ARG_UNUSED (flags),
			     bool *ARG_UNUSED (no_add_attrs))
{
  tree decl = *node;
  tree id = TREE_VALUE (args);
  enum symbol_visibility vis;

  if (TYPE_P (*node))
    {
      if (TREE_CODE (*node) == ENUMERAL_TYPE)
	/* OK */;
      else if (TREE_CODE (*node) != RECORD_TYPE && TREE_CODE (*node) != UNION_TYPE)
	{
	  warning (OPT_Wattributes, "%qE attribute ignored on non-class types",
		   name);
	  return NULL_TREE;
	}
      else if (TYPE_FIELDS (*node))
	{
	  error ("%qE attribute ignored because %qT is already defined",
		 name, *node);
	  return NULL_TREE;
	}
    }
  else if (decl_function_context (decl) != 0 || !TREE_PUBLIC (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  if (TREE_CODE (id) != STRING_CST)
    {
      error ("visibility argument not a string");
      return NULL_TREE;
    }

  /*  If this is a type, set the visibility on the type decl.  */
  if (TYPE_P (decl))
    {
      decl = TYPE_NAME (decl);
      if (!decl)
	return NULL_TREE;
      if (TREE_CODE (decl) == IDENTIFIER_NODE)
	{
	   warning (OPT_Wattributes, "%qE attribute ignored on types",
		    name);
	   return NULL_TREE;
	}
    }

  if (strcmp (TREE_STRING_POINTER (id), "default") == 0)
    vis = VISIBILITY_DEFAULT;
  else if (strcmp (TREE_STRING_POINTER (id), "internal") == 0)
    vis = VISIBILITY_INTERNAL;
  else if (strcmp (TREE_STRING_POINTER (id), "hidden") == 0)
    vis = VISIBILITY_HIDDEN;
  else if (strcmp (TREE_STRING_POINTER (id), "protected") == 0)
    vis = VISIBILITY_PROTECTED;
  else
    {
      error ("visibility argument must be one of \"default\", \"hidden\", \"protected\" or \"internal\"");
      vis = VISIBILITY_DEFAULT;
    }

  if (DECL_VISIBILITY_SPECIFIED (decl)
      && vis != DECL_VISIBILITY (decl))
    {
      tree attributes = (TYPE_P (*node)
			 ? TYPE_ATTRIBUTES (*node)
			 : DECL_ATTRIBUTES (decl));
      if (lookup_attribute ("visibility", attributes))
	error ("%qD redeclared with different visibility", decl);
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllimport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "dllimport");
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllexport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "dllexport");
    }

  DECL_VISIBILITY (decl) = vis;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;

  /* Go ahead and attach the attribute to the node as well.  This is needed
     so we can determine whether we have VISIBILITY_DEFAULT because the
     visibility was not specified, or because it was explicitly overridden
     from the containing scope.  */

  return NULL_TREE;
}

/* Determine the ELF symbol visibility for DECL, which is either a
   variable or a function.  It is an error to use this function if a
   definition of DECL is not available in this translation unit.
   Returns true if the final visibility has been determined by this
   function; false if the caller is free to make additional
   modifications.  */

bool
c_determine_visibility (tree decl)
{
  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == FUNCTION_DECL);

  /* If the user explicitly specified the visibility with an
     attribute, honor that.  DECL_VISIBILITY will have been set during
     the processing of the attribute.  We check for an explicit
     attribute, rather than just checking DECL_VISIBILITY_SPECIFIED,
     to distinguish the use of an attribute from the use of a "#pragma
     GCC visibility push(...)"; in the latter case we still want other
     considerations to be able to overrule the #pragma.  */
  if (lookup_attribute ("visibility", DECL_ATTRIBUTES (decl))
      || (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && (lookup_attribute ("dllimport", DECL_ATTRIBUTES (decl))
	      || lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl)))))
    return true;

  /* Set default visibility to whatever the user supplied with
     visibility_specified depending on #pragma GCC visibility.  */
  if (!DECL_VISIBILITY_SPECIFIED (decl))
    {
      if (visibility_options.inpragma
	  || DECL_VISIBILITY (decl) != default_visibility)
	{
	  DECL_VISIBILITY (decl) = default_visibility;
	  DECL_VISIBILITY_SPECIFIED (decl) = visibility_options.inpragma;
	  /* If visibility changed and DECL already has DECL_RTL, ensure
	     symbol flags are updated.  */
	  if (((TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl))
	       || TREE_CODE (decl) == FUNCTION_DECL)
	      && DECL_RTL_SET_P (decl))
	    make_decl_rtl (decl);
	}
    }
  return false;
}

/* Handle an "tls_model" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_tls_model_attribute (tree *node, tree name, tree args,
			    int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree id;
  tree decl = *node;
  enum tls_model kind;

  *no_add_attrs = true;

  if (TREE_CODE (decl) != VAR_DECL || !DECL_THREAD_LOCAL_P (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  kind = DECL_TLS_MODEL (decl);
  id = TREE_VALUE (args);
  if (TREE_CODE (id) != STRING_CST)
    {
      error ("tls_model argument not a string");
      return NULL_TREE;
    }

  if (!strcmp (TREE_STRING_POINTER (id), "local-exec"))
    kind = TLS_MODEL_LOCAL_EXEC;
  else if (!strcmp (TREE_STRING_POINTER (id), "initial-exec"))
    kind = TLS_MODEL_INITIAL_EXEC;
  else if (!strcmp (TREE_STRING_POINTER (id), "local-dynamic"))
    kind = optimize ? TLS_MODEL_LOCAL_DYNAMIC : TLS_MODEL_GLOBAL_DYNAMIC;
  else if (!strcmp (TREE_STRING_POINTER (id), "global-dynamic"))
    kind = TLS_MODEL_GLOBAL_DYNAMIC;
  else
    error ("tls_model argument must be one of \"local-exec\", \"initial-exec\", \"local-dynamic\" or \"global-dynamic\"");

  DECL_TLS_MODEL (decl) = kind;
  return NULL_TREE;
}

/* Handle a "no_instrument_function" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_instrument_function_attribute (tree *node, tree name,
					 tree ARG_UNUSED (args),
					 int ARG_UNUSED (flags),
					 bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute applies only to functions", name);
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"can%'t set %qE attribute after definition", name);
      *no_add_attrs = true;
    }
  else
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;

  return NULL_TREE;
}

/* Handle a "malloc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_malloc_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (*node))))
    DECL_IS_MALLOC (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "alloc_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alloc_size_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			     int ARG_UNUSED (flags), bool *no_add_attrs)
{
  unsigned arg_count = type_num_arguments (*node);
  for (; args; args = TREE_CHAIN (args))
    {
      tree position = TREE_VALUE (args);

      if (TREE_CODE (position) != INTEGER_CST
	  || TREE_INT_CST_HIGH (position) 
	  || TREE_INT_CST_LOW (position) < 1
	  || TREE_INT_CST_LOW (position) > arg_count )
	{
	  warning (OPT_Wattributes, 
	           "alloc_size parameter outside range");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }
  return NULL_TREE;
}

/* Handle a "returns_twice" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_returns_twice_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_RETURNS_TWICE (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_limit_stack" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_limit_stack_attribute (tree *node, tree name,
				 tree ARG_UNUSED (args),
				 int ARG_UNUSED (flags),
				 bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
	     "%qE attribute applies only to functions", name);
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"can%'t set %qE attribute after definition", name);
      *no_add_attrs = true;
    }
  else
    DECL_NO_LIMIT_STACK (decl) = 1;

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_PURE_P (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no vops" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_novops_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool *ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  DECL_IS_NOVOPS (*node) = 1;
  return NULL_TREE;
}

/* Handle a "deprecated" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_deprecated_attribute (tree *node, tree name,
			     tree args, int flags,
			     bool *no_add_attrs)
{
  tree type = NULL_TREE;
  int warn = 0;
  tree what = NULL_TREE;

  if (!args)
    *no_add_attrs = true;
  else if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      error ("deprecated message is not a string");
      *no_add_attrs = true;
    }

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
	*node = build_variant_type_copy (*node);
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
	    what = TYPE_NAME (*node);
	  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (type)))
	    what = DECL_NAME (TYPE_NAME (type));
	}
      if (what)
	warning (OPT_Wattributes, "%qE attribute ignored for %qE", name, what);
      else
	warning (OPT_Wattributes, "%qE attribute ignored", name);
    }

  return NULL_TREE;
}

/* Handle a "vector_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_vector_size_attribute (tree *node, tree name, tree args,
			      int ARG_UNUSED (flags),
			      bool *no_add_attrs)
{
  unsigned HOST_WIDE_INT vecsize, nunits;
  enum machine_mode orig_mode;
  tree type = *node, new_type, size;

  *no_add_attrs = true;

  size = TREE_VALUE (args);

  if (!host_integerp (size, 1))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  /* Get the vector size (in bytes).  */
  vecsize = tree_low_cst (size, 1);

  /* We need to provide for vector pointers, vector arrays, and
     functions returning vectors.  For example:

       __attribute__((vector_size(16))) short *foo;

     In this case, the mode is SI, but the type being modified is
     HI, so we need to look further.  */

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE
	 || TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);

  /* Get the mode of the type being modified.  */
  orig_mode = TYPE_MODE (type);

  if ((!INTEGRAL_TYPE_P (type)
       && !SCALAR_FLOAT_TYPE_P (type)
       && !FIXED_POINT_TYPE_P (type))
      || (!SCALAR_FLOAT_MODE_P (orig_mode)
	  && GET_MODE_CLASS (orig_mode) != MODE_INT
	  && !ALL_SCALAR_FIXED_POINT_MODE_P (orig_mode))
      || !host_integerp (TYPE_SIZE_UNIT (type), 1)
      || TREE_CODE (type) == BOOLEAN_TYPE)
    {
      error ("invalid vector type for attribute %qE", name);
      return NULL_TREE;
    }

  if (vecsize % tree_low_cst (TYPE_SIZE_UNIT (type), 1))
    {
      error ("vector size not an integral multiple of component size");
      return NULL;
    }

  if (vecsize == 0)
    {
      error ("zero vector size");
      return NULL;
    }

  /* Calculate how many units fit in the vector.  */
  nunits = vecsize / tree_low_cst (TYPE_SIZE_UNIT (type), 1);
  if (nunits & (nunits - 1))
    {
      error ("number of components of the vector not a power of two");
      return NULL_TREE;
    }

  new_type = build_vector_type (type, nunits);

  /* Build back pointers if needed.  */
  *node = lang_hooks.types.reconstruct_complex_type (*node, new_type);

  return NULL_TREE;
}

/* Handle the "nonnull" attribute.  */
static tree
handle_nonnull_attribute (tree *node, tree ARG_UNUSED (name),
			  tree args, int ARG_UNUSED (flags),
			  bool *no_add_attrs)
{
  tree type = *node;
  unsigned HOST_WIDE_INT attr_arg_num;

  /* If no arguments are specified, all pointer arguments should be
     non-null.  Verify a full prototype is given so that the arguments
     will have the correct types when we actually check them later.  */
  if (!args)
    {
      if (!TYPE_ARG_TYPES (type))
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
      unsigned HOST_WIDE_INT arg_num = 0, ck_num;

      if (!get_nonnull_operand (TREE_VALUE (args), &arg_num))
	{
	  error ("nonnull argument has invalid operand number (argument %lu)",
		 (unsigned long) attr_arg_num);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      argument = TYPE_ARG_TYPES (type);
      if (argument)
	{
	  for (ck_num = 1; ; ck_num++)
	    {
	      if (!argument || ck_num == arg_num)
		break;
	      argument = TREE_CHAIN (argument);
	    }

	  if (!argument
	      || TREE_CODE (TREE_VALUE (argument)) == VOID_TYPE)
	    {
	      error ("nonnull argument with out-of-range operand number (argument %lu, operand %lu)",
		     (unsigned long) attr_arg_num, (unsigned long) arg_num);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }

	  if (TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE)
	    {
	      error ("nonnull argument references non-pointer operand (argument %lu, operand %lu)",
		   (unsigned long) attr_arg_num, (unsigned long) arg_num);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}
    }

  return NULL_TREE;
}

/* Check the argument list of a function call for null in argument slots
   that are marked as requiring a non-null pointer argument.  The NARGS
   arguments are passed in the array ARGARRAY.
*/

static void
check_function_nonnull (tree attrs, int nargs, tree *argarray)
{
  tree a, args;
  int i;

  for (a = attrs; a; a = TREE_CHAIN (a))
    {
      if (is_attribute_p ("nonnull", TREE_PURPOSE (a)))
	{
	  args = TREE_VALUE (a);

	  /* Walk the argument list.  If we encounter an argument number we
	     should check for non-null, do it.  If the attribute has no args,
	     then every pointer argument is checked (in which case the check
	     for pointer type is done in check_nonnull_arg).  */
	  for (i = 0; i < nargs; i++)
	    {
	      if (!args || nonnull_check_p (args, i + 1))
		check_function_arguments_recurse (check_nonnull_arg, NULL,
						  argarray[i],
						  i + 1);
	    }
	}
    }
}

/* Check that the Nth argument of a function call (counting backwards
   from the end) is a (pointer)0.  The NARGS arguments are passed in the
   array ARGARRAY.  */

static void
check_function_sentinel (tree attrs, int nargs, tree *argarray, tree typelist)
{
  tree attr = lookup_attribute ("sentinel", attrs);

  if (attr)
    {
      int len = 0;
      int pos = 0;
      tree sentinel;

      /* Skip over the named arguments.  */
      while (typelist && len < nargs)
	{
	  typelist = TREE_CHAIN (typelist);
	  len++;
	}

      if (TREE_VALUE (attr))
	{
	  tree p = TREE_VALUE (TREE_VALUE (attr));
	  pos = TREE_INT_CST_LOW (p);
	}

      /* The sentinel must be one of the varargs, i.e.
	 in position >= the number of fixed arguments.  */
      if ((nargs - 1 - pos) < len)
	{
	  warning (OPT_Wformat,
		   "not enough variable arguments to fit a sentinel");
	  return;
	}

      /* Validate the sentinel.  */
      sentinel = argarray[nargs - 1 - pos];
      if ((!POINTER_TYPE_P (TREE_TYPE (sentinel))
	   || !integer_zerop (sentinel))
	  /* Although __null (in C++) is only an integer we allow it
	     nevertheless, as we are guaranteed that it's exactly
	     as wide as a pointer, and we don't want to force
	     users to cast the NULL they have written there.
	     We warn with -Wstrict-null-sentinel, though.  */
	  && (warn_strict_null_sentinel || null_node != sentinel))
	warning (OPT_Wformat, "missing sentinel in function call");
    }
}

/* Helper for check_function_nonnull; given a list of operands which
   must be non-null in ARGS, determine if operand PARAM_NUM should be
   checked.  */

static bool
nonnull_check_p (tree args, unsigned HOST_WIDE_INT param_num)
{
  unsigned HOST_WIDE_INT arg_num = 0;

  for (; args; args = TREE_CHAIN (args))
    {
      bool found = get_nonnull_operand (TREE_VALUE (args), &arg_num);

      gcc_assert (found);

      if (arg_num == param_num)
	return true;
    }
  return false;
}

/* Check that the function argument PARAM (which is operand number
   PARAM_NUM) is non-null.  This is called by check_function_nonnull
   via check_function_arguments_recurse.  */

static void
check_nonnull_arg (void * ARG_UNUSED (ctx), tree param,
		   unsigned HOST_WIDE_INT param_num)
{
  /* Just skip checking the argument if it's not a pointer.  This can
     happen if the "nonnull" attribute was given without an operand
     list (which means to check every pointer argument).  */

  if (TREE_CODE (TREE_TYPE (param)) != POINTER_TYPE)
    return;

  if (integer_zerop (param))
    warning (OPT_Wnonnull, "null argument where non-null required "
	     "(argument %lu)", (unsigned long) param_num);
}

/* Helper for nonnull attribute handling; fetch the operand number
   from the attribute argument list.  */

static bool
get_nonnull_operand (tree arg_num_expr, unsigned HOST_WIDE_INT *valp)
{
  /* Verify the arg number is a constant.  */
  if (TREE_CODE (arg_num_expr) != INTEGER_CST
      || TREE_INT_CST_HIGH (arg_num_expr) != 0)
    return false;

  *valp = TREE_INT_CST_LOW (arg_num_expr);
  return true;
}

/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nothrow_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_NOTHROW (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "cleanup" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_cleanup_attribute (tree *node, tree name, tree args,
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;
  tree cleanup_id, cleanup_decl;

  /* ??? Could perhaps support cleanups on TREE_STATIC, much like we do
     for global destructors in C++.  This requires infrastructure that
     we don't have generically at the moment.  It's also not a feature
     we'd be missing too much, since we do have attribute constructor.  */
  if (TREE_CODE (decl) != VAR_DECL || TREE_STATIC (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Verify that the argument is a function in scope.  */
  /* ??? We could support pointers to functions here as well, if
     that was considered desirable.  */
  cleanup_id = TREE_VALUE (args);
  if (TREE_CODE (cleanup_id) != IDENTIFIER_NODE)
    {
      error ("cleanup argument not an identifier");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  cleanup_decl = lookup_name (cleanup_id);
  if (!cleanup_decl || TREE_CODE (cleanup_decl) != FUNCTION_DECL)
    {
      error ("cleanup argument not a function");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* That the function has proper type is checked with the
     eventual call to build_function_call.  */

  return NULL_TREE;
}

/* Handle a "warn_unused_result" attribute.  No special handling.  */

static tree
handle_warn_unused_result_attribute (tree *node, tree name,
			       tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  /* Ignore the attribute for functions not returning any value.  */
  if (VOID_TYPE_P (TREE_TYPE (*node)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "sentinel" attribute.  */

static tree
handle_sentinel_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree params = TYPE_ARG_TYPES (*node);

  if (!params)
    {
      warning (OPT_Wattributes,
	       "%qE attribute requires prototypes with named arguments", name);
      *no_add_attrs = true;
    }
  else
    {
      while (TREE_CHAIN (params))
	params = TREE_CHAIN (params);

      if (VOID_TYPE_P (TREE_VALUE (params)))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute only applies to variadic functions", name);
	  *no_add_attrs = true;
	}
    }

  if (args)
    {
      tree position = TREE_VALUE (args);

      if (TREE_CODE (position) != INTEGER_CST)
	{
	  warning (OPT_Wattributes, 
		   "requested position is not an integer constant");
	  *no_add_attrs = true;
	}
      else
	{
	  if (tree_int_cst_lt (position, integer_zero_node))
	    {
	      warning (OPT_Wattributes,
		       "requested position is less than zero");
	      *no_add_attrs = true;
	    }
	}
    }

  return NULL_TREE;
}

/* Handle a "type_generic" attribute.  */

static tree
handle_type_generic_attribute (tree *node, tree ARG_UNUSED (name),
			       tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			       bool * ARG_UNUSED (no_add_attrs))
{
  tree params;
  
  /* Ensure we have a function type.  */
  gcc_assert (TREE_CODE (*node) == FUNCTION_TYPE);
  
  params = TYPE_ARG_TYPES (*node);
  while (params && ! VOID_TYPE_P (TREE_VALUE (params)))
    params = TREE_CHAIN (params);

  /* Ensure we have a variadic function.  */
  gcc_assert (!params);

  return NULL_TREE;
}

/* Handle a "target" attribute.  */

static tree
handle_target_attribute (tree *node, tree name, tree args, int flags,
			 bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (! targetm.target_option.valid_attribute_p (*node, name, args,
						      flags))
    *no_add_attrs = true;

  return NULL_TREE;
}

/* Arguments being collected for optimization.  */
typedef const char *const_char_p;		/* For DEF_VEC_P.  */
DEF_VEC_P(const_char_p);
DEF_VEC_ALLOC_P(const_char_p, gc);
static GTY(()) VEC(const_char_p, gc) *optimize_args;


/* Inner function to convert a TREE_LIST to argv string to parse the optimize
   options in ARGS.  ATTR_P is true if this is for attribute(optimize), and
   false for #pragma GCC optimize.  */

bool
parse_optimize_options (tree args, bool attr_p)
{
  bool ret = true;
  unsigned opt_argc;
  unsigned i;
  int saved_flag_strict_aliasing;
  const char **opt_argv;
  tree ap;

  /* Build up argv vector.  Just in case the string is stored away, use garbage
     collected strings.  */
  VEC_truncate (const_char_p, optimize_args, 0);
  VEC_safe_push (const_char_p, gc, optimize_args, NULL);

  for (ap = args; ap != NULL_TREE; ap = TREE_CHAIN (ap))
    {
      tree value = TREE_VALUE (ap);

      if (TREE_CODE (value) == INTEGER_CST)
	{
	  char buffer[20];
	  sprintf (buffer, "-O%ld", (long) TREE_INT_CST_LOW (value));
	  VEC_safe_push (const_char_p, gc, optimize_args, ggc_strdup (buffer));
	}

      else if (TREE_CODE (value) == STRING_CST)
	{
	  /* Split string into multiple substrings.  */
	  size_t len = TREE_STRING_LENGTH (value);
	  char *p = ASTRDUP (TREE_STRING_POINTER (value));
	  char *end = p + len;
	  char *comma;
	  char *next_p = p;

	  while (next_p != NULL)
	    {
	      size_t len2;
	      char *q, *r;

	      p = next_p;
	      comma = strchr (p, ',');
	      if (comma)
		{
		  len2 = comma - p;
		  *comma = '\0';
		  next_p = comma+1;
		}
	      else
		{
		  len2 = end - p;
		  next_p = NULL;
		}

	      r = q = (char *) ggc_alloc (len2 + 3);

	      /* If the user supplied -Oxxx or -fxxx, only allow -Oxxx or -fxxx
		 options.  */
	      if (*p == '-' && p[1] != 'O' && p[1] != 'f')
		{
		  ret = false;
		  if (attr_p)
		    warning (OPT_Wattributes,
			     "Bad option %s to optimize attribute.", p);
		  else
		    warning (OPT_Wpragmas,
			     "Bad option %s to pragma attribute", p);
		  continue;
		}

	      if (*p != '-')
		{
		  *r++ = '-';

		  /* Assume that Ox is -Ox, a numeric value is -Ox, a s by
		     itself is -Os, and any other switch begins with a -f.  */
		  if ((*p >= '0' && *p <= '9')
		      || (p[0] == 's' && p[1] == '\0'))
		    *r++ = 'O';
		  else if (*p != 'O')
		    *r++ = 'f';
		}

	      memcpy (r, p, len2);
	      r[len2] = '\0';
	      VEC_safe_push (const_char_p, gc, optimize_args, q);
	    }

	}
    }

  opt_argc = VEC_length (const_char_p, optimize_args);
  opt_argv = (const char **) alloca (sizeof (char *) * (opt_argc + 1));

  for (i = 1; i < opt_argc; i++)
    opt_argv[i] = VEC_index (const_char_p, optimize_args, i);

  saved_flag_strict_aliasing = flag_strict_aliasing;

  /* Now parse the options.  */
  decode_options (opt_argc, opt_argv);

  /* Don't allow changing -fstrict-aliasing.  */
  flag_strict_aliasing = saved_flag_strict_aliasing;

  VEC_truncate (const_char_p, optimize_args, 0);
  return ret;
}

/* For handling "optimize" attribute. arguments as in
   struct attribute_spec.handler.  */

static tree
handle_optimize_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else
    {
      struct cl_optimization cur_opts;
      tree old_opts = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node);

      /* Save current options.  */
      cl_optimization_save (&cur_opts);

      /* If we previously had some optimization options, use them as the
	 default.  */
      if (old_opts)
	cl_optimization_restore (TREE_OPTIMIZATION (old_opts));

      /* Parse options, and update the vector.  */
      parse_optimize_options (args, true);
      DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node)
	= build_optimization_node ();

      /* Restore current options.  */
      cl_optimization_restore (&cur_opts);
    }

  return NULL_TREE;
}

/* Check for valid arguments being passed to a function.
   ATTRS is a list of attributes.  There are NARGS arguments in the array
   ARGARRAY.  TYPELIST is the list of argument types for the function.
 */
void
check_function_arguments (tree attrs, int nargs, tree *argarray, tree typelist)
{
  /* Check for null being passed in a pointer argument that must be
     non-null.  We also need to do this if format checking is enabled.  */

  if (warn_nonnull)
    check_function_nonnull (attrs, nargs, argarray);

  /* Check for errors in format strings.  */

  if (warn_format || warn_missing_format_attribute)
    check_function_format (attrs, nargs, argarray);

  if (warn_format)
    check_function_sentinel (attrs, nargs, argarray, typelist);
}

/* Generic argument checking recursion routine.  PARAM is the argument to
   be checked.  PARAM_NUM is the number of the argument.  CALLBACK is invoked
   once the argument is resolved.  CTX is context for the callback.  */
void
check_function_arguments_recurse (void (*callback)
				  (void *, tree, unsigned HOST_WIDE_INT),
				  void *ctx, tree param,
				  unsigned HOST_WIDE_INT param_num)
{
  if (CONVERT_EXPR_P (param)
      && (TYPE_PRECISION (TREE_TYPE (param))
	  == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (param, 0)))))
    {
      /* Strip coercion.  */
      check_function_arguments_recurse (callback, ctx,
					TREE_OPERAND (param, 0), param_num);
      return;
    }

  if (TREE_CODE (param) == CALL_EXPR)
    {
      tree type = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (param)));
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
	    tree inner_arg;
	    tree format_num_expr;
	    int format_num;
	    int i;
	    call_expr_arg_iterator iter;

	    /* Extract the argument number, which was previously checked
	       to be valid.  */
	    format_num_expr = TREE_VALUE (TREE_VALUE (attrs));

	    gcc_assert (TREE_CODE (format_num_expr) == INTEGER_CST
			&& !TREE_INT_CST_HIGH (format_num_expr));

	    format_num = TREE_INT_CST_LOW (format_num_expr);

	    for (inner_arg = first_call_expr_arg (param, &iter), i = 1;
		 inner_arg != 0;
		 inner_arg = next_call_expr_arg (&iter), i++)
	      if (i == format_num)
		{
		  check_function_arguments_recurse (callback, ctx,
						    inner_arg, param_num);
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

/* Checks the number of arguments NARGS against the required number
   REQUIRED and issues an error if there is a mismatch.  Returns true
   if the number of arguments is correct, otherwise false.  */

static bool
validate_nargs (tree fndecl, int nargs, int required)
{
  if (nargs < required)
    {
      error ("not enough arguments to function %qE", fndecl);
      return false;
    }
  else if (nargs > required)
    {
      error ("too many arguments to function %qE", fndecl);
      return false;
    }
  return true;
}

/* Verifies the NARGS arguments ARGS to the builtin function FNDECL.
   Returns false if there was an error, otherwise true.  */

bool
check_builtin_function_arguments (tree fndecl, int nargs, tree *args)
{
  if (!DECL_BUILT_IN (fndecl)
      || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL)
    return true;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_CONSTANT_P:
      return validate_nargs (fndecl, nargs, 1);

    case BUILT_IN_ISFINITE:
    case BUILT_IN_ISINF:
    case BUILT_IN_ISINF_SIGN:
    case BUILT_IN_ISNAN:
    case BUILT_IN_ISNORMAL:
      if (validate_nargs (fndecl, nargs, 1))
	{
	  if (TREE_CODE (TREE_TYPE (args[0])) != REAL_TYPE)
	    {
	      error ("non-floating-point argument in call to "
		     "function %qE", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_ISGREATER:
    case BUILT_IN_ISGREATEREQUAL:
    case BUILT_IN_ISLESS:
    case BUILT_IN_ISLESSEQUAL:
    case BUILT_IN_ISLESSGREATER:
    case BUILT_IN_ISUNORDERED:
      if (validate_nargs (fndecl, nargs, 2))
	{
	  enum tree_code code0, code1;
	  code0 = TREE_CODE (TREE_TYPE (args[0]));
	  code1 = TREE_CODE (TREE_TYPE (args[1]));
	  if (!((code0 == REAL_TYPE && code1 == REAL_TYPE)
		|| (code0 == REAL_TYPE && code1 == INTEGER_TYPE)
		|| (code0 == INTEGER_TYPE && code1 == REAL_TYPE)))
	    {
	      error ("non-floating-point arguments in call to "
		     "function %qE", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_FPCLASSIFY:
      if (validate_nargs (fndecl, nargs, 6))
	{
	  unsigned i;
	  
	  for (i=0; i<5; i++)
	    if (TREE_CODE (args[i]) != INTEGER_CST)
	      {
		error ("non-const integer argument %u in call to function %qE",
		       i+1, fndecl);
		return false;
	      }

	  if (TREE_CODE (TREE_TYPE (args[5])) != REAL_TYPE)
	    {
	      error ("non-floating-point argument in call to function %qE",
		     fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    default:
      return true;
    }
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

int
field_decl_cmp (const void *x_p, const void *y_p)
{
  const tree *const x = (const tree *const) x_p;
  const tree *const y = (const tree *const) y_p;

  if (DECL_NAME (*x) == DECL_NAME (*y))
    /* A nontype is "greater" than a type.  */
    return (TREE_CODE (*y) == TYPE_DECL) - (TREE_CODE (*x) == TYPE_DECL);
  if (DECL_NAME (*x) == NULL_TREE)
    return -1;
  if (DECL_NAME (*y) == NULL_TREE)
    return 1;
  if (DECL_NAME (*x) < DECL_NAME (*y))
    return -1;
  return 1;
}

static struct {
  gt_pointer_operator new_value;
  void *cookie;
} resort_data;

/* This routine compares two fields like field_decl_cmp but using the
pointer operator in resort_data.  */

static int
resort_field_decl_cmp (const void *x_p, const void *y_p)
{
  const tree *const x = (const tree *const) x_p;
  const tree *const y = (const tree *const) y_p;

  if (DECL_NAME (*x) == DECL_NAME (*y))
    /* A nontype is "greater" than a type.  */
    return (TREE_CODE (*y) == TYPE_DECL) - (TREE_CODE (*x) == TYPE_DECL);
  if (DECL_NAME (*x) == NULL_TREE)
    return -1;
  if (DECL_NAME (*y) == NULL_TREE)
    return 1;
  {
    tree d1 = DECL_NAME (*x);
    tree d2 = DECL_NAME (*y);
    resort_data.new_value (&d1, resort_data.cookie);
    resort_data.new_value (&d2, resort_data.cookie);
    if (d1 < d2)
      return -1;
  }
  return 1;
}

/* Resort DECL_SORTED_FIELDS because pointers have been reordered.  */

void
resort_sorted_fields (void *obj,
		      void * ARG_UNUSED (orig_obj),
		      gt_pointer_operator new_value,
		      void *cookie)
{
  struct sorted_fields_type *sf = (struct sorted_fields_type *) obj;
  resort_data.new_value = new_value;
  resort_data.cookie = cookie;
  qsort (&sf->elts[0], sf->len, sizeof (tree),
	 resort_field_decl_cmp);
}

/* Subroutine of c_parse_error.
   Return the result of concatenating LHS and RHS. RHS is really
   a string literal, its first character is indicated by RHS_START and
   RHS_SIZE is its length (including the terminating NUL character).

   The caller is responsible for deleting the returned pointer.  */

static char *
catenate_strings (const char *lhs, const char *rhs_start, int rhs_size)
{
  const int lhs_size = strlen (lhs);
  char *result = XNEWVEC (char, lhs_size + rhs_size);
  strncpy (result, lhs, lhs_size);
  strncpy (result + lhs_size, rhs_start, rhs_size);
  return result;
}

/* Issue the error given by GMSGID, indicating that it occurred before
   TOKEN, which had the associated VALUE.  */

void
c_parse_error (const char *gmsgid, enum cpp_ttype token_type, 
	       tree value, unsigned char token_flags)
{
#define catenate_messages(M1, M2) catenate_strings ((M1), (M2), sizeof (M2))

  char *message = NULL;

  if (token_type == CPP_EOF)
    message = catenate_messages (gmsgid, " at end of input");
  else if (token_type == CPP_CHAR 
	   || token_type == CPP_WCHAR 
	   || token_type == CPP_CHAR16
	   || token_type == CPP_CHAR32)
    {
      unsigned int val = TREE_INT_CST_LOW (value);
      const char *prefix;

      switch (token_type)
	{
	default:
	  prefix = "";
	  break;
	case CPP_WCHAR:
	  prefix = "L";
	  break;
	case CPP_CHAR16:
	  prefix = "u";
	  break;
	case CPP_CHAR32:
	  prefix = "U";
	  break;
        }

      if (val <= UCHAR_MAX && ISGRAPH (val))
	message = catenate_messages (gmsgid, " before %s'%c'");
      else
	message = catenate_messages (gmsgid, " before %s'\\x%x'");

      error (message, prefix, val);
      free (message);
      message = NULL;
    }
  else if (token_type == CPP_STRING 
	   || token_type == CPP_WSTRING 
	   || token_type == CPP_STRING16
	   || token_type == CPP_STRING32)
    message = catenate_messages (gmsgid, " before string constant");
  else if (token_type == CPP_NUMBER)
    message = catenate_messages (gmsgid, " before numeric constant");
  else if (token_type == CPP_NAME)
    {
      message = catenate_messages (gmsgid, " before %qE");
      error (message, value);
      free (message);
      message = NULL;
    }
  else if (token_type == CPP_PRAGMA)
    message = catenate_messages (gmsgid, " before %<#pragma%>");
  else if (token_type == CPP_PRAGMA_EOL)
    message = catenate_messages (gmsgid, " before end of line");
  else if (token_type < N_TTYPES)
    {
      message = catenate_messages (gmsgid, " before %qs token");
      error (message, cpp_type2name (token_type, token_flags));
      free (message);
      message = NULL;
    }
  else
    error (gmsgid);

  if (message)
    {
      error (message);
      free (message);
    }
#undef catenate_messages
}

/* Callback from cpp_error for PFILE to print diagnostics from the
   preprocessor.  The diagnostic is of type LEVEL, at location
   LOCATION unless this is after lexing and the compiler's location
   should be used instead, with column number possibly overridden by
   COLUMN_OVERRIDE if not zero; MSG is the translated message and AP
   the arguments.  Returns true if a diagnostic was emitted, false
   otherwise.  */

bool
c_cpp_error (cpp_reader *pfile ATTRIBUTE_UNUSED, int level,
	     location_t location, unsigned int column_override,
	     const char *msg, va_list *ap)
{
  diagnostic_info diagnostic;
  diagnostic_t dlevel;
  int save_warn_system_headers = warn_system_headers;
  bool ret;

  switch (level)
    {
    case CPP_DL_WARNING_SYSHDR:
      if (flag_no_output)
	return false;
      warn_system_headers = 1;
      /* Fall through.  */
    case CPP_DL_WARNING:
      if (flag_no_output)
	return false;
      dlevel = DK_WARNING;
      break;
    case CPP_DL_PEDWARN:
      if (flag_no_output && !flag_pedantic_errors)
	return false;
      dlevel = DK_PEDWARN;
      break;
    case CPP_DL_ERROR:
      dlevel = DK_ERROR;
      break;
    case CPP_DL_ICE:
      dlevel = DK_ICE;
      break;
    case CPP_DL_NOTE:
      dlevel = DK_NOTE;
      break;
    case CPP_DL_FATAL:
      dlevel = DK_FATAL;
      break;
    default:
      gcc_unreachable ();
    }
  if (done_lexing)
    location = input_location;
  diagnostic_set_info_translated (&diagnostic, msg, ap,
				  location, dlevel);
  if (column_override)
    diagnostic_override_column (&diagnostic, column_override);
  ret = report_diagnostic (&diagnostic);
  if (level == CPP_DL_WARNING_SYSHDR)
    warn_system_headers = save_warn_system_headers;
  return ret;
}

/* Convert a character from the host to the target execution character
   set.  cpplib handles this, mostly.  */

HOST_WIDE_INT
c_common_to_target_charset (HOST_WIDE_INT c)
{
  /* Character constants in GCC proper are sign-extended under -fsigned-char,
     zero-extended under -fno-signed-char.  cpplib insists that characters
     and character constants are always unsigned.  Hence we must convert
     back and forth.  */
  cppchar_t uc = ((cppchar_t)c) & ((((cppchar_t)1) << CHAR_BIT)-1);

  uc = cpp_host_to_exec_charset (parse_in, uc);

  if (flag_signed_char)
    return ((HOST_WIDE_INT)uc) << (HOST_BITS_PER_WIDE_INT - CHAR_TYPE_SIZE)
			       >> (HOST_BITS_PER_WIDE_INT - CHAR_TYPE_SIZE);
  else
    return uc;
}

/* Build the result of __builtin_offsetof.  EXPR is a nested sequence of
   component references, with STOP_REF, or alternatively an INDIRECT_REF of
   NULL, at the bottom; much like the traditional rendering of offsetof as a
   macro.  Returns the folded and properly cast result.  */

static tree
fold_offsetof_1 (tree expr, tree stop_ref)
{
  enum tree_code code = PLUS_EXPR;
  tree base, off, t;

  if (expr == stop_ref && TREE_CODE (expr) != ERROR_MARK)
    return size_zero_node;

  switch (TREE_CODE (expr))
    {
    case ERROR_MARK:
      return expr;

    case VAR_DECL:
      error ("cannot apply %<offsetof%> to static data member %qD", expr);
      return error_mark_node;

    case CALL_EXPR:
    case TARGET_EXPR:
      error ("cannot apply %<offsetof%> when %<operator[]%> is overloaded");
      return error_mark_node;

    case INTEGER_CST:
      gcc_assert (integer_zerop (expr));
      return size_zero_node;

    case NOP_EXPR:
    case INDIRECT_REF:
      base = fold_offsetof_1 (TREE_OPERAND (expr, 0), stop_ref);
      gcc_assert (base == error_mark_node || base == size_zero_node);
      return base;

    case COMPONENT_REF:
      base = fold_offsetof_1 (TREE_OPERAND (expr, 0), stop_ref);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      if (DECL_C_BIT_FIELD (t))
	{
	  error ("attempt to take address of bit-field structure "
		 "member %qD", t);
	  return error_mark_node;
	}
      off = size_binop_loc (input_location, PLUS_EXPR, DECL_FIELD_OFFSET (t),
			    size_int (tree_low_cst (DECL_FIELD_BIT_OFFSET (t),
						    1)
				      / BITS_PER_UNIT));
      break;

    case ARRAY_REF:
      base = fold_offsetof_1 (TREE_OPERAND (expr, 0), stop_ref);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) < 0)
	{
	  code = MINUS_EXPR;
	  t = fold_build1_loc (input_location, NEGATE_EXPR, TREE_TYPE (t), t);
	}
      t = convert (sizetype, t);
      off = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (TREE_TYPE (expr)), t);
      break;

    case COMPOUND_EXPR:
      /* Handle static members of volatile structs.  */
      t = TREE_OPERAND (expr, 1);
      gcc_assert (TREE_CODE (t) == VAR_DECL);
      return fold_offsetof_1 (t, stop_ref);

    default:
      gcc_unreachable ();
    }

  return size_binop (code, base, off);
}

tree
fold_offsetof (tree expr, tree stop_ref)
{
  /* Convert back from the internal sizetype to size_t.  */
  return convert (size_type_node, fold_offsetof_1 (expr, stop_ref));
}

/* Print an error message for an invalid lvalue.  USE says
   how the lvalue is being used and so selects the error message.  */

void
lvalue_error (enum lvalue_use use)
{
  switch (use)
    {
    case lv_assign:
      error ("lvalue required as left operand of assignment");
      break;
    case lv_increment:
      error ("lvalue required as increment operand");
      break;
    case lv_decrement:
      error ("lvalue required as decrement operand");
      break;
    case lv_addressof:
      error ("lvalue required as unary %<&%> operand");
      break;
    case lv_asm:
      error ("lvalue required in asm statement");
      break;
    default:
      gcc_unreachable ();
    }
}

/* *PTYPE is an incomplete array.  Complete it with a domain based on
   INITIAL_VALUE.  If INITIAL_VALUE is not present, use 1 if DO_DEFAULT
   is true.  Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if INITIAL_VALUE was NULL, and 3 if INITIAL_VALUE was empty.  */

int
complete_array_type (tree *ptype, tree initial_value, bool do_default)
{
  tree maxindex, type, main_type, elt, unqual_elt;
  int failure = 0, quals;
  hashval_t hashcode = 0;

  maxindex = size_zero_node;
  if (initial_value)
    {
      if (TREE_CODE (initial_value) == STRING_CST)
	{
	  int eltsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (initial_value)));
	  maxindex = size_int (TREE_STRING_LENGTH (initial_value)/eltsize - 1);
	}
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  VEC(constructor_elt,gc) *v = CONSTRUCTOR_ELTS (initial_value);

	  if (VEC_empty (constructor_elt, v))
	    {
	      if (pedantic)
		failure = 3;
	      maxindex = integer_minus_one_node;
	    }
	  else
	    {
	      tree curindex;
	      unsigned HOST_WIDE_INT cnt;
	      constructor_elt *ce;
	      bool fold_p = false;

	      if (VEC_index (constructor_elt, v, 0)->index)
		maxindex = fold_convert_loc (input_location, sizetype,
					     VEC_index (constructor_elt,
							v, 0)->index);
	      curindex = maxindex;

	      for (cnt = 1;
		   VEC_iterate (constructor_elt, v, cnt, ce);
		   cnt++)
		{
		  bool curfold_p = false;
		  if (ce->index)
		    curindex = ce->index, curfold_p = true;
		  else
		    {
		      if (fold_p)
		        curindex = fold_convert (sizetype, curindex);
		      curindex = size_binop (PLUS_EXPR, curindex,
					     size_one_node);
		    }
		  if (tree_int_cst_lt (maxindex, curindex))
		    maxindex = curindex, fold_p = curfold_p;
		}
	       if (fold_p)
	         maxindex = fold_convert (sizetype, maxindex);
	    }
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    failure = 1;
	}
    }
  else
    {
      failure = 2;
      if (!do_default)
	return failure;
    }

  type = *ptype;
  elt = TREE_TYPE (type);
  quals = TYPE_QUALS (strip_array_types (elt));
  if (quals == 0)
    unqual_elt = elt;
  else
    unqual_elt = c_build_qualified_type (elt, TYPE_UNQUALIFIED);

  /* Using build_distinct_type_copy and modifying things afterward instead
     of using build_array_type to create a new type preserves all of the
     TYPE_LANG_FLAG_? bits that the front end may have set.  */
  main_type = build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
  TREE_TYPE (main_type) = unqual_elt;
  TYPE_DOMAIN (main_type) = build_index_type (maxindex);
  layout_type (main_type);

  /* Make sure we have the canonical MAIN_TYPE. */
  hashcode = iterative_hash_object (TYPE_HASH (unqual_elt), hashcode);
  hashcode = iterative_hash_object (TYPE_HASH (TYPE_DOMAIN (main_type)), 
				    hashcode);
  main_type = type_hash_canon (hashcode, main_type);

  /* Fix the canonical type.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (main_type))
      || TYPE_STRUCTURAL_EQUALITY_P (TYPE_DOMAIN (main_type)))
    SET_TYPE_STRUCTURAL_EQUALITY (main_type);
  else if (TYPE_CANONICAL (TREE_TYPE (main_type)) != TREE_TYPE (main_type)
	   || (TYPE_CANONICAL (TYPE_DOMAIN (main_type))
	       != TYPE_DOMAIN (main_type)))
    TYPE_CANONICAL (main_type) 
      = build_array_type (TYPE_CANONICAL (TREE_TYPE (main_type)),
			  TYPE_CANONICAL (TYPE_DOMAIN (main_type)));
  else
    TYPE_CANONICAL (main_type) = main_type;

  if (quals == 0)
    type = main_type;
  else
    type = c_build_qualified_type (main_type, quals);

  if (COMPLETE_TYPE_P (type)
      && TREE_CODE (TYPE_SIZE_UNIT (type)) == INTEGER_CST
      && TREE_OVERFLOW (TYPE_SIZE_UNIT (type)))
    {
      error ("size of array is too large");
      /* If we proceed with the array type as it is, we'll eventually
	 crash in tree_low_cst().  */
      type = error_mark_node;
    }

  *ptype = type;
  return failure;
}


/* Used to help initialize the builtin-types.def table.  When a type of
   the correct size doesn't exist, use error_mark_node instead of NULL.
   The later results in segfaults even when a decl using the type doesn't
   get invoked.  */

tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = lang_hooks.types.type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* A helper function for resolve_overloaded_builtin in resolving the
   overloaded __sync_ builtins.  Returns a positive power of 2 if the
   first operand of PARAMS is a pointer to a supported data type.
   Returns 0 if an error is encountered.  */

static int
sync_resolve_size (tree function, VEC(tree,gc) *params)
{
  tree type;
  int size;

  if (VEC_empty (tree, params))
    {
      error ("too few arguments to function %qE", function);
      return 0;
    }

  type = TREE_TYPE (VEC_index (tree, params, 0));
  if (TREE_CODE (type) != POINTER_TYPE)
    goto incompatible;

  type = TREE_TYPE (type);
  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    goto incompatible;

  size = tree_low_cst (TYPE_SIZE_UNIT (type), 1);
  if (size == 1 || size == 2 || size == 4 || size == 8 || size == 16)
    return size;

 incompatible:
  error ("incompatible type for argument %d of %qE", 1, function);
  return 0;
}

/* A helper function for resolve_overloaded_builtin.  Adds casts to
   PARAMS to make arguments match up with those of FUNCTION.  Drops
   the variadic arguments at the end.  Returns false if some error
   was encountered; true on success.  */

static bool
sync_resolve_params (tree orig_function, tree function, VEC(tree, gc) *params)
{
  tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (function));
  tree ptype;
  int number;
  unsigned int parmnum;

  /* We've declared the implementation functions to use "volatile void *"
     as the pointer parameter, so we shouldn't get any complaints from the
     call to check_function_arguments what ever type the user used.  */
  arg_types = TREE_CHAIN (arg_types);
  ptype = TREE_TYPE (TREE_TYPE (VEC_index (tree, params, 0)));
  number = 2;

  /* For the rest of the values, we need to cast these to FTYPE, so that we
     don't get warnings for passing pointer types, etc.  */
  parmnum = 0;
  while (arg_types != void_list_node)
    {
      tree val;

      ++parmnum;
      if (VEC_length (tree, params) <= parmnum)
	{
	  error ("too few arguments to function %qE", orig_function);
	  return false;
	}

      /* ??? Ideally for the first conversion we'd use convert_for_assignment
	 so that we get warnings for anything that doesn't match the pointer
	 type.  This isn't portable across the C and C++ front ends atm.  */
      val = VEC_index (tree, params, parmnum);
      val = convert (ptype, val);
      val = convert (TREE_VALUE (arg_types), val);
      VEC_replace (tree, params, parmnum, val);

      arg_types = TREE_CHAIN (arg_types);
      number++;
    }

  /* The definition of these primitives is variadic, with the remaining
     being "an optional list of variables protected by the memory barrier".
     No clue what that's supposed to mean, precisely, but we consider all
     call-clobbered variables to be protected so we're safe.  */
  VEC_truncate (tree, params, parmnum + 1);

  return true;
}

/* A helper function for resolve_overloaded_builtin.  Adds a cast to
   RESULT to make it match the type of the first pointer argument in
   PARAMS.  */

static tree
sync_resolve_return (tree first_param, tree result)
{
  tree ptype = TREE_TYPE (TREE_TYPE (first_param));
  ptype = TYPE_MAIN_VARIANT (ptype);
  return convert (ptype, result);
}

/* Some builtin functions are placeholders for other expressions.  This
   function should be called immediately after parsing the call expression
   before surrounding code has committed to the type of the expression.

   LOC is the location of the builtin call.

   FUNCTION is the DECL that has been invoked; it is known to be a builtin.
   PARAMS is the argument list for the call.  The return value is non-null
   when expansion is complete, and null if normal processing should
   continue.  */

tree
resolve_overloaded_builtin (location_t loc, tree function, VEC(tree,gc) *params)
{
  enum built_in_function orig_code = DECL_FUNCTION_CODE (function);
  switch (DECL_BUILT_IN_CLASS (function))
    {
    case BUILT_IN_NORMAL:
      break;
    case BUILT_IN_MD:
      if (targetm.resolve_overloaded_builtin)
	return targetm.resolve_overloaded_builtin (loc, function, params);
      else
	return NULL_TREE;
    default:
      return NULL_TREE;
    }

  /* Handle BUILT_IN_NORMAL here.  */
  switch (orig_code)
    {
    case BUILT_IN_FETCH_AND_ADD_N:
    case BUILT_IN_FETCH_AND_SUB_N:
    case BUILT_IN_FETCH_AND_OR_N:
    case BUILT_IN_FETCH_AND_AND_N:
    case BUILT_IN_FETCH_AND_XOR_N:
    case BUILT_IN_FETCH_AND_NAND_N:
    case BUILT_IN_ADD_AND_FETCH_N:
    case BUILT_IN_SUB_AND_FETCH_N:
    case BUILT_IN_OR_AND_FETCH_N:
    case BUILT_IN_AND_AND_FETCH_N:
    case BUILT_IN_XOR_AND_FETCH_N:
    case BUILT_IN_NAND_AND_FETCH_N:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_N:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N:
    case BUILT_IN_LOCK_TEST_AND_SET_N:
    case BUILT_IN_LOCK_RELEASE_N:
      {
	int n = sync_resolve_size (function, params);
	tree new_function, first_param, result;

	if (n == 0)
	  return error_mark_node;

	new_function = built_in_decls[orig_code + exact_log2 (n) + 1];
	if (!sync_resolve_params (function, new_function, params))
	  return error_mark_node;

	first_param = VEC_index (tree, params, 0);
	result = build_function_call_vec (loc, new_function, params, NULL);
	if (orig_code != BUILT_IN_BOOL_COMPARE_AND_SWAP_N
	    && orig_code != BUILT_IN_LOCK_RELEASE_N)
	  result = sync_resolve_return (first_param, result);

	return result;
      }

    default:
      return NULL_TREE;
    }
}

/* Ignoring their sign, return true if two scalar types are the same.  */
bool
same_scalar_type_ignoring_signedness (tree t1, tree t2)
{
  enum tree_code c1 = TREE_CODE (t1), c2 = TREE_CODE (t2);

  gcc_assert ((c1 == INTEGER_TYPE || c1 == REAL_TYPE || c1 == FIXED_POINT_TYPE)
	      && (c2 == INTEGER_TYPE || c2 == REAL_TYPE
		  || c2 == FIXED_POINT_TYPE));

  /* Equality works here because c_common_signed_type uses
     TYPE_MAIN_VARIANT.  */
  return c_common_signed_type (t1)
    == c_common_signed_type (t2);
}

/* Check for missing format attributes on function pointers.  LTYPE is
   the new type or left-hand side type.  RTYPE is the old type or
   right-hand side type.  Returns TRUE if LTYPE is missing the desired
   attribute.  */

bool
check_missing_format_attribute (tree ltype, tree rtype)
{
  tree const ttr = TREE_TYPE (rtype), ttl = TREE_TYPE (ltype);
  tree ra;

  for (ra = TYPE_ATTRIBUTES (ttr); ra; ra = TREE_CHAIN (ra))
    if (is_attribute_p ("format", TREE_PURPOSE (ra)))
      break;
  if (ra)
    {
      tree la;
      for (la = TYPE_ATTRIBUTES (ttl); la; la = TREE_CHAIN (la))
	if (is_attribute_p ("format", TREE_PURPOSE (la)))
	  break;
      return !la;
    }
  else
    return false;
}

/* Subscripting with type char is likely to lose on a machine where
   chars are signed.  So warn on any machine, but optionally.  Don't
   warn for unsigned char since that type is safe.  Don't warn for
   signed char because anyone who uses that must have done so
   deliberately. Furthermore, we reduce the false positive load by
   warning only for non-constant value of type char.  */

void
warn_array_subscript_with_type_char (tree index)
{
  if (TYPE_MAIN_VARIANT (TREE_TYPE (index)) == char_type_node
      && TREE_CODE (index) != INTEGER_CST)
    warning (OPT_Wchar_subscripts, "array subscript has type %<char%>");
}

/* Implement -Wparentheses for the unexpected C precedence rules, to
   cover cases like x + y << z which readers are likely to
   misinterpret.  We have seen an expression in which CODE is a binary
   operator used to combine expressions ARG_LEFT and ARG_RIGHT, which
   before folding had CODE_LEFT and CODE_RIGHT.  CODE_LEFT and
   CODE_RIGHT may be ERROR_MARK, which means that that side of the
   expression was not formed using a binary or unary operator, or it
   was enclosed in parentheses.  */

void
warn_about_parentheses (enum tree_code code,
			enum tree_code code_left, tree arg_left,
			enum tree_code code_right, tree arg_right)
{
  if (!warn_parentheses)
    return;

  /* This macro tests that the expression ARG with original tree code
     CODE appears to be a boolean expression. or the result of folding a
     boolean expression.  */
#define APPEARS_TO_BE_BOOLEAN_EXPR_P(CODE, ARG)                             \
	(truth_value_p (TREE_CODE (ARG))                                    \
	 || TREE_CODE (TREE_TYPE (ARG)) == BOOLEAN_TYPE                     \
	 /* Folding may create 0 or 1 integers from other expressions.  */  \
	 || ((CODE) != INTEGER_CST                                          \
	     && (integer_onep (ARG) || integer_zerop (ARG))))

  switch (code) 
    {
    case LSHIFT_EXPR:
      if (code_left == PLUS_EXPR || code_right == PLUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<+%> inside %<<<%>");
      else if (code_left == MINUS_EXPR || code_right == MINUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<-%> inside %<<<%>");
      return;

    case RSHIFT_EXPR:
      if (code_left == PLUS_EXPR || code_right == PLUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<+%> inside %<>>%>");
      else if (code_left == MINUS_EXPR || code_right == MINUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<-%> inside %<>>%>");
      return;

    case TRUTH_ORIF_EXPR:
      if (code_left == TRUTH_ANDIF_EXPR || code_right == TRUTH_ANDIF_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<&&%> within %<||%>");
      return;

    case BIT_IOR_EXPR:
      if (code_left == BIT_AND_EXPR || code_left == BIT_XOR_EXPR
	  || code_left == PLUS_EXPR || code_left == MINUS_EXPR
	  || code_right == BIT_AND_EXPR || code_right == BIT_XOR_EXPR
	  || code_right == PLUS_EXPR || code_right == MINUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<|%>");
      /* Check cases like x|y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison
	       || TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning (OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<|%>");
      /* Check cases like !x | y */
      else if (code_left == TRUTH_NOT_EXPR
	       && !APPEARS_TO_BE_BOOLEAN_EXPR_P (code_right, arg_right))
	warning (OPT_Wparentheses, "suggest parentheses around operand of "
		 "%<!%> or change %<|%> to %<||%> or %<!%> to %<~%>");
      return;

    case BIT_XOR_EXPR:
      if (code_left == BIT_AND_EXPR
	  || code_left == PLUS_EXPR || code_left == MINUS_EXPR
	  || code_right == BIT_AND_EXPR
	  || code_right == PLUS_EXPR || code_right == MINUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<^%>");
      /* Check cases like x^y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison
	       || TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning (OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<^%>");
      return;

    case BIT_AND_EXPR:
      if (code_left == PLUS_EXPR || code_right == PLUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<+%> in operand of %<&%>");
      else if (code_left == MINUS_EXPR || code_right == MINUS_EXPR)
	warning (OPT_Wparentheses,
		 "suggest parentheses around %<-%> in operand of %<&%>");
      /* Check cases like x&y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison
	       || TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning (OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<&%>");
      /* Check cases like !x & y */
      else if (code_left == TRUTH_NOT_EXPR
	       && !APPEARS_TO_BE_BOOLEAN_EXPR_P (code_right, arg_right))
	warning (OPT_Wparentheses, "suggest parentheses around operand of "
		 "%<!%> or change %<&%> to %<&&%> or %<!%> to %<~%>");
      return;

    case EQ_EXPR:
      if (TREE_CODE_CLASS (code_left) == tcc_comparison
          || TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning (OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<==%>");
      return;
    case NE_EXPR:
      if (TREE_CODE_CLASS (code_left) == tcc_comparison
          || TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning (OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<!=%>");
      return;

    default:
      if (TREE_CODE_CLASS (code) == tcc_comparison
	   && ((TREE_CODE_CLASS (code_left) == tcc_comparison
		&& code_left != NE_EXPR && code_left != EQ_EXPR
		&& INTEGRAL_TYPE_P (TREE_TYPE (arg_left)))
	       || (TREE_CODE_CLASS (code_right) == tcc_comparison
		   && code_right != NE_EXPR && code_right != EQ_EXPR
		   && INTEGRAL_TYPE_P (TREE_TYPE (arg_right)))))
	warning (OPT_Wparentheses, "comparisons like %<X<=Y<=Z%> do not "
		 "have their mathematical meaning");
      return;
    }
#undef NOT_A_BOOLEAN_EXPR_P
}

/* If LABEL (a LABEL_DECL) has not been used, issue a warning.  */

void
warn_for_unused_label (tree label)
{
  if (!TREE_USED (label))
    {
      if (DECL_INITIAL (label))
	warning (OPT_Wunused_label, "label %q+D defined but not used", label);
      else
        warning (OPT_Wunused_label, "label %q+D declared but not defined", label);
    }
}

#ifndef TARGET_HAS_TARGETCM
struct gcc_targetcm targetcm = TARGETCM_INITIALIZER;
#endif

/* Warn for division by zero according to the value of DIVISOR.  LOC
   is the location of the division operator.  */

void
warn_for_div_by_zero (location_t loc, tree divisor)
{
  /* If DIVISOR is zero, and has integral or fixed-point type, issue a warning
     about division by zero.  Do not issue a warning if DIVISOR has a
     floating-point type, since we consider 0.0/0.0 a valid way of
     generating a NaN.  */
  if (c_inhibit_evaluation_warnings == 0
      && (integer_zerop (divisor) || fixed_zerop (divisor)))
    warning_at (loc, OPT_Wdiv_by_zero, "division by zero");
}

/* Subroutine of build_binary_op. Give warnings for comparisons
   between signed and unsigned quantities that may fail. Do the
   checking based on the original operand trees ORIG_OP0 and ORIG_OP1,
   so that casts will be considered, but default promotions won't
   be.

   LOCATION is the location of the comparison operator.

   The arguments of this function map directly to local variables
   of build_binary_op.  */

void 
warn_for_sign_compare (location_t location,
		       tree orig_op0, tree orig_op1, 
		       tree op0, tree op1, 
		       tree result_type, enum tree_code resultcode)
{
  int op0_signed = !TYPE_UNSIGNED (TREE_TYPE (orig_op0));
  int op1_signed = !TYPE_UNSIGNED (TREE_TYPE (orig_op1));
  int unsignedp0, unsignedp1;
  
  /* In C++, check for comparison of different enum types.  */
  if (c_dialect_cxx()
      && TREE_CODE (TREE_TYPE (orig_op0)) == ENUMERAL_TYPE
      && TREE_CODE (TREE_TYPE (orig_op1)) == ENUMERAL_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (orig_op0))
	 != TYPE_MAIN_VARIANT (TREE_TYPE (orig_op1)))
    {
      warning_at (location,
		  OPT_Wsign_compare, "comparison between types %qT and %qT",
		  TREE_TYPE (orig_op0), TREE_TYPE (orig_op1));
    }

  /* Do not warn if the comparison is being done in a signed type,
     since the signed type will only be chosen if it can represent
     all the values of the unsigned type.  */
  if (!TYPE_UNSIGNED (result_type))
    /* OK */;
  /* Do not warn if both operands are unsigned.  */
  else if (op0_signed == op1_signed)
    /* OK */;
  else
    {
      tree sop, uop, base_type;
      bool ovf;

      if (op0_signed)
        sop = orig_op0, uop = orig_op1;
      else 
        sop = orig_op1, uop = orig_op0;

      STRIP_TYPE_NOPS (sop); 
      STRIP_TYPE_NOPS (uop);
      base_type = (TREE_CODE (result_type) == COMPLEX_TYPE
		   ? TREE_TYPE (result_type) : result_type);

      /* Do not warn if the signed quantity is an unsuffixed integer
         literal (or some static constant expression involving such
         literals or a conditional expression involving such literals)
         and it is non-negative.  */
      if (tree_expr_nonnegative_warnv_p (sop, &ovf))
        /* OK */;
      /* Do not warn if the comparison is an equality operation, the
         unsigned quantity is an integral constant, and it would fit
         in the result if the result were signed.  */
      else if (TREE_CODE (uop) == INTEGER_CST
               && (resultcode == EQ_EXPR || resultcode == NE_EXPR)
	       && int_fits_type_p (uop, c_common_signed_type (base_type)))
        /* OK */;
      /* In C, do not warn if the unsigned quantity is an enumeration
         constant and its maximum value would fit in the result if the
         result were signed.  */
      else if (!c_dialect_cxx() && TREE_CODE (uop) == INTEGER_CST
               && TREE_CODE (TREE_TYPE (uop)) == ENUMERAL_TYPE
               && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE (uop)),
				   c_common_signed_type (base_type)))
        /* OK */;
      else 
        warning_at (location,
		    OPT_Wsign_compare, 
		    "comparison between signed and unsigned integer expressions");
    }
  
  /* Warn if two unsigned values are being compared in a size larger
     than their original size, and one (and only one) is the result of
     a `~' operator.  This comparison will always fail.
     
     Also warn if one operand is a constant, and the constant does not
     have all bits set that are set in the ~ operand when it is
     extended.  */

  op0 = get_narrower (op0, &unsignedp0);
  op1 = get_narrower (op1, &unsignedp1);
  
  if ((TREE_CODE (op0) == BIT_NOT_EXPR)
      ^ (TREE_CODE (op1) == BIT_NOT_EXPR))
    {
      if (TREE_CODE (op0) == BIT_NOT_EXPR)
	op0 = get_narrower (TREE_OPERAND (op0, 0), &unsignedp0);
      if (TREE_CODE (op1) == BIT_NOT_EXPR)
	op1 = get_narrower (TREE_OPERAND (op1, 0), &unsignedp1);

      if (host_integerp (op0, 0) || host_integerp (op1, 0))
        {
          tree primop;
          HOST_WIDE_INT constant, mask;
          int unsignedp;
          unsigned int bits;
          
          if (host_integerp (op0, 0))
            {
              primop = op1;
              unsignedp = unsignedp1;
              constant = tree_low_cst (op0, 0);
            }
          else
            {
              primop = op0;
              unsignedp = unsignedp0;
              constant = tree_low_cst (op1, 0);
            }
          
          bits = TYPE_PRECISION (TREE_TYPE (primop));
          if (bits < TYPE_PRECISION (result_type)
              && bits < HOST_BITS_PER_LONG && unsignedp)
            {
              mask = (~ (HOST_WIDE_INT) 0) << bits;
              if ((mask & constant) != mask)
		{
		  if (constant == 0)
		    warning (OPT_Wsign_compare, 
			     "promoted ~unsigned is always non-zero");
		  else
		    warning_at (location, OPT_Wsign_compare, 
				"comparison of promoted ~unsigned with constant");
		}
            }
        }
      else if (unsignedp0 && unsignedp1
               && (TYPE_PRECISION (TREE_TYPE (op0))
                   < TYPE_PRECISION (result_type))
               && (TYPE_PRECISION (TREE_TYPE (op1))
                   < TYPE_PRECISION (result_type)))
        warning_at (location, OPT_Wsign_compare,
                 "comparison of promoted ~unsigned with unsigned");
    }
}

/* Setup a TYPE_DECL node as a typedef representation.

   X is a TYPE_DECL for a typedef statement.  Create a brand new
   ..._TYPE node (which will be just a variant of the existing
   ..._TYPE node with identical properties) and then install X
   as the TYPE_NAME of this brand new (duplicate) ..._TYPE node.

   The whole point here is to end up with a situation where each
   and every ..._TYPE node the compiler creates will be uniquely
   associated with AT MOST one node representing a typedef name.
   This way, even though the compiler substitutes corresponding
   ..._TYPE nodes for TYPE_DECL (i.e. "typedef name") nodes very
   early on, later parts of the compiler can always do the reverse
   translation and get back the corresponding typedef name.  For
   example, given:

	typedef struct S MY_TYPE;
	MY_TYPE object;

   Later parts of the compiler might only know that `object' was of
   type `struct S' if it were not for code just below.  With this
   code however, later parts of the compiler see something like:

	struct S' == struct S
	typedef struct S' MY_TYPE;
	struct S' object;

    And they can then deduce (from the node for type struct S') that
    the original object declaration was:

		MY_TYPE object;

    Being able to do this is important for proper support of protoize,
    and also for generating precise symbolic debugging information
    which takes full account of the programmer's (typedef) vocabulary.

    Obviously, we don't want to generate a duplicate ..._TYPE node if
    the TYPE_DECL node that we are now processing really represents a
    standard built-in type.  */

void
set_underlying_type (tree x)
{
  if (x == error_mark_node)
    return;
  if (DECL_IS_BUILTIN (x))
    {
      if (TYPE_NAME (TREE_TYPE (x)) == 0)
	TYPE_NAME (TREE_TYPE (x)) = x;
    }
  else if (TREE_TYPE (x) != error_mark_node
	   && DECL_ORIGINAL_TYPE (x) == NULL_TREE)
    {
      tree tt = TREE_TYPE (x);
      DECL_ORIGINAL_TYPE (x) = tt;
      tt = build_variant_type_copy (tt);
      TYPE_STUB_DECL (tt) = TYPE_STUB_DECL (DECL_ORIGINAL_TYPE (x));
      TYPE_NAME (tt) = x;
      TREE_USED (tt) = TREE_USED (x);
      TREE_TYPE (x) = tt;
    }
}

/* Returns true if X is a typedef decl.  */

bool
is_typedef_decl (tree x)
{
  return (x && TREE_CODE (x) == TYPE_DECL
          && DECL_ORIGINAL_TYPE (x) != NULL_TREE);
}

/* The C and C++ parsers both use vectors to hold function arguments.
   For efficiency, we keep a cache of unused vectors.  This is the
   cache.  */

typedef VEC(tree,gc)* tree_gc_vec;
DEF_VEC_P(tree_gc_vec);
DEF_VEC_ALLOC_P(tree_gc_vec,gc);
static GTY((deletable)) VEC(tree_gc_vec,gc) *tree_vector_cache;

/* Return a new vector from the cache.  If the cache is empty,
   allocate a new vector.  These vectors are GC'ed, so it is OK if the
   pointer is not released..  */

VEC(tree,gc) *
make_tree_vector (void)
{
  if (!VEC_empty (tree_gc_vec, tree_vector_cache))
    return VEC_pop (tree_gc_vec, tree_vector_cache);
  else
    {
      /* Passing 0 to VEC_alloc returns NULL, and our callers require
	 that we always return a non-NULL value.  The vector code uses
	 4 when growing a NULL vector, so we do too.  */
      return VEC_alloc (tree, gc, 4);
    }
}

/* Release a vector of trees back to the cache.  */

void
release_tree_vector (VEC(tree,gc) *vec)
{
  if (vec != NULL)
    {
      VEC_truncate (tree, vec, 0);
      VEC_safe_push (tree_gc_vec, gc, tree_vector_cache, vec);
    }
}

/* Get a new tree vector holding a single tree.  */

VEC(tree,gc) *
make_tree_vector_single (tree t)
{
  VEC(tree,gc) *ret = make_tree_vector ();
  VEC_quick_push (tree, ret, t);
  return ret;
}

/* Get a new tree vector which is a copy of an existing one.  */

VEC(tree,gc) *
make_tree_vector_copy (const VEC(tree,gc) *orig)
{
  VEC(tree,gc) *ret;
  unsigned int ix;
  tree t;

  ret = make_tree_vector ();
  VEC_reserve (tree, gc, ret, VEC_length (tree, orig));
  for (ix = 0; VEC_iterate (tree, orig, ix, t); ++ix)
    VEC_quick_push (tree, ret, t);
  return ret;
}

#include "gt-c-common.h"
