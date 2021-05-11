/* Subroutines shared by all languages that are variants of C.
   Copyright (C) 1992-2021 Free Software Foundation, Inc.

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

#define GCC_C_COMMON_C

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "memmodel.h"
#include "c-common.h"
#include "gimple-expr.h"
#include "tm_p.h"
#include "stringpool.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "intl.h"
#include "stor-layout.h"
#include "calls.h"
#include "attribs.h"
#include "varasm.h"
#include "trans-mem.h"
#include "c-objc.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "toplev.h"
#include "tree-iterator.h"
#include "opts.h"
#include "gimplify.h"
#include "substring-locations.h"
#include "spellcheck.h"
#include "c-spellcheck.h"
#include "selftest.h"
#include "debug.h"

cpp_reader *parse_in;		/* Declared in c-pragma.h.  */

/* Mode used to build pointers (VOIDmode means ptr_mode).  */

machine_mode c_default_pointer_mode = VOIDmode;

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

	tree char8_type_node;
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

   Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.

	tree wchar_array_type_node;

   Type `char8_t[SOMENUMBER]' or something like it.
   Used when a UTF-8 string literal is created.

	tree char8_array_type_node;

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

/* C/ObjC language option variables.  */


/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.  */

int flag_cond_mismatch;

/* Nonzero means enable C89 Amendment 1 features.  */

int flag_isoc94;

/* Nonzero means use the ISO C99 (or C11) dialect of C.  */

int flag_isoc99;

/* Nonzero means use the ISO C11 dialect of C.  */

int flag_isoc11;

/* Nonzero means use the ISO C2X dialect of C.  */

int flag_isoc2x;

/* Nonzero means that we have builtin functions, and main is an int.  */

int flag_hosted = 1;


/* ObjC language option variables.  */


/* Tells the compiler that this is a special run.  Do not perform any
   compiling, instead we are to test some platform dependent features
   and output a C header file with appropriate definitions.  */

int print_struct_values;

/* Tells the compiler what is the constant string class for ObjC.  */

const char *constant_string_class_name;


/* C++ language option variables.  */

/* The reference version of the ABI for -Wabi.  */

int warn_abi_version = -1;

/* The C++ dialect being used.  Default set in c_common_post_options.  */

enum cxx_dialect cxx_dialect = cxx_unset;

/* Maximum template instantiation depth.  This limit exists to limit the
   time it takes to notice excessively recursive template instantiations.

   The default is lower than the 1024 recommended by the C++0x standard
   because G++ runs out of stack before 1024 with highly recursive template
   argument deduction substitution (g++.dg/cpp0x/enum11.C).  */

int max_tinst_depth = 900;

/* The elements of `ridpointers' are identifier nodes for the reserved
   type names and storage classes.  It is indexed by a RID_... value.  */
tree *ridpointers;

tree (*make_fname_decl) (location_t, tree, int);

/* Nonzero means don't warn about problems that occur when the code is
   executed.  */
int c_inhibit_evaluation_warnings;

/* Whether we are building a boolean conversion inside
   convert_for_assignment, or some other late binary operation.  If
   build_binary_op is called for C (from code shared by C and C++) in
   this case, then the operands have already been folded and the
   result will not be folded again, so C_MAYBE_CONST_EXPR should not
   be generated.  */
bool in_late_binary_op;

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

/* Global visibility options.  */
struct visibility_flags visibility_options;

static tree check_case_value (location_t, tree);


static void check_nonnull_arg (void *, tree, unsigned HOST_WIDE_INT);
static bool nonnull_check_p (tree, unsigned HOST_WIDE_INT);

/* Reserved words.  The third field is a mask: keywords are disabled
   if they match the mask.

   Masks for languages:
   C --std=c89: D_C99 | D_CXXONLY | D_OBJC | D_CXX_OBJC
   C --std=c99: D_CXXONLY | D_OBJC
   ObjC is like C except that D_OBJC and D_CXX_OBJC are not set
   C++ --std=c++98: D_CONLY | D_CXX11 | D_CXX20 | D_OBJC
   C++ --std=c++11: D_CONLY | D_CXX20 | D_OBJC
   C++ --std=c++20: D_CONLY | D_OBJC
   ObjC++ is like C++ except that D_OBJC is not set

   If -fno-asm is used, D_ASM is added to the mask.  If
   -fno-gnu-keywords is used, D_EXT is added.  If -fno-asm and C in
   C89 mode, D_EXT89 is added for both -fno-asm and -fno-gnu-keywords.
   In C with -Wc++-compat, we warn if D_CXXWARN is set.

   Note the complication of the D_CXX_OBJC keywords.  These are
   reserved words such as 'class'.  In C++, 'class' is a reserved
   word.  In Objective-C++ it is too.  In Objective-C, it is a
   reserved word too, but only if it follows an '@' sign.
*/
const struct c_common_resword c_common_reswords[] =
{
  { "_Alignas",		RID_ALIGNAS,   D_CONLY },
  { "_Alignof",		RID_ALIGNOF,   D_CONLY },
  { "_Atomic",		RID_ATOMIC,    D_CONLY },
  { "_Bool",		RID_BOOL,      D_CONLY },
  { "_Complex",		RID_COMPLEX,	0 },
  { "_Imaginary",	RID_IMAGINARY, D_CONLY },
  { "_Float16",         RID_FLOAT16,   D_CONLY },
  { "_Float32",         RID_FLOAT32,   D_CONLY },
  { "_Float64",         RID_FLOAT64,   D_CONLY },
  { "_Float128",        RID_FLOAT128,  D_CONLY },
  { "_Float32x",        RID_FLOAT32X,  D_CONLY },
  { "_Float64x",        RID_FLOAT64X,  D_CONLY },
  { "_Float128x",       RID_FLOAT128X, D_CONLY },
  { "_Decimal32",       RID_DFLOAT32,  D_CONLY },
  { "_Decimal64",       RID_DFLOAT64,  D_CONLY },
  { "_Decimal128",      RID_DFLOAT128, D_CONLY },
  { "_Fract",           RID_FRACT,     D_CONLY | D_EXT },
  { "_Accum",           RID_ACCUM,     D_CONLY | D_EXT },
  { "_Sat",             RID_SAT,       D_CONLY | D_EXT },
  { "_Static_assert",   RID_STATIC_ASSERT, D_CONLY },
  { "_Noreturn",        RID_NORETURN,  D_CONLY },
  { "_Generic",         RID_GENERIC,   D_CONLY },
  { "_Thread_local",    RID_THREAD,    D_CONLY },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof",	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__auto_type",	RID_AUTO_TYPE,	D_CONLY },
  { "__bases",          RID_BASES, D_CXXONLY },
  { "__builtin_addressof", RID_ADDRESSOF, D_CXXONLY },
  { "__builtin_bit_cast", RID_BUILTIN_BIT_CAST, D_CXXONLY },
  { "__builtin_call_with_static_chain",
    RID_BUILTIN_CALL_WITH_STATIC_CHAIN, D_CONLY },
  { "__builtin_choose_expr", RID_CHOOSE_EXPR, D_CONLY },
  { "__builtin_complex", RID_BUILTIN_COMPLEX, D_CONLY },
  { "__builtin_convertvector", RID_BUILTIN_CONVERTVECTOR, 0 },
  { "__builtin_has_attribute", RID_BUILTIN_HAS_ATTRIBUTE, 0 },
  { "__builtin_launder", RID_BUILTIN_LAUNDER, D_CXXONLY },
  { "__builtin_shuffle", RID_BUILTIN_SHUFFLE, 0 },
  { "__builtin_tgmath", RID_BUILTIN_TGMATH, D_CONLY },
  { "__builtin_offsetof", RID_OFFSETOF, 0 },
  { "__builtin_types_compatible_p", RID_TYPES_COMPATIBLE_P, D_CONLY },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__constinit",	RID_CONSTINIT,	D_CXXONLY },
  { "__decltype",       RID_DECLTYPE,   D_CXXONLY },
  { "__direct_bases",   RID_DIRECT_BASES, D_CXXONLY },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME, 0 },
  { "__has_nothrow_assign", RID_HAS_NOTHROW_ASSIGN, D_CXXONLY },
  { "__has_nothrow_constructor", RID_HAS_NOTHROW_CONSTRUCTOR, D_CXXONLY },
  { "__has_nothrow_copy", RID_HAS_NOTHROW_COPY, D_CXXONLY },
  { "__has_trivial_assign", RID_HAS_TRIVIAL_ASSIGN, D_CXXONLY },
  { "__has_trivial_constructor", RID_HAS_TRIVIAL_CONSTRUCTOR, D_CXXONLY },
  { "__has_trivial_copy", RID_HAS_TRIVIAL_COPY, D_CXXONLY },
  { "__has_trivial_destructor", RID_HAS_TRIVIAL_DESTRUCTOR, D_CXXONLY },
  { "__has_unique_object_representations", RID_HAS_UNIQUE_OBJ_REPRESENTATIONS,
					D_CXXONLY },
  { "__has_virtual_destructor", RID_HAS_VIRTUAL_DESTRUCTOR, D_CXXONLY },
  { "__imag",		RID_IMAGPART,	0 },
  { "__imag__",		RID_IMAGPART,	0 },
  { "__inline",		RID_INLINE,	0 },
  { "__inline__",	RID_INLINE,	0 },
  { "__is_abstract",	RID_IS_ABSTRACT, D_CXXONLY },
  { "__is_aggregate",	RID_IS_AGGREGATE, D_CXXONLY },
  { "__is_base_of",	RID_IS_BASE_OF, D_CXXONLY },
  { "__is_class",	RID_IS_CLASS,	D_CXXONLY },
  { "__is_empty",	RID_IS_EMPTY,	D_CXXONLY },
  { "__is_enum",	RID_IS_ENUM,	D_CXXONLY },
  { "__is_final",	RID_IS_FINAL,	D_CXXONLY },
  { "__is_literal_type", RID_IS_LITERAL_TYPE, D_CXXONLY },
  { "__is_pod",		RID_IS_POD,	D_CXXONLY },
  { "__is_polymorphic",	RID_IS_POLYMORPHIC, D_CXXONLY },
  { "__is_same",     RID_IS_SAME_AS, D_CXXONLY },
  { "__is_same_as",     RID_IS_SAME_AS, D_CXXONLY },
  { "__is_standard_layout", RID_IS_STD_LAYOUT, D_CXXONLY },
  { "__is_trivial",     RID_IS_TRIVIAL, D_CXXONLY },
  { "__is_trivially_assignable", RID_IS_TRIVIALLY_ASSIGNABLE, D_CXXONLY },
  { "__is_trivially_constructible", RID_IS_TRIVIALLY_CONSTRUCTIBLE, D_CXXONLY },
  { "__is_trivially_copyable", RID_IS_TRIVIALLY_COPYABLE, D_CXXONLY },
  { "__is_union",	RID_IS_UNION,	D_CXXONLY },
  { "__label__",	RID_LABEL,	0 },
  { "__null",		RID_NULL,	0 },
  { "__real",		RID_REALPART,	0 },
  { "__real__",		RID_REALPART,	0 },
  { "__restrict",	RID_RESTRICT,	0 },
  { "__restrict__",	RID_RESTRICT,	0 },
  { "__signed",		RID_SIGNED,	0 },
  { "__signed__",	RID_SIGNED,	0 },
  { "__thread",		RID_THREAD,	0 },
  { "__transaction_atomic", RID_TRANSACTION_ATOMIC, 0 },
  { "__transaction_relaxed", RID_TRANSACTION_RELAXED, 0 },
  { "__transaction_cancel", RID_TRANSACTION_CANCEL, 0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__underlying_type", RID_UNDERLYING_TYPE, D_CXXONLY },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "__GIMPLE",		RID_GIMPLE,	D_CONLY },
  { "__PHI",		RID_PHI,	D_CONLY },
  { "__RTL",		RID_RTL,	D_CONLY },
  { "alignas",		RID_ALIGNAS,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "alignof",		RID_ALIGNOF,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "asm",		RID_ASM,	D_ASM },
  { "auto",		RID_AUTO,	0 },
  { "bool",		RID_BOOL,	D_CXXONLY | D_CXXWARN },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "catch",		RID_CATCH,	D_CXX_OBJC | D_CXXWARN },
  { "char",		RID_CHAR,	0 },
  { "char8_t",		RID_CHAR8,	D_CXX_CHAR8_T_FLAGS | D_CXXWARN },
  { "char16_t",		RID_CHAR16,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "char32_t",		RID_CHAR32,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "class",		RID_CLASS,	D_CXX_OBJC | D_CXXWARN },
  { "const",		RID_CONST,	0 },
  { "consteval",	RID_CONSTEVAL,	D_CXXONLY | D_CXX20 | D_CXXWARN },
  { "constexpr",	RID_CONSTEXPR,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "constinit",	RID_CONSTINIT,	D_CXXONLY | D_CXX20 | D_CXXWARN },
  { "const_cast",	RID_CONSTCAST,	D_CXXONLY | D_CXXWARN },
  { "continue",		RID_CONTINUE,	0 },
  { "decltype",         RID_DECLTYPE,   D_CXXONLY | D_CXX11 | D_CXXWARN },
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
  { "noexcept",		RID_NOEXCEPT,	D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "nullptr",		RID_NULLPTR,	D_CXXONLY | D_CXX11 | D_CXXWARN },
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
  { "static_assert",    RID_STATIC_ASSERT, D_CXXONLY | D_CXX11 | D_CXXWARN },
  { "static_cast",	RID_STATCAST,	D_CXXONLY | D_CXXWARN },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "template",		RID_TEMPLATE,	D_CXXONLY | D_CXXWARN },
  { "this",		RID_THIS,	D_CXXONLY | D_CXXWARN },
  { "thread_local",	RID_THREAD,	D_CXXONLY | D_CXX11 | D_CXXWARN },
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
  { "__is_assignable", RID_IS_ASSIGNABLE, D_CXXONLY },
  { "__is_constructible", RID_IS_CONSTRUCTIBLE, D_CXXONLY },
  { "__is_nothrow_assignable", RID_IS_NOTHROW_ASSIGNABLE, D_CXXONLY },
  { "__is_nothrow_constructible", RID_IS_NOTHROW_CONSTRUCTIBLE, D_CXXONLY },

  /* C++ transactional memory.  */
  { "synchronized",	RID_SYNCHRONIZED, D_CXX_OBJC | D_TRANSMEM },
  { "atomic_noexcept",	RID_ATOMIC_NOEXCEPT, D_CXXONLY | D_TRANSMEM },
  { "atomic_cancel",	RID_ATOMIC_CANCEL, D_CXXONLY | D_TRANSMEM },
  { "atomic_commit",	RID_TRANSACTION_ATOMIC, D_CXXONLY | D_TRANSMEM },

  /* Concepts-related keywords */
  { "concept",		RID_CONCEPT,	D_CXX_CONCEPTS_FLAGS | D_CXXWARN },
  { "requires", 	RID_REQUIRES,	D_CXX_CONCEPTS_FLAGS | D_CXXWARN },

  /* Modules-related keywords, these are internal unspellable tokens,
     created by the preprocessor.  */
  { "module ",		RID__MODULE,	D_CXX_MODULES_FLAGS | D_CXXWARN },
  { "import ",		RID__IMPORT,	D_CXX_MODULES_FLAGS | D_CXXWARN },
  { "export ",		RID__EXPORT,	D_CXX_MODULES_FLAGS | D_CXXWARN },

  /* Coroutines-related keywords */
  { "co_await",		RID_CO_AWAIT,	D_CXX_COROUTINES_FLAGS | D_CXXWARN },
  { "co_yield",		RID_CO_YIELD,	D_CXX_COROUTINES_FLAGS | D_CXXWARN },
  { "co_return", 	RID_CO_RETURN,	D_CXX_COROUTINES_FLAGS | D_CXXWARN },

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
  { "optional",		RID_AT_OPTIONAL,	D_OBJC },
  { "required",		RID_AT_REQUIRED,	D_OBJC },
  { "property",		RID_AT_PROPERTY,	D_OBJC },
  { "package",		RID_AT_PACKAGE,		D_OBJC },
  { "synthesize",	RID_AT_SYNTHESIZE,	D_OBJC },
  { "dynamic",		RID_AT_DYNAMIC,		D_OBJC },
  /* These are recognized only in protocol-qualifier context
     (see above) */
  { "bycopy",		RID_BYCOPY,		D_OBJC },
  { "byref",		RID_BYREF,		D_OBJC },
  { "in",		RID_IN,			D_OBJC },
  { "inout",		RID_INOUT,		D_OBJC },
  { "oneway",		RID_ONEWAY,		D_OBJC },
  { "out",		RID_OUT,		D_OBJC },
  /* These are recognized inside a property attribute list */
  { "assign",		RID_ASSIGN,		D_OBJC },
  { "atomic",		RID_PROPATOMIC,		D_OBJC },
  { "copy",		RID_COPY,		D_OBJC },
  { "getter",		RID_GETTER,		D_OBJC },
  { "nonatomic",	RID_NONATOMIC,		D_OBJC },
  { "readonly",		RID_READONLY,		D_OBJC },
  { "readwrite",	RID_READWRITE,		D_OBJC },
  { "retain",		RID_RETAIN,		D_OBJC },
  { "setter",		RID_SETTER,		D_OBJC },
  /* These are Objective C implementation of nullability, accepted only in
     specific contexts.  */
  { "null_unspecified", RID_NULL_UNSPECIFIED,	D_OBJC },
  { "nullable",		RID_NULLABLE,		D_OBJC },
  { "nonnull",		RID_NONNULL,		D_OBJC },
  { "null_resettable",	RID_NULL_RESETTABLE,	D_OBJC },
};

const unsigned int num_c_common_reswords =
  sizeof c_common_reswords / sizeof (struct c_common_resword);

/* Return identifier for address space AS.  */

const char *
c_addr_space_name (addr_space_t as)
{
  int rid = RID_FIRST_ADDR_SPACE + as;
  gcc_assert (ridpointers [rid]);
  return IDENTIFIER_POINTER (ridpointers [rid]);
}

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
	  saved = tree_cons (decl, build_int_cst (integer_type_node, ix),
			     saved);
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
  int nchars, charsz;
  tree e_type, i_type, a_type;

  /* Compute the number of elements, for the array type.  */
  if (TREE_TYPE (value) == char_array_type_node || !TREE_TYPE (value))
    {
      charsz = 1;
      e_type = char_type_node;
    }
  else if (flag_char8_t && TREE_TYPE (value) == char8_array_type_node)
    {
      charsz = TYPE_PRECISION (char8_type_node) / BITS_PER_UNIT;
      e_type = char8_type_node;
    }
  else if (TREE_TYPE (value) == char16_array_type_node)
    {
      charsz = TYPE_PRECISION (char16_type_node) / BITS_PER_UNIT;
      e_type = char16_type_node;
    }
  else if (TREE_TYPE (value) == char32_array_type_node)
    {
      charsz = TYPE_PRECISION (char32_type_node) / BITS_PER_UNIT;
      e_type = char32_type_node;
    }
  else
    {
      charsz = TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT;
      e_type = wchar_type_node;
    }

  /* This matters only for targets where ssizetype has smaller precision
     than 32 bits.  */
  if (wi::lts_p (wi::to_wide (TYPE_MAX_VALUE (ssizetype)), length))
    {
      error ("size of string literal is too large");
      length = tree_to_shwi (TYPE_MAX_VALUE (ssizetype)) / charsz * charsz;
      char *str = CONST_CAST (char *, TREE_STRING_POINTER (value));
      memset (str + length, '\0',
	      MIN (TREE_STRING_LENGTH (value) - length, charsz));
      TREE_STRING_LENGTH (value) = length;
    }
  nchars = length / charsz;

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
  i_type = build_index_type (size_int (nchars - 1));
  a_type = build_array_type (e_type, i_type);
  if (c_dialect_cxx() || warn_write_strings)
    a_type = c_build_qualified_type (a_type, TYPE_QUAL_CONST);

  TREE_TYPE (value) = a_type;
  TREE_CONSTANT (value) = 1;
  TREE_READONLY (value) = 1;
  TREE_STATIC (value) = 1;
  return value;
}

/* Given a string of type STRING_TYPE, determine what kind of string
   token would give an equivalent execution encoding: CPP_STRING,
   CPP_STRING16, or CPP_STRING32.  Return CPP_OTHER in case of error.
   This may not be exactly the string token type that initially created
   the string, since CPP_WSTRING is indistinguishable from the 16/32 bit
   string type, and CPP_UTF8STRING is indistinguishable from CPP_STRING
   at this point.

   This effectively reverses part of the logic in lex_string and
   fix_string_type.  */

static enum cpp_ttype
get_cpp_ttype_from_string_type (tree string_type)
{
  gcc_assert (string_type);
  if (TREE_CODE (string_type) == POINTER_TYPE)
    string_type = TREE_TYPE (string_type);

  if (TREE_CODE (string_type) != ARRAY_TYPE)
    return CPP_OTHER;

  tree element_type = TREE_TYPE (string_type);
  if (TREE_CODE (element_type) != INTEGER_TYPE)
    return CPP_OTHER;

  int bits_per_character = TYPE_PRECISION (element_type);
  switch (bits_per_character)
    {
    case 8:
      return CPP_STRING;  /* It could have also been CPP_UTF8STRING.  */
    case 16:
      return CPP_STRING16;
    case 32:
      return CPP_STRING32;
    }

  return CPP_OTHER;
}

/* The global record of string concatentations, for use in
   extracting locations within string literals.  */

GTY(()) string_concat_db *g_string_concat_db;

/* Implementation of LANG_HOOKS_GET_SUBSTRING_LOCATION.  */

const char *
c_get_substring_location (const substring_loc &substr_loc,
			  location_t *out_loc)
{
  enum cpp_ttype tok_type
    = get_cpp_ttype_from_string_type (substr_loc.get_string_type ());
  if (tok_type == CPP_OTHER)
    return "unrecognized string type";

  return get_location_within_string (parse_in, g_string_concat_db,
				     substr_loc.get_fmt_string_loc (),
				     tok_type,
				     substr_loc.get_caret_idx (),
				     substr_loc.get_start_idx (),
				     substr_loc.get_end_idx (),
				     out_loc);
}


/* Return true iff T is a boolean promoted to int.  */

bool
bool_promoted_to_int_p (tree t)
{
  return (CONVERT_EXPR_P (t)
	  && TREE_TYPE (t) == integer_type_node
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == BOOLEAN_TYPE);
}

/* vector_targets_convertible_p is used for vector pointer types.  The
   callers perform various checks that the qualifiers are satisfactory,
   while OTOH vector_targets_convertible_p ignores the number of elements
   in the vectors.  That's fine with vector pointers as we can consider,
   say, a vector of 8 elements as two consecutive vectors of 4 elements,
   and that does not require and conversion of the pointer values.
   In contrast, vector_types_convertible_p and
   vector_types_compatible_elements_p are used for vector value types.  */
/* True if pointers to distinct types T1 and T2 can be converted to
   each other without an explicit cast.  Only returns true for opaque
   vector types.  */
bool
vector_targets_convertible_p (const_tree t1, const_tree t2)
{
  if (VECTOR_TYPE_P (t1) && VECTOR_TYPE_P (t2)
      && (TYPE_VECTOR_OPAQUE (t1) || TYPE_VECTOR_OPAQUE (t2))
      && tree_int_cst_equal (TYPE_SIZE (t1), TYPE_SIZE (t2)))
    return true;

  return false;
}

/* vector_types_convertible_p is used for vector value types.
   It could in principle call vector_targets_convertible_p as a subroutine,
   but then the check for vector type would be duplicated with its callers,
   and also the purpose of vector_targets_convertible_p would become
   muddled.
   Where vector_types_convertible_p returns true, a conversion might still be
   needed to make the types match.
   In contrast, vector_targets_convertible_p is used for vector pointer
   values, and vector_types_compatible_elements_p is used specifically
   in the context for binary operators, as a check if use is possible without
   conversion.  */
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
     && (TREE_CODE (TREE_TYPE (t1)) != REAL_TYPE
	 || known_eq (TYPE_VECTOR_SUBPARTS (t1),
		      TYPE_VECTOR_SUBPARTS (t2)))
     && (INTEGRAL_TYPE_P (TREE_TYPE (t1))
	 == INTEGRAL_TYPE_P (TREE_TYPE (t2))));

  if (!convertible_lax || flag_lax_vector_conversions)
    return convertible_lax;

  if (known_eq (TYPE_VECTOR_SUBPARTS (t1), TYPE_VECTOR_SUBPARTS (t2))
      && lang_hooks.types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return true;

  if (emit_lax_note && !emitted_lax_note)
    {
      emitted_lax_note = true;
      inform (input_location, "use %<-flax-vector-conversions%> to permit "
              "conversions between vectors with differing "
              "element types or numbers of subparts");
    }

  return false;
}

/* Build a VEC_PERM_EXPR if V0, V1 and MASK are not error_mark_nodes
   and have vector types, V0 has the same type as V1, and the number of
   elements of V0, V1, MASK is the same.

   In case V1 is a NULL_TREE it is assumed that __builtin_shuffle was
   called with two arguments.  In this case implementation passes the
   first argument twice in order to share the same tree code.  This fact
   could enable the mask-values being twice the vector length.  This is
   an implementation accident and this semantics is not guaranteed to
   the user.  */
tree
c_build_vec_perm_expr (location_t loc, tree v0, tree v1, tree mask,
		       bool complain)
{
  tree ret;
  bool wrap = true;
  bool maybe_const = false;
  bool two_arguments = false;

  if (v1 == NULL_TREE)
    {
      two_arguments = true;
      v1 = v0;
    }

  if (v0 == error_mark_node || v1 == error_mark_node
      || mask == error_mark_node)
    return error_mark_node;

  if (!gnu_vector_type_p (TREE_TYPE (mask))
      || !VECTOR_INTEGER_TYPE_P (TREE_TYPE (mask)))
    {
      if (complain)
	error_at (loc, "%<__builtin_shuffle%> last argument must "
		       "be an integer vector");
      return error_mark_node;
    }

  if (!gnu_vector_type_p (TREE_TYPE (v0))
      || !gnu_vector_type_p (TREE_TYPE (v1)))
    {
      if (complain)
	error_at (loc, "%<__builtin_shuffle%> arguments must be vectors");
      return error_mark_node;
    }

  if (TYPE_MAIN_VARIANT (TREE_TYPE (v0)) != TYPE_MAIN_VARIANT (TREE_TYPE (v1)))
    {
      if (complain)
	error_at (loc, "%<__builtin_shuffle%> argument vectors must be of "
		       "the same type");
      return error_mark_node;
    }

  if (maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (v0)),
		TYPE_VECTOR_SUBPARTS (TREE_TYPE (mask)))
      && maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (v1)),
		   TYPE_VECTOR_SUBPARTS (TREE_TYPE (mask))))
    {
      if (complain)
	error_at (loc, "%<__builtin_shuffle%> number of elements of the "
		       "argument vector(s) and the mask vector should "
		       "be the same");
      return error_mark_node;
    }

  if (GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (TREE_TYPE (v0))))
      != GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (TREE_TYPE (mask)))))
    {
      if (complain)
	error_at (loc, "%<__builtin_shuffle%> argument vector(s) inner type "
		       "must have the same size as inner type of the mask");
      return error_mark_node;
    }

  if (!c_dialect_cxx ())
    {
      /* Avoid C_MAYBE_CONST_EXPRs inside VEC_PERM_EXPR.  */
      v0 = c_fully_fold (v0, false, &maybe_const);
      wrap &= maybe_const;

      if (two_arguments)
        v1 = v0 = save_expr (v0);
      else
        {
          v1 = c_fully_fold (v1, false, &maybe_const);
          wrap &= maybe_const;
        }

      mask = c_fully_fold (mask, false, &maybe_const);
      wrap &= maybe_const;
    }
  else if (two_arguments)
    v1 = v0 = save_expr (v0);

  ret = build3_loc (loc, VEC_PERM_EXPR, TREE_TYPE (v0), v0, v1, mask);

  if (!c_dialect_cxx () && !wrap)
    ret = c_wrap_maybe_const (ret, true);

  return ret;
}

/* Build a VEC_CONVERT ifn for __builtin_convertvector builtin.  */

tree
c_build_vec_convert (location_t loc1, tree expr, location_t loc2, tree type,
		     bool complain)
{
  if (error_operand_p (type))
    return error_mark_node;
  if (error_operand_p (expr))
    return error_mark_node;

  if (!gnu_vector_type_p (TREE_TYPE (expr))
      || (!VECTOR_INTEGER_TYPE_P (TREE_TYPE (expr))
	  && !VECTOR_FLOAT_TYPE_P (TREE_TYPE (expr))))
    {
      if (complain)
	error_at (loc1, "%<__builtin_convertvector%> first argument must "
			"be an integer or floating vector");
      return error_mark_node;
    }

  if (!gnu_vector_type_p (type)
      || (!VECTOR_INTEGER_TYPE_P (type) && !VECTOR_FLOAT_TYPE_P (type)))
    {
      if (complain)
	error_at (loc2, "%<__builtin_convertvector%> second argument must "
			"be an integer or floating vector type");
      return error_mark_node;
    }

  if (maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (expr)),
		TYPE_VECTOR_SUBPARTS (type)))
    {
      if (complain)
	error_at (loc1, "%<__builtin_convertvector%> number of elements "
			"of the first argument vector and the second argument "
			"vector type should be the same");
      return error_mark_node;
    }

  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (expr)))
       == TYPE_MAIN_VARIANT (TREE_TYPE (type)))
      || (VECTOR_INTEGER_TYPE_P (TREE_TYPE (expr))
	  && VECTOR_INTEGER_TYPE_P (type)
	  && (TYPE_PRECISION (TREE_TYPE (TREE_TYPE (expr)))
	      == TYPE_PRECISION (TREE_TYPE (type)))))
    return build1_loc (loc1, VIEW_CONVERT_EXPR, type, expr);

  bool wrap = true;
  bool maybe_const = false;
  tree ret;
  if (!c_dialect_cxx ())
    {
      /* Avoid C_MAYBE_CONST_EXPRs inside of VEC_CONVERT argument.  */
      expr = c_fully_fold (expr, false, &maybe_const);
      wrap &= maybe_const;
    }

  ret = build_call_expr_internal_loc (loc1, IFN_VEC_CONVERT, type, 1, expr);

  if (!wrap)
    ret = c_wrap_maybe_const (ret, true);

  return ret;
}

/* Like tree.c:get_narrower, but retain conversion from C++0x scoped enum
   to integral type.  */

tree
c_common_get_narrower (tree op, int *unsignedp_ptr)
{
  op = get_narrower (op, unsignedp_ptr);

  if (TREE_CODE (TREE_TYPE (op)) == ENUMERAL_TYPE
      && ENUM_IS_SCOPED (TREE_TYPE (op)))
    {
      /* C++0x scoped enumerations don't implicitly convert to integral
	 type; if we stripped an explicit conversion to a larger type we
	 need to replace it so common_type will still work.  */
      tree type = c_common_type_for_size (TYPE_PRECISION (TREE_TYPE (op)),
					  TYPE_UNSIGNED (TREE_TYPE (op)));
      op = fold_convert (type, op);
    }
  return op;
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
tree
shorten_binary_op (tree result_type, tree op0, tree op1, bool bitwise)
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

  arg0 = c_common_get_narrower (op0, &unsigned0);
  arg1 = c_common_get_narrower (op1, &unsigned1);

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

/* Returns true iff any integer value of type FROM_TYPE can be represented as
   real of type TO_TYPE.  This is a helper function for unsafe_conversion_p.  */

static bool
int_safely_convertible_to_real_p (const_tree from_type, const_tree to_type)
{
  tree type_low_bound = TYPE_MIN_VALUE (from_type);
  tree type_high_bound = TYPE_MAX_VALUE (from_type);
  REAL_VALUE_TYPE real_low_bound =
	  real_value_from_int_cst (0, type_low_bound);
  REAL_VALUE_TYPE real_high_bound =
	  real_value_from_int_cst (0, type_high_bound);

  return exact_real_truncate (TYPE_MODE (to_type), &real_low_bound)
	 && exact_real_truncate (TYPE_MODE (to_type), &real_high_bound);
}

/* Checks if expression EXPR of complex/real/integer type cannot be converted
   to the complex/real/integer type TYPE.  Function returns non-zero when:
	* EXPR is a constant which cannot be exactly converted to TYPE.
	* EXPR is not a constant and size of EXPR's type > than size of TYPE,
	  for EXPR type and TYPE being both integers or both real, or both
	  complex.
	* EXPR is not a constant of complex type and TYPE is a real or
	  an integer.
	* EXPR is not a constant of real type and TYPE is an integer.
	* EXPR is not a constant of integer type which cannot be
	  exactly converted to real type.

   Function allows conversions between types of different signedness if
   CHECK_SIGN is false and can return SAFE_CONVERSION (zero) in that
   case.  Function can return UNSAFE_SIGN if CHECK_SIGN is true.

   RESULT, when non-null is the result of the conversion.  When constant
   it is included in the text of diagnostics.

   Function allows conversions from complex constants to non-complex types,
   provided that imaginary part is zero and real part can be safely converted
   to TYPE.  */

enum conversion_safety
unsafe_conversion_p (tree type, tree expr, tree result, bool check_sign)
{
  enum conversion_safety give_warning = SAFE_CONVERSION; /* is 0 or false */
  tree expr_type = TREE_TYPE (expr);

  expr = fold_for_warn (expr);

  if (TREE_CODE (expr) == REAL_CST || TREE_CODE (expr) == INTEGER_CST)
    {
      /* If type is complex, we are interested in compatibility with
	 underlying type.  */
      if (TREE_CODE (type) == COMPLEX_TYPE)
	  type = TREE_TYPE (type);

      /* Warn for real constant that is not an exact integer converted
	 to integer type.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
	  && TREE_CODE (type) == INTEGER_TYPE)
	{
	  if (!real_isinteger (TREE_REAL_CST_PTR (expr), TYPE_MODE (expr_type)))
	    give_warning = UNSAFE_REAL;
	}
      /* Warn for an integer constant that does not fit into integer type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
	       && TREE_CODE (type) == INTEGER_TYPE
	       && !int_fits_type_p (expr, type))
	{
	  if (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type)
	      && tree_int_cst_sgn (expr) < 0)
	    {
	      if (check_sign)
		give_warning = UNSAFE_SIGN;
	    }
	  else if (!TYPE_UNSIGNED (type) && TYPE_UNSIGNED (expr_type))
	    {
	      if (check_sign)
		give_warning = UNSAFE_SIGN;
	    }
	  else
	    give_warning = UNSAFE_OTHER;
	}
      else if (TREE_CODE (type) == REAL_TYPE)
	{
	  /* Warn for an integer constant that does not fit into real type.  */
	  if (TREE_CODE (expr_type) == INTEGER_TYPE)
	    {
	      REAL_VALUE_TYPE a = real_value_from_int_cst (0, expr);
	      if (!exact_real_truncate (TYPE_MODE (type), &a))
		give_warning = UNSAFE_REAL;
	    }
	  /* Warn for a real constant that does not fit into a smaller
	     real type.  */
	  else if (TREE_CODE (expr_type) == REAL_TYPE
		   && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	    {
	      REAL_VALUE_TYPE a = TREE_REAL_CST (expr);
	      if (!exact_real_truncate (TYPE_MODE (type), &a))
		give_warning = UNSAFE_REAL;
	    }
	}
    }

  else if (TREE_CODE (expr) == COMPLEX_CST)
    {
      tree imag_part = TREE_IMAGPART (expr);
      /* Conversion from complex constant with zero imaginary part,
	 perform check for conversion of real part.  */
      if ((TREE_CODE (imag_part) == REAL_CST
	   && real_zerop (imag_part))
	  || (TREE_CODE (imag_part) == INTEGER_CST
	      && integer_zerop (imag_part)))
	/* Note: in this branch we use recursive call to unsafe_conversion_p
	   with different type of EXPR, but it is still safe, because when EXPR
	   is a constant, it's type is not used in text of generated warnings
	   (otherwise they could sound misleading).  */
	return unsafe_conversion_p (type, TREE_REALPART (expr), result,
				    check_sign);
      /* Conversion from complex constant with non-zero imaginary part.  */
      else
	{
	  /* Conversion to complex type.
	     Perform checks for both real and imaginary parts.  */
	  if (TREE_CODE (type) == COMPLEX_TYPE)
	    {
	      enum conversion_safety re_safety =
		unsafe_conversion_p (type, TREE_REALPART (expr),
				     result, check_sign);
	      enum conversion_safety im_safety =
		unsafe_conversion_p (type, imag_part, result, check_sign);

	      /* Merge the results into appropriate single warning.  */

	      /* Note: this case includes SAFE_CONVERSION, i.e. success.  */
	      if (re_safety == im_safety)
		give_warning = re_safety;
	      else if (!re_safety && im_safety)
		give_warning = im_safety;
	      else if (re_safety && !im_safety)
		give_warning = re_safety;
	      else
		give_warning = UNSAFE_OTHER;
	    }
	  /* Warn about conversion from complex to real or integer type.  */
	  else
	    give_warning = UNSAFE_IMAGINARY;
	}
    }

  /* Checks for remaining case: EXPR is not constant.  */
  else
    {
      /* Warn for real types converted to integer types.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
	  && TREE_CODE (type) == INTEGER_TYPE)
	give_warning = UNSAFE_REAL;

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
		    return SAFE_CONVERSION;
		  /* If constant is unsigned and fits in the target
		     type, then the result will also fit.  */
		  else if ((TREE_CODE (op0) == INTEGER_CST
			    && unsigned0
			    && int_fits_type_p (op0, type))
			   || (TREE_CODE (op1) == INTEGER_CST
			       && unsigned1
			       && int_fits_type_p (op1, type)))
		    return SAFE_CONVERSION;
		}
	    }
	  /* Warn for integer types converted to smaller integer types.  */
	  if (TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	    give_warning = UNSAFE_OTHER;

	  /* When they are the same width but different signedness,
	     then the value may change.  */
	  else if (((TYPE_PRECISION (type) == TYPE_PRECISION (expr_type)
		     && TYPE_UNSIGNED (expr_type) != TYPE_UNSIGNED (type))
		    /* Even when converted to a bigger type, if the type is
		       unsigned but expr is signed, then negative values
		       will be changed.  */
		    || (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type)))
		   && check_sign)
	    give_warning = UNSAFE_SIGN;
	}

      /* Warn for integer types converted to real types if and only if
	 all the range of values of the integer type cannot be
	 represented by the real type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
	       && TREE_CODE (type) == REAL_TYPE)
	{
	  /* Don't warn about char y = 0xff; float x = (int) y;  */
	  expr = get_unwidened (expr, 0);
	  expr_type = TREE_TYPE (expr);

	  if (!int_safely_convertible_to_real_p (expr_type, type))
	    give_warning = UNSAFE_OTHER;
	}

      /* Warn for real types converted to smaller real types.  */
      else if (TREE_CODE (expr_type) == REAL_TYPE
	       && TREE_CODE (type) == REAL_TYPE
	       && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	give_warning = UNSAFE_REAL;

      /* Check conversion between two complex types.  */
      else if (TREE_CODE (expr_type) == COMPLEX_TYPE
	       && TREE_CODE (type) == COMPLEX_TYPE)
	{
	  /* Extract underlying types (i.e., type of real and imaginary
	     parts) of expr_type and type.  */
	  tree from_type = TREE_TYPE (expr_type);
	  tree to_type = TREE_TYPE (type);

	  /* Warn for real types converted to integer types.  */
	  if (TREE_CODE (from_type) == REAL_TYPE
	      && TREE_CODE (to_type) == INTEGER_TYPE)
	    give_warning = UNSAFE_REAL;

	  /* Warn for real types converted to smaller real types.  */
	  else if (TREE_CODE (from_type) == REAL_TYPE
		   && TREE_CODE (to_type) == REAL_TYPE
		   && TYPE_PRECISION (to_type) < TYPE_PRECISION (from_type))
	    give_warning = UNSAFE_REAL;

	  /* Check conversion for complex integer types.  Here implementation
	     is simpler than for real-domain integers because it does not
	     involve sophisticated cases, such as bitmasks, casts, etc.  */
	  else if (TREE_CODE (from_type) == INTEGER_TYPE
		   && TREE_CODE (to_type) == INTEGER_TYPE)
	    {
	      /* Warn for integer types converted to smaller integer types.  */
	      if (TYPE_PRECISION (to_type) < TYPE_PRECISION (from_type))
		give_warning = UNSAFE_OTHER;

	      /* Check for different signedness, see case for real-domain
		 integers (above) for a more detailed comment.  */
	      else if (((TYPE_PRECISION (to_type) == TYPE_PRECISION (from_type)
			 && TYPE_UNSIGNED (to_type) != TYPE_UNSIGNED (from_type))
			|| (TYPE_UNSIGNED (to_type) && !TYPE_UNSIGNED (from_type)))
		       && check_sign)
		give_warning = UNSAFE_SIGN;
	    }
	  else if (TREE_CODE (from_type) == INTEGER_TYPE
		   && TREE_CODE (to_type) == REAL_TYPE
		   && !int_safely_convertible_to_real_p (from_type, to_type))
	    give_warning = UNSAFE_OTHER;
	}

      /* Warn for complex types converted to real or integer types.  */
      else if (TREE_CODE (expr_type) == COMPLEX_TYPE
	       && TREE_CODE (type) != COMPLEX_TYPE)
	give_warning = UNSAFE_IMAGINARY;
    }

  return give_warning;
}


/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (location_t loc, tree type, tree expr)
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
    warnings_for_convert_and_check (loc, type, expr_for_warning, result);

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
static bool warning_candidate_p (tree);
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
	  *end = copy ? new_tlist (NULL, add->expr, add->writer) : add;
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
	  warning_at (EXPR_LOC_OR_LOC (writer, input_location),
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

static bool
warning_candidate_p (tree x)
{
  if (DECL_P (x) && DECL_ARTIFICIAL (x))
    return false;

  if (TREE_CODE (x) == BLOCK)
    return false;

  /* VOID_TYPE_P (TREE_TYPE (x)) is workaround for cp/tree.c
     (lvalue_p) crash on TRY/CATCH. */
  if (TREE_TYPE (x) == NULL_TREE || VOID_TYPE_P (TREE_TYPE (x)))
    return false;

  if (!lvalue_p (x))
    return false;

  /* No point to track non-const calls, they will never satisfy
     operand_equal_p.  */
  if (TREE_CODE (x) == CALL_EXPR && (call_expr_flags (x) & ECF_CONST) == 0)
    return false;

  if (TREE_CODE (x) == STRING_CST)
    return false;

  return true;
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
    case SIZEOF_EXPR:
    case PAREN_SIZEOF_EXPR:
      return;

    case COMPOUND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    sequenced_binary:
      tmp_before = tmp_nosp = tmp_list2 = tmp_list3 = 0;
      verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_nosp, NULL_TREE);
      warn_for_collisions (tmp_nosp);
      merge_tlist (pbefore_sp, tmp_before, 0);
      merge_tlist (pbefore_sp, tmp_nosp, 0);
      verify_tree (TREE_OPERAND (x, 1), &tmp_list3, &tmp_list2, NULL_TREE);
      warn_for_collisions (tmp_list2);
      merge_tlist (pbefore_sp, tmp_list3, 0);
      merge_tlist (pno_sp, tmp_list2, 0);
      return;

    case COND_EXPR:
      tmp_before = tmp_list2 = 0;
      verify_tree (TREE_OPERAND (x, 0), &tmp_before, &tmp_list2, NULL_TREE);
      warn_for_collisions (tmp_list2);
      merge_tlist (pbefore_sp, tmp_before, 0);
      merge_tlist (pbefore_sp, tmp_list2, 0);

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
	    merge_tlist (&tmp_list3, tmp_nosp, 0);
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

    case VIEW_CONVERT_EXPR:
      if (location_wrapper_p (x))
	{
	  x = TREE_OPERAND (x, 0);
	  goto restart;
	}
      goto do_default;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case COMPONENT_REF:
    case ARRAY_REF:
      if (cxx_dialect >= cxx17)
	goto sequenced_binary;
      goto do_default;

    default:
    do_default:
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

static constexpr size_t verify_sequence_points_limit = 1024;

/* Called from verify_sequence_points via walk_tree.  */

static tree
verify_tree_lim_r (tree *tp, int *walk_subtrees, void *data)
{
  if (++*((size_t *) data) > verify_sequence_points_limit)
    return integer_zero_node;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Try to warn for undefined behavior in EXPR due to missing sequence
   points.  */

void
verify_sequence_points (tree expr)
{
  tlist *before_sp = nullptr, *after_sp = nullptr;

  /* verify_tree is highly recursive, and merge_tlist is O(n^2),
     so we return early if the expression is too big.  */
  size_t n = 0;
  if (walk_tree (&expr, verify_tree_lim_r, &n, nullptr))
    return;

  warned_ids = nullptr;
  save_expr_cache = nullptr;
  if (!tlist_firstobj)
    {
      gcc_obstack_init (&tlist_obstack);
      tlist_firstobj = (char *) obstack_alloc (&tlist_obstack, 0);
    }

  verify_tree (expr, &before_sp, &after_sp, NULL_TREE);
  warn_for_collisions (after_sp);
  obstack_free (&tlist_obstack, tlist_firstobj);
}

/* Validate the expression after `case' and apply default promotions.  */

static tree
check_case_value (location_t loc, tree value)
{
  if (value == NULL_TREE)
    return value;

  if (TREE_CODE (value) == INTEGER_CST)
    /* Promote char or short to int.  */
    value = perform_integral_promotions (value);
  else if (value != error_mark_node)
    {
      error_at (loc, "case label does not reduce to an integer constant");
      value = error_mark_node;
    }

  constant_expression_warning (value);

  return value;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
c_common_type_for_size (unsigned int bits, int unsignedp)
{
  int i;

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

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& bits == int_n_data[i].bitsize)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

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

  return NULL_TREE;
}

/* Return a fixed-point type that has at least IBIT ibits and FBIT fbits
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed;
   and saturating if SATP is nonzero, otherwise not saturating.  */

tree
c_common_fixed_point_type_for_size (unsigned int ibit, unsigned int fbit,
				    int unsignedp, int satp)
{
  enum mode_class mclass;
  if (ibit == 0)
    mclass = unsignedp ? MODE_UFRACT : MODE_FRACT;
  else
    mclass = unsignedp ? MODE_UACCUM : MODE_ACCUM;

  opt_scalar_mode opt_mode;
  scalar_mode mode;
  FOR_EACH_MODE_IN_CLASS (opt_mode, mclass)
    {
      mode = opt_mode.require ();
      if (GET_MODE_IBIT (mode) >= ibit && GET_MODE_FBIT (mode) >= fbit)
	break;
    }

  if (!opt_mode.exists (&mode) || !targetm.scalar_mode_supported_p (mode))
    {
      sorry ("GCC cannot support operators with integer types and "
	     "fixed-point types that have too many integral and "
	     "fractional bits together");
      return NULL_TREE;
    }

  return c_common_type_for_mode (mode, satp);
}

/* Used for communication between c_common_type_for_mode and
   c_register_builtin_type.  */
tree registered_builtin_types;

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.
   If the mode is a fixed-point mode,
   then UNSIGNEDP selects between saturating and nonsaturating types.  */

tree
c_common_type_for_mode (machine_mode mode, int unsignedp)
{
  tree t;
  int i;

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

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& mode == int_n_data[i].m)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

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

  for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    if (FLOATN_NX_TYPE_NODE (i) != NULL_TREE
	&& mode == TYPE_MODE (FLOATN_NX_TYPE_NODE (i)))
      return FLOATN_NX_TYPE_NODE (i);

  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node))
      || mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    {
      unsigned int precision
	= GET_MODE_PRECISION (as_a <scalar_int_mode> (mode));
      return (unsignedp
	      ? make_unsigned_type (precision)
	      : make_signed_type (precision));
    }

  if (COMPLEX_MODE_P (mode))
    {
      machine_mode inner_mode;
      tree inner_type;

      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;

      for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
	if (COMPLEX_FLOATN_NX_TYPE_NODE (i) != NULL_TREE
	    && mode == TYPE_MODE (COMPLEX_FLOATN_NX_TYPE_NODE (i)))
	  return COMPLEX_FLOATN_NX_TYPE_NODE (i);

      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;

      inner_mode = GET_MODE_INNER (mode);
      inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_complex_type (inner_type);
    }
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
	   && valid_vector_subparts_p (GET_MODE_NUNITS (mode)))
    {
      unsigned int elem_bits = vector_element_size (GET_MODE_BITSIZE (mode),
						    GET_MODE_NUNITS (mode));
      tree bool_type = build_nonstandard_boolean_type (elem_bits);
      return build_vector_type_for_mode (bool_type, mode);
    }
  else if (VECTOR_MODE_P (mode)
	   && valid_vector_subparts_p (GET_MODE_NUNITS (mode)))
    {
      machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_vector_type_for_mode (inner_type, mode);
    }

  if (dfloat32_type_node != NULL_TREE
      && mode == TYPE_MODE (dfloat32_type_node))
    return dfloat32_type_node;
  if (dfloat64_type_node != NULL_TREE
      && mode == TYPE_MODE (dfloat64_type_node))
    return dfloat64_type_node;
  if (dfloat128_type_node != NULL_TREE
      && mode == TYPE_MODE (dfloat128_type_node))
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
    {
      tree type = TREE_VALUE (t);
      if (TYPE_MODE (type) == mode
	  && VECTOR_TYPE_P (type) == VECTOR_MODE_P (mode)
	  && !!unsignedp == !!TYPE_UNSIGNED (type))
	return type;
    }
  return NULL_TREE;
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
  int i;

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

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& (type1 == int_n_trees[i].unsigned_type
	    || type1 == int_n_trees[i].signed_type))
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

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

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& TYPE_MODE (type) == int_n_data[i].m
	&& TYPE_PRECISION (type) == int_n_data[i].bitsize)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

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
  int i;

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
  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& width == int_n_data[i].bitsize)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);
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
  lang_hooks.decls.pushdecl (decl);

  registered_builtin_types = tree_cons (0, type, registered_builtin_types);
}

/* Print an error message for invalid operands to arith operation
   CODE with TYPE0 for operand 0, and TYPE1 for operand 1.
   RICHLOC is a rich location for the message, containing either
   three separate locations for each of the operator and operands

      lhs op rhs
      ~~~ ^~ ~~~

   (C FE), or one location ranging over all over them

      lhs op rhs
      ~~~~^~~~~~

   (C++ FE).  */

void
binary_op_error (rich_location *richloc, enum tree_code code,
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
  error_at (richloc,
	    "invalid operands to binary %s (have %qT and %qT)",
	    opname, type0, type1);
}

/* Given an expression as a tree, return its original type.  Do this
   by stripping any conversion that preserves the sign and precision.  */
static tree
expr_original_type (tree expr)
{
  STRIP_SIGN_NOPS (expr);
  return TREE_TYPE (expr);
}

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.
   This function is also responsible for converting the two operands
   to the proper common type for comparison.

   The arguments of this function are all pointers to local variables
   of build_binary_op: OP0_PTR is &OP0, OP1_PTR is &OP1,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   LOC is the location of the comparison.

   If this function returns non-NULL_TREE, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

tree
shorten_compare (location_t loc, tree *op0_ptr, tree *op1_ptr,
		 tree *restype_ptr, enum tree_code *rescode_ptr)
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

  primop0 = c_common_get_narrower (op0, &unsignedp0);
  primop1 = c_common_get_narrower (op1, &unsignedp1);

  /* If primopN is first sign-extended from primopN's precision to opN's
     precision, then zero-extended from opN's precision to
     *restype_ptr precision, shortenings might be invalid.  */
  if (TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (TREE_TYPE (op0))
      && TYPE_PRECISION (TREE_TYPE (op0)) < TYPE_PRECISION (*restype_ptr)
      && !unsignedp0
      && TYPE_UNSIGNED (TREE_TYPE (op0)))
    primop0 = op0;
  if (TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (TREE_TYPE (op1))
      && TYPE_PRECISION (TREE_TYPE (op1)) < TYPE_PRECISION (*restype_ptr)
      && !unsignedp1
      && TYPE_UNSIGNED (TREE_TYPE (op1)))
    primop1 = op1;

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
      std::swap (primop0, primop1);
      std::swap (op0, op1);
      *op0_ptr = op0;
      *op1_ptr = op1;
      std::swap (unsignedp0, unsignedp1);
      std::swap (real1, real2);

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
	  primop1 = force_fit_type (*restype_ptr,
				    wi::to_wide
				     (primop1,
				      TYPE_PRECISION (*restype_ptr)),
				    0, TREE_OVERFLOW (primop1));
	}
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      min_gt = tree_int_cst_lt (primop1, minval);
      max_gt = tree_int_cst_lt (primop1, maxval);
      min_lt = tree_int_cst_lt (minval, primop1);
      max_lt = tree_int_cst_lt (maxval, primop1);

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

      if (TREE_CODE (primop0) != INTEGER_CST
	  /* Don't warn if it's from a (non-system) macro.  */
	  && !(from_macro_expansion_at
	       (expansion_point_location_if_in_system_header
		(EXPR_LOCATION (primop0)))))
	{
	  if (val == truthvalue_false_node)
	    warning_at (loc, OPT_Wtype_limits,
			"comparison is always false due to limited range of data type");
	  if (val == truthvalue_true_node)
	    warning_at (loc, OPT_Wtype_limits,
			"comparison is always true due to limited range of data type");
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
	   && DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop0)))
	   && DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop1))))
    type = common_type (TREE_TYPE (primop0), TREE_TYPE (primop1));

  /* If either arg is decimal float and the other is float, fail.  */
  else if (real1 && real2
	   && (DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop0)))
	       || DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (primop1)))))
    return NULL_TREE;

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

      /* We want to fold unsigned comparisons of >= and < against zero.
	 For these, we may also issue a warning if we have a non-constant
	 compared against zero, where the zero was spelled as "0" (rather
	 than merely folding to it).
	 If we have at least one constant, then op1 is constant
	 and we may have a non-constant expression as op0.  */
      if (!real1 && !real2 && integer_zerop (primop1)
	  && TYPE_UNSIGNED (*restype_ptr))
	{
	  tree value = NULL_TREE;
	  /* All unsigned values are >= 0, so we warn.  However,
	     if OP0 is a constant that is >= 0, the signedness of
	     the comparison isn't an issue, so suppress the
	     warning.  */
	  tree folded_op0 = fold_for_warn (op0);
	  bool warn = 
	    warn_type_limits && !in_system_header_at (loc)
	    && !(TREE_CODE (folded_op0) == INTEGER_CST
		 && !TREE_OVERFLOW (convert (c_common_signed_type (type),
					     folded_op0)))
	    /* Do not warn for enumeration types.  */
	    && (TREE_CODE (expr_original_type (folded_op0)) != ENUMERAL_TYPE);
	  
	  switch (code)
	    {
	    case GE_EXPR:
	      if (warn)
		warning_at (loc, OPT_Wtype_limits,
			    "comparison of unsigned expression in %<>= 0%> "
			    "is always true");
	      value = truthvalue_true_node;
	      break;

	    case LT_EXPR:
	      if (warn)
		warning_at (loc, OPT_Wtype_limits,
			    "comparison of unsigned expression in %<< 0%> "
			    "is always false");
	      value = truthvalue_false_node;
	      break;

	    default:
	      break;
	    }

	  if (value != NULL_TREE)
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

  return NULL_TREE;
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

tree
pointer_int_sum (location_t loc, enum tree_code resultcode,
		 tree ptrop, tree intop, bool complain)
{
  tree size_exp, ret;

  /* The result is a pointer of the same type that is being added.  */
  tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (complain && warn_pointer_arith)
	pedwarn (loc, OPT_Wpointer_arith,
		 "pointer of type %<void *%> used in arithmetic");
      else if (!complain)
	return error_mark_node;
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (complain && warn_pointer_arith)
	pedwarn (loc, OPT_Wpointer_arith,
		 "pointer to a function used in arithmetic");
      else if (!complain)
	return error_mark_node;
      size_exp = integer_one_node;
    }
  else if (!verify_type_context (loc, TCTX_POINTER_ARITH,
				 TREE_TYPE (result_type)))
    size_exp = integer_one_node;
  else
    size_exp = size_in_bytes_loc (loc, TREE_TYPE (result_type));

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
      && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (intop))
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
			       convert (int_type, TREE_OPERAND (intop, 1)),
			       true);
      intop = convert (int_type, TREE_OPERAND (intop, 0));
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */
  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype)
      || TYPE_UNSIGNED (TREE_TYPE (intop)) != TYPE_UNSIGNED (sizetype))
    intop = convert (c_common_type_for_size (TYPE_PRECISION (sizetype),
					     TYPE_UNSIGNED (sizetype)), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate type
     for the pointer operation and disregard an overflow that occurred only
     because of the sign-extension change in the latter conversion.  */
  {
    tree t = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (intop), intop,
			      convert (TREE_TYPE (intop), size_exp));
    intop = convert (sizetype, t);
    if (TREE_OVERFLOW_P (intop) && !TREE_OVERFLOW (t))
      intop = wide_int_to_tree (TREE_TYPE (intop), wi::to_wide (intop));
  }

  /* Create the sum or difference.  */
  if (resultcode == MINUS_EXPR)
    intop = fold_build1_loc (loc, NEGATE_EXPR, sizetype, intop);

  ret = fold_build_pointer_plus_loc (loc, ptrop, intop);

  fold_undefer_and_ignore_overflow_warnings ();

  return ret;
}

/* Wrap a C_MAYBE_CONST_EXPR around an expression that is fully folded
   and if NON_CONST is known not to be permitted in an evaluated part
   of a constant expression.  */

tree
c_wrap_maybe_const (tree expr, bool non_const)
{
  bool nowarning = TREE_NO_WARNING (expr);
  location_t loc = EXPR_LOCATION (expr);

  /* This should never be called for C++.  */
  if (c_dialect_cxx ())
    gcc_unreachable ();

  /* The result of folding may have a NOP_EXPR to set TREE_NO_WARNING.  */
  STRIP_TYPE_NOPS (expr);
  expr = build2 (C_MAYBE_CONST_EXPR, TREE_TYPE (expr), NULL, expr);
  C_MAYBE_CONST_EXPR_NON_CONST (expr) = non_const;
  if (nowarning)
    TREE_NO_WARNING (expr) = 1;
  protected_set_expr_location (expr, loc);

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
  STRIP_ANY_LOCATION_WRAPPER (expr);
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
      if (TREE_CODE (TREE_TYPE (expr)) == ENUMERAL_TYPE
	  && !integer_zerop (expr)
	  && !integer_onep (expr))
	warning_at (location, OPT_Wint_in_bool_context,
		    "enum constant in boolean context");
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
      expr = build_unary_op (location, ADDR_EXPR, expr, false);
      /* Fall through.  */

    case ADDR_EXPR:
      {
 	tree inner = TREE_OPERAND (expr, 0);
	if (decl_with_nonnull_addr_p (inner))
	  {
	    /* Common Ada programmer's mistake.  */
	    warning_at (location,
			OPT_Waddress,
			"the address of %qD will always evaluate as %<true%>",
			inner);
	    return truthvalue_true_node;
	  }
	break;
      }

    case COMPLEX_EXPR:
      expr = build_binary_op (EXPR_LOCATION (expr),
			      (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			       ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
		c_common_truthvalue_conversion (location,
						TREE_OPERAND (expr, 0)),
		c_common_truthvalue_conversion (location,
						TREE_OPERAND (expr, 1)),
			      false);
      goto ret;

    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
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

    case MULT_EXPR:
      warning_at (EXPR_LOCATION (expr), OPT_Wint_in_bool_context,
		  "%<*%> in boolean context, suggest %<&&%> instead");
      break;

    case LSHIFT_EXPR:
      /* We will only warn on signed shifts here, because the majority of
	 false positive warnings happen in code where unsigned arithmetic
	 was used in anticipation of a possible overflow.
	 Furthermore, if we see an unsigned type here we know that the
	 result of the shift is not subject to integer promotion rules.  */
      if (TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
	  && !TYPE_UNSIGNED (TREE_TYPE (expr)))
	warning_at (EXPR_LOCATION (expr), OPT_Wint_in_bool_context,
		    "%<<<%> in boolean context, did you mean %<<%>?");
      break;

    case COND_EXPR:
      if (warn_int_in_bool_context
	  && !from_macro_definition_at (EXPR_LOCATION (expr)))
	{
	  tree val1 = fold_for_warn (TREE_OPERAND (expr, 1));
	  tree val2 = fold_for_warn (TREE_OPERAND (expr, 2));
	  if (TREE_CODE (val1) == INTEGER_CST
	      && TREE_CODE (val2) == INTEGER_CST
	      && !integer_zerop (val1)
	      && !integer_zerop (val2)
	      && (!integer_onep (val1)
		  || !integer_onep (val2)))
	    warning_at (EXPR_LOCATION (expr), OPT_Wint_in_bool_context,
			"%<?:%> using integer constants in boolean context, "
			"the expression will always evaluate to %<true%>");
	  else if ((TREE_CODE (val1) == INTEGER_CST
		    && !integer_zerop (val1)
		    && !integer_onep (val1))
		   || (TREE_CODE (val2) == INTEGER_CST
		       && !integer_zerop (val2)
		       && !integer_onep (val2)))
	    warning_at (EXPR_LOCATION (expr), OPT_Wint_in_bool_context,
			"%<?:%> using integer constants in boolean context");
	}
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      if (c_dialect_cxx ())
	/* Avoid premature folding.  */
	break;
      else
	{
	  int w = warn_int_in_bool_context;
	  warn_int_in_bool_context = 0;
	  /* Folding will happen later for C.  */
	  expr = build3 (COND_EXPR, truthvalue_type_node,
			 TREE_OPERAND (expr, 0),
			 c_common_truthvalue_conversion (location,
							 TREE_OPERAND (expr, 1)),
			 c_common_truthvalue_conversion (location,
							 TREE_OPERAND (expr, 2)));
	  warn_int_in_bool_context = w;
	  goto ret;
	}

    CASE_CONVERT:
      {
	tree totype = TREE_TYPE (expr);
	tree fromtype = TREE_TYPE (TREE_OPERAND (expr, 0));

	if (POINTER_TYPE_P (totype)
	    && !c_inhibit_evaluation_warnings
	    && TREE_CODE (fromtype) == REFERENCE_TYPE)
	  {
	    tree inner = expr;
	    STRIP_NOPS (inner);

	    if (DECL_P (inner))
	      warning_at (location,
			  OPT_Waddress,
			  "the compiler can assume that the address of "
			  "%qD will always evaluate to %<true%>",
			  inner);
	  }

	/* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	   since that affects how `default_conversion' will behave.  */
	if (TREE_CODE (totype) == REFERENCE_TYPE
	    || TREE_CODE (fromtype) == REFERENCE_TYPE)
	  break;
	/* Don't strip a conversion from C++0x scoped enum, since they
	   don't implicitly convert to other types.  */
	if (TREE_CODE (fromtype) == ENUMERAL_TYPE
	    && ENUM_IS_SCOPED (fromtype))
	  break;
	/* If this isn't narrowing the argument, we can ignore it.  */
	if (TYPE_PRECISION (totype) >= TYPE_PRECISION (fromtype))
	  return c_common_truthvalue_conversion (location,
						 TREE_OPERAND (expr, 0));
      }
      break;

    case MODIFY_EXPR:
      if (!TREE_NO_WARNING (expr)
	  && warn_parentheses
	  && warning_at (location, OPT_Wparentheses,
			 "suggest parentheses around assignment used as "
			 "truth value"))
	TREE_NO_WARNING (expr) = 1;
      break;

    case CONST_DECL:
      {
	tree folded_expr = fold_for_warn (expr);
	if (folded_expr != expr)
	  return c_common_truthvalue_conversion (location, folded_expr);
      }
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree t = save_expr (expr);
      expr = (build_binary_op
	      (EXPR_LOCATION (expr),
	       (TREE_SIDE_EFFECTS (expr)
		? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	c_common_truthvalue_conversion
	       (location,
		build_unary_op (location, REALPART_EXPR, t, false)),
	c_common_truthvalue_conversion
	       (location,
		build_unary_op (location, IMAGPART_EXPR, t, false)),
	       false));
      goto ret;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == FIXED_POINT_TYPE)
    {
      tree fixed_zero_node = build_fixed (TREE_TYPE (expr),
					  FCONST0 (TYPE_MODE
						   (TREE_TYPE (expr))));
      return build_binary_op (location, NE_EXPR, expr, fixed_zero_node, true);
    }
  else
    return build_binary_op (location, NE_EXPR, expr, integer_zero_node, true);

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

  if ((type_quals & TYPE_QUAL_CONST)
      || (type && TREE_CODE (type) == REFERENCE_TYPE))
    /* We used to check TYPE_NEEDS_CONSTRUCTING here, but now a constexpr
       constructor can produce constant init, so rely on cp_finish_decl to
       clear TREE_READONLY if the variable has non-constant init.  */
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

/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

alias_set_type
c_common_get_alias_set (tree t)
{
  /* For VLAs, use the alias set of the element type rather than the
     default of alias set 0 for types compared structurally.  */
  if (TYPE_P (t) && TYPE_STRUCTURAL_EQUALITY_P (t))
    {
      if (TREE_CODE (t) == ARRAY_TYPE)
	return get_alias_set (TREE_TYPE (t));
      return -1;
    }

  /* That's all the expressions we handle specially.  */
  if (!TYPE_P (t))
    return -1;

  /* Unlike char, char8_t doesn't alias. */
  if (flag_char8_t && t == char8_type_node)
    return -1;

  /* The C standard guarantees that any object may be accessed via an
     lvalue that has narrow character type (except char8_t).  */
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

  return -1;
}

/* Compute the value of 'sizeof (TYPE)' or '__alignof__ (TYPE)', where
   the IS_SIZEOF parameter indicates which operator is being applied.
   The COMPLAIN flag controls whether we should diagnose possibly
   ill-formed constructs or not.  LOC is the location of the SIZEOF or
   TYPEOF operator.  If MIN_ALIGNOF, the least alignment required for
   a type in any context should be returned, rather than the normal
   alignment for that type.  */

tree
c_sizeof_or_alignof_type (location_t loc,
			  tree type, bool is_sizeof, bool min_alignof,
			  int complain)
{
  const char *op_name;
  tree value = NULL;
  enum tree_code type_code = TREE_CODE (type);

  op_name = is_sizeof ? "sizeof" : "__alignof__";

  if (type_code == FUNCTION_TYPE)
    {
      if (is_sizeof)
	{
	  if (complain && warn_pointer_arith)
	    pedwarn (loc, OPT_Wpointer_arith,
		     "invalid application of %<sizeof%> to a function type");
          else if (!complain)
            return error_mark_node;
	  value = size_one_node;
	}
      else
	{
	  if (complain)
	    {
	      if (c_dialect_cxx ())
		pedwarn (loc, OPT_Wpedantic, "ISO C++ does not permit "
			 "%<alignof%> applied to a function type");
	      else
		pedwarn (loc, OPT_Wpedantic, "ISO C does not permit "
			 "%<_Alignof%> applied to a function type");
	    }
	  value = size_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);
	}
    }
  else if (type_code == VOID_TYPE || type_code == ERROR_MARK)
    {
      if (type_code == VOID_TYPE
	  && complain && warn_pointer_arith)
	pedwarn (loc, OPT_Wpointer_arith,
		 "invalid application of %qs to a void type", op_name);
      else if (!complain)
        return error_mark_node;
      value = size_one_node;
    }
  else if (!COMPLETE_TYPE_P (type)
	   && (!c_dialect_cxx () || is_sizeof || type_code != ARRAY_TYPE))
    {
      if (complain)
	error_at (loc, "invalid application of %qs to incomplete type %qT",
		  op_name, type);
      return error_mark_node;
    }
  else if (c_dialect_cxx () && type_code == ARRAY_TYPE
	   && !COMPLETE_TYPE_P (TREE_TYPE (type)))
    {
      if (complain)
	error_at (loc, "invalid application of %qs to array type %qT of "
		  "incomplete element type", op_name, type);
      return error_mark_node;
    }
  else if (!verify_type_context (loc, is_sizeof ? TCTX_SIZEOF : TCTX_ALIGNOF,
				 type, !complain))
    {
      if (!complain)
	return error_mark_node;
      value = size_one_node;
    }
  else
    {
      if (is_sizeof)
	/* Convert in case a char is more than one unit.  */
	value = size_binop_loc (loc, CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
				size_int (TYPE_PRECISION (char_type_node)
					  / BITS_PER_UNIT));
      else if (min_alignof)
	value = size_int (min_align_of_type (type));
      else
	value = size_int (TYPE_ALIGN_UNIT (type));
    }

  /* VALUE will have the middle-end integer type sizetype.
     However, we should really return a value of type `size_t',
     which is just a typedef for an ordinary integer type.  */
  value = fold_convert_loc (loc, size_type_node, value);

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

  if (!verify_type_context (loc, TCTX_ALIGNOF, TREE_TYPE (expr)))
    t = size_one_node;

  else if (VAR_OR_FUNCTION_DECL_P (expr))
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

  else if (INDIRECT_REF_P (expr))
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
#define DEF_ATTR_STRING(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
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
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10) NAME,
#define DEF_FUNCTION_TYPE_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
				NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7) NAME,
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
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
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
  tree t;
  tree *args = XALLOCAVEC (tree, n);
  va_list list;
  int i;

  va_start (list, n);
  for (i = 0; i < n; ++i)
    {
      builtin_type a = (builtin_type) va_arg (list, int);
      t = builtin_types[a];
      if (t == error_mark_node)
	goto egress;
      args[i] = t;
    }

  t = builtin_types[ret];
  if (t == error_mark_node)
    goto egress;
  if (var)
    t = build_varargs_function_type_array (t, n, args);
  else
    t = build_function_type_array (t, n, args);

 egress:
  builtin_types[def] = t;
  va_end (list);
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
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
  def_fn_type (ENUM, RETURN, 0, 8, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	\
	       ARG7, ARG8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
  def_fn_type (ENUM, RETURN, 0, 9, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	\
	       ARG7, ARG8, ARG9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10)		 \
  def_fn_type (ENUM, RETURN, 0, 10, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	 \
	       ARG7, ARG8, ARG9, ARG10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
  def_fn_type (ENUM, RETURN, 0, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	 \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
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
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6) \
  def_fn_type (ENUM, RETURN, 1, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				\
  def_fn_type (ENUM, RETURN, 1, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
  builtin_types[(int) ENUM] = build_pointer_type (builtin_types[(int) TYPE]);

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
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
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

  targetm.init_builtins ();

  build_common_builtin_nodes ();
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
  int char8_type_size;
  int char16_type_size;
  int char32_type_size;
  int wchar_type_size;
  tree array_domain_type;
  tree va_list_ref_type_node;
  tree va_list_arg_type_node;
  int i;

  build_common_tree_nodes (flag_signed_char);

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

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      char name[25];

      sprintf (name, "__int%d", int_n_data[i].bitsize);
      record_builtin_type ((enum rid)(RID_FIRST_INT_N + i), name,
			   int_n_trees[i].signed_type);
      sprintf (name, "__int%d__", int_n_data[i].bitsize);
      record_builtin_type ((enum rid)(RID_FIRST_INT_N + i), name,
			   int_n_trees[i].signed_type);

      sprintf (name, "__int%d unsigned", int_n_data[i].bitsize);
      record_builtin_type (RID_MAX, name, int_n_trees[i].unsigned_type);
      sprintf (name, "__int%d__ unsigned", int_n_data[i].bitsize);
      record_builtin_type (RID_MAX, name, int_n_trees[i].unsigned_type);
    }

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
  /* Note that this is different than the __int128 type that's part of
     the generic __intN support.  */
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
  if (targetm.scalar_mode_supported_p (TImode))
    {
      widest_integer_literal_type_node = intTI_type_node;
      widest_unsigned_literal_type_node = unsigned_intTI_type_node;
    }
  else
    {
      widest_integer_literal_type_node = intDI_type_node;
      widest_unsigned_literal_type_node = unsigned_intDI_type_node;
    }

  signed_size_type_node = c_common_signed_type (size_type_node);

  pid_type_node =
    TREE_TYPE (identifier_global_value (get_identifier (PID_TYPE)));

  record_builtin_type (RID_FLOAT, NULL, float_type_node);
  record_builtin_type (RID_DOUBLE, NULL, double_type_node);
  record_builtin_type (RID_MAX, "long double", long_double_type_node);

  if (!c_dialect_cxx ())
    for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
      if (FLOATN_NX_TYPE_NODE (i) != NULL_TREE)
	record_builtin_type ((enum rid) (RID_FLOATN_NX_FIRST + i), NULL,
			     FLOATN_NX_TYPE_NODE (i));

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

  if (!c_dialect_cxx ())
    for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
      if (COMPLEX_FLOATN_NX_TYPE_NODE (i) != NULL_TREE)
	{
	  char buf[30];
	  sprintf (buf, "complex _Float%d%s", floatn_nx_types[i].n,
		   floatn_nx_types[i].extended ? "x" : "");
	  lang_hooks.decls.pushdecl
	    (build_decl (UNKNOWN_LOCATION,
			 TYPE_DECL,
			 get_identifier (buf),
			 COMPLEX_FLOATN_NX_TYPE_NODE (i)));
	}

  /* Make fileptr_type_node a distinct void * type until
     FILE type is defined.  Likewise for const struct tm*.  */
  for (unsigned i = 0;
       i < sizeof (builtin_structptr_types) / sizeof (builtin_structptr_type);
       ++i)
    builtin_structptr_types[i].node
      = build_variant_type_copy (builtin_structptr_types[i].base);

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

  /* Define 'char8_t'.  */
  char8_type_node = get_identifier (CHAR8_TYPE);
  char8_type_node = TREE_TYPE (identifier_global_value (char8_type_node));
  char8_type_size = TYPE_PRECISION (char8_type_node);
  if (c_dialect_cxx ())
    {
      char8_type_node = make_unsigned_type (char8_type_size);

      if (flag_char8_t)
        record_builtin_type (RID_CHAR8, "char8_t", char8_type_node);
    }

  /* This is for UTF-8 string constants.  */
  char8_array_type_node
    = build_array_type (char8_type_node, array_domain_type);

  /* Define 'char16_t'.  */
  char16_type_node = get_identifier (CHAR16_TYPE);
  char16_type_node = TREE_TYPE (identifier_global_value (char16_type_node));
  char16_type_size = TYPE_PRECISION (char16_type_node);
  if (c_dialect_cxx ())
    {
      char16_type_node = make_unsigned_type (char16_type_size);

      if (cxx_dialect >= cxx11)
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

      if (cxx_dialect >= cxx11)
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
    c_uint16_type_node = uint16_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT16_TYPE)));
  if (UINT32_TYPE)
    c_uint32_type_node = uint32_type_node =
      TREE_TYPE (identifier_global_value (c_get_ident (UINT32_TYPE)));
  if (UINT64_TYPE)
    c_uint64_type_node = uint64_type_node =
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

  default_function_type
    = build_varargs_function_type_list (integer_type_node, NULL_TREE);
  unsigned_ptrdiff_type_node = c_common_unsigned_type (ptrdiff_type_node);

  lang_hooks.decls.pushdecl
    (build_decl (UNKNOWN_LOCATION,
		 TYPE_DECL, get_identifier ("__builtin_va_list"),
		 va_list_type_node));
  if (targetm.enum_va_list_p)
    {
      int l;
      const char *pname;
      tree ptype;

      for (l = 0; targetm.enum_va_list_p (l, &pname, &ptype); ++l)
	{
	  lang_hooks.decls.pushdecl
	    (build_decl (UNKNOWN_LOCATION,
		         TYPE_DECL, get_identifier (pname),
	  	         ptype));

	}
    }

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

  c_define_builtins (va_list_ref_type_node, va_list_arg_type_node);

  main_identifier_node = get_identifier ("main");

  /* Create the built-in __null node.  It is important that this is
     not shared.  */
  null_node = make_int_cst (1, 1);
  TREE_TYPE (null_node) = c_common_type_for_size (POINTER_SIZE, 0);

  /* Since builtin_types isn't gc'ed, don't export these nodes.  */
  memset (builtin_types, 0, sizeof (builtin_types));
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

/* build_va_arg helper function.  Return a VA_ARG_EXPR with location LOC, type
   TYPE and operand OP.  */

static tree
build_va_arg_1 (location_t loc, tree type, tree op)
{
  tree expr = build1 (VA_ARG_EXPR, type, op);
  SET_EXPR_LOCATION (expr, loc);
  return expr;
}

/* Return a VA_ARG_EXPR corresponding to a source-level expression
   va_arg (EXPR, TYPE) at source location LOC.  */

tree
build_va_arg (location_t loc, tree expr, tree type)
{
  tree va_type = TREE_TYPE (expr);
  tree canon_va_type = (va_type == error_mark_node
			? error_mark_node
			: targetm.canonical_va_list_type (va_type));

  if (va_type == error_mark_node
      || canon_va_type == NULL_TREE)
    {
      if (canon_va_type == NULL_TREE)
	error_at (loc, "first argument to %<va_arg%> not of type %<va_list%>");

      /* Let's handle things neutrally, if expr:
	 - has undeclared type, or
	 - is not an va_list type.  */
      return build_va_arg_1 (loc, type, error_mark_node);
    }

  if (TREE_CODE (canon_va_type) != ARRAY_TYPE)
    {
      /* Case 1: Not an array type.  */

      /* Take the address, to get '&ap'.  Note that &ap is not a va_list
	 type.  */
      c_common_mark_addressable_vec (expr);
      expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (expr)), expr);

      return build_va_arg_1 (loc, type, expr);
    }

  /* Case 2: Array type.

     Background:

     For contrast, let's start with the simple case (case 1).  If
     canon_va_type is not an array type, but say a char *, then when
     passing-by-value a va_list, the type of the va_list param decl is
     the same as for another va_list decl (all ap's are char *):

     f2_1 (char * ap)
       D.1815 = VA_ARG (&ap, 0B, 1);
       return D.1815;

     f2 (int i)
       char * ap.0;
       char * ap;
       __builtin_va_start (&ap, 0);
       ap.0 = ap;
       res = f2_1 (ap.0);
       __builtin_va_end (&ap);
       D.1812 = res;
       return D.1812;

     However, if canon_va_type is ARRAY_TYPE, then when passing-by-value a
     va_list the type of the va_list param decl (case 2b, struct * ap) is not
     the same as for another va_list decl (case 2a, struct ap[1]).

     f2_1 (struct  * ap)
       D.1844 = VA_ARG (ap, 0B, 0);
       return D.1844;

     f2 (int i)
       struct  ap[1];
       __builtin_va_start (&ap, 0);
       res = f2_1 (&ap);
       __builtin_va_end (&ap);
       D.1841 = res;
       return D.1841;

     Case 2b is different because:
     - on the callee side, the parm decl has declared type va_list, but
       grokdeclarator changes the type of the parm decl to a pointer to the
       array elem type.
     - on the caller side, the pass-by-value uses &ap.

     We unify these two cases (case 2a: va_list is array type,
     case 2b: va_list is pointer to array elem type), by adding '&' for the
     array type case, such that we have a pointer to array elem in both
     cases.  */

  if (TREE_CODE (va_type) == ARRAY_TYPE)
    {
      /* Case 2a: va_list is array type.  */

      /* Take the address, to get '&ap'.  Make sure it's a pointer to array
	 elem type.  */
      c_common_mark_addressable_vec (expr);
      expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (canon_va_type)),
		     expr);

      /* Verify that &ap is still recognized as having va_list type.  */
      tree canon_expr_type
	= targetm.canonical_va_list_type (TREE_TYPE (expr));
      gcc_assert (canon_expr_type != NULL_TREE);
    }
  else
    {
      /* Case 2b: va_list is pointer to array elem type.  */
      gcc_assert (POINTER_TYPE_P (va_type));

      /* Comparison as in std_canonical_va_list_type.  */
      gcc_assert (TYPE_MAIN_VARIANT (TREE_TYPE (va_type))
		  == TYPE_MAIN_VARIANT (TREE_TYPE (canon_va_type)));

      /* Don't take the address.  We've already got '&ap'.  */
      ;
    }

  return build_va_arg_1 (loc, type, expr);
}


/* Linked list of disabled built-in functions.  */

struct disabled_builtin
{
  const char *name;
  struct disabled_builtin *next;
};
static disabled_builtin *disabled_builtins = NULL;

static bool builtin_function_disabled_p (const char *);

/* Disable a built-in function specified by -fno-builtin-NAME.  If NAME
   begins with "__builtin_", give an error.  */

void
disable_builtin_function (const char *name)
{
  if (startswith (name, "__builtin_"))
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
	      || startswith (name, "__builtin_"));

  libname = name + strlen ("__builtin_");
  decl = add_builtin_function (name, fntype, fncode, fnclass,
			       (fallback_p ? libname : NULL),
			       fnattrs);

  set_builtin_decl (fncode, decl, implicit_p);

  if (both_p
      && !flag_no_builtin && !builtin_function_disabled_p (libname)
      && !(nonansi_p && flag_no_nonansi_builtin))
    add_builtin_function (libname, libtype, fncode, fnclass,
			  NULL, fnattrs);
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
      return true;

    default:
      return false;
    }
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

bool
self_promoting_args_p (const_tree parms)
{
  const_tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);

      if (type == error_mark_node)
	continue;

      if (TREE_CHAIN (t) == NULL_TREE && type != void_type_node)
	return false;

      if (type == NULL_TREE)
	return false;

      if (TYPE_MAIN_VARIANT (type) == float_type_node)
	return false;

      if (c_promoting_integer_type_p (type))
	return false;
    }
  return true;
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
   COND is the condition for the switch-statement itself.
   Returns the CASE_LABEL_EXPR created, or ERROR_MARK_NODE if no
   CASE_LABEL_EXPR is created.  */

tree
c_add_case_label (location_t loc, splay_tree cases, tree cond,
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
    pedwarn (loc, OPT_Wpedantic,
	     "range expressions in switch statements are non-standard");

  type = TREE_TYPE (cond);
  if (low_value)
    {
      low_value = check_case_value (loc, low_value);
      low_value = convert_and_check (loc, type, low_value);
      low_value = fold (low_value);
      if (low_value == error_mark_node)
	goto error_out;
    }
  if (high_value)
    {
      high_value = check_case_value (loc, high_value);
      high_value = convert_and_check (loc, type, high_value);
      high_value = fold (high_value);
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
	  inform (DECL_SOURCE_LOCATION (duplicate),
		  "this is the first entry overlapping that value");
	}
      else if (low_value)
	{
	  error_at (loc, "duplicate case value") ;
	  inform (DECL_SOURCE_LOCATION (duplicate), "previously used here");
	}
      else
	{
	  error_at (loc, "multiple default labels in one switch");
	  inform (DECL_SOURCE_LOCATION (duplicate),
		  "this is the first default label");
	}
      goto error_out;
    }

  /* Add a CASE_LABEL to the statement-tree.  */
  case_label = add_stmt (build_case_label (low_value, high_value, label));
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

/* Subroutine of c_switch_covers_all_cases_p, called via
   splay_tree_foreach.  Return 1 if it doesn't cover all the cases.
   ARGS[0] is initially NULL and after the first iteration is the
   so far highest case label.  ARGS[1] is the minimum of SWITCH_COND's
   type.  */

static int
c_switch_covers_all_cases_p_1 (splay_tree_node node, void *data)
{
  tree label = (tree) node->value;
  tree *args = (tree *) data;

  /* If there is a default case, we shouldn't have called this.  */
  gcc_assert (CASE_LOW (label));

  if (args[0] == NULL_TREE)
    {
      if (wi::to_widest (args[1]) < wi::to_widest (CASE_LOW (label)))
	return 1;
    }
  else if (wi::add (wi::to_widest (args[0]), 1)
	   != wi::to_widest (CASE_LOW (label)))
    return 1;
  if (CASE_HIGH (label))
    args[0] = CASE_HIGH (label);
  else
    args[0] = CASE_LOW (label);
  return 0;
}

/* Return true if switch with CASES and switch condition with type
   covers all possible values in the case labels.  */

bool
c_switch_covers_all_cases_p (splay_tree cases, tree type)
{
  /* If there is default:, this is always the case.  */
  splay_tree_node default_node
    = splay_tree_lookup (cases, (splay_tree_key) NULL);
  if (default_node)
    return true;

  if (!INTEGRAL_TYPE_P (type))
    return false;

  tree args[2] = { NULL_TREE, TYPE_MIN_VALUE (type) };
  if (splay_tree_foreach (cases, c_switch_covers_all_cases_p_1, args))
    return false;

  /* If there are no cases at all, or if the highest case label
     is smaller than TYPE_MAX_VALUE, return false.  */
  if (args[0] == NULL_TREE
      || wi::to_widest (args[0]) < wi::to_widest (TYPE_MAX_VALUE (type)))
    return false;

  return true;
}

/* Return true if stmt can fall through.  Used by block_may_fallthru
   default case.  */

bool
c_block_may_fallthru (const_tree stmt)
{
  switch (TREE_CODE (stmt))
    {
    case SWITCH_STMT:
      return (!SWITCH_STMT_ALL_CASES_P (stmt)
	      || !SWITCH_STMT_NO_BREAK_P (stmt)
	      || block_may_fallthru (SWITCH_STMT_BODY (stmt)));

    default:
      return true;
    }
}

/* Finish an expression taking the address of LABEL (an
   IDENTIFIER_NODE).  Returns an expression for the address.

   LOC is the location for the expression returned.  */

tree
finish_label_address_expr (tree label, location_t loc)
{
  tree result;

  pedwarn (input_location, OPT_Wpedantic, "taking the address of a label is non-standard");

  if (label == error_mark_node)
    return error_mark_node;

  label = lookup_label (label);
  if (label == NULL_TREE)
    result = null_pointer_node;
  else
    {
      TREE_USED (label) = 1;
      result = build1 (ADDR_EXPR, ptr_type_node, label);
      /* The current function is not necessarily uninlinable.
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
  if (flag_char8_t)
    builtin_define_with_value ("__CHAR8_TYPE__", CHAR8_TYPE, 0);
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
  /* GIMPLE FE testcases need access to the GCC internal 'sizetype'.
     Expose it as __SIZETYPE__.  */
  if (flag_gimple)
    builtin_define_with_value ("__SIZETYPE__", SIZETYPE, 0);
}

static void
c_init_attributes (void)
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)				\
  built_in_attributes[(int) ENUM] = NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)				\
  built_in_attributes[(int) ENUM] = build_int_cst (integer_type_node, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)				\
  built_in_attributes[(int) ENUM] = build_string (strlen (VALUE), VALUE);
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

/* Check whether the byte alignment ALIGN is a valid user-specified
   alignment less than the supported maximum.  If so, return ALIGN's
   base-2 log; if not, output an error and return -1.  If OBJFILE
   then reject alignments greater than MAX_OFILE_ALIGNMENT when
   converted to bits.  Otherwise, consider valid only alignments
   that are less than HOST_BITS_PER_INT - LOG2_BITS_PER_UNIT.
   Zero is not considered a valid argument (and results in -1 on
   return) but it only triggers a warning when WARN_ZERO is set.  */

int
check_user_alignment (const_tree align, bool objfile, bool warn_zero)
{
  if (error_operand_p (align))
    return -1;

  if (TREE_CODE (align) != INTEGER_CST
      || !INTEGRAL_TYPE_P (TREE_TYPE (align)))
    {
      error ("requested alignment is not an integer constant");
      return -1;
    }

  if (integer_zerop (align))
    {
      if (warn_zero)
	warning (OPT_Wattributes,
		 "requested alignment %qE is not a positive power of 2",
		 align);
      return -1;
    }

  /* Log2 of the byte alignment ALIGN.  */
  int log2align;
  if (tree_int_cst_sgn (align) == -1
      || (log2align = tree_log2 (align)) == -1)
    {
      error ("requested alignment %qE is not a positive power of 2",
	     align);
      return -1;
    }

  if (objfile)
    {
      unsigned maxalign = MAX_OFILE_ALIGNMENT / BITS_PER_UNIT;
      if (!tree_fits_uhwi_p (align) || tree_to_uhwi (align) > maxalign)
	{
	  error ("requested alignment %qE exceeds object file maximum %u",
		 align, maxalign);
	  return -1;
	}
    }

  if (log2align >= HOST_BITS_PER_INT - LOG2_BITS_PER_UNIT)
    {
      error ("requested alignment %qE exceeds maximum %u",
	     align, 1U << (HOST_BITS_PER_INT - LOG2_BITS_PER_UNIT - 1));
      return -1;
    }

  return log2align;
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
  gcc_assert (VAR_OR_FUNCTION_DECL_P (decl));

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
	  if (((VAR_P (decl) && TREE_STATIC (decl))
	       || TREE_CODE (decl) == FUNCTION_DECL)
	      && DECL_RTL_SET_P (decl))
	    make_decl_rtl (decl);
	}
    }
  return false;
}

/* Data to communicate through check_function_arguments_recurse between
   check_function_nonnull and check_nonnull_arg.  */

struct nonnull_arg_ctx
{
  /* Location of the call.  */
  location_t loc;
  /* The function whose arguments are being checked and its type (used
     for calls through function pointers).  */
  const_tree fndecl, fntype;
  /* True if a warning has been issued.  */
  bool warned_p;
};

/* Check the argument list of a function call to CTX.FNDECL of CTX.FNTYPE
   for null in argument slots that are marked as requiring a non-null
   pointer argument.  The NARGS arguments are passed in the array ARGARRAY.
   Return true if we have warned.  */

static bool
check_function_nonnull (nonnull_arg_ctx &ctx, int nargs, tree *argarray)
{
  int firstarg = 0;
  if (TREE_CODE (ctx.fntype) == METHOD_TYPE)
    {
      bool closure = false;
      if (ctx.fndecl)
	{
	  /* For certain lambda expressions the C++ front end emits calls
	     that pass a null this pointer as an argument named __closure
	     to the member operator() of empty function.  Detect those
	     and avoid checking them, but proceed to check the remaining
	     arguments.  */
	  tree arg0 = DECL_ARGUMENTS (ctx.fndecl);
	  if (tree arg0name = DECL_NAME (arg0))
	    closure = id_equal (arg0name, "__closure");
	}

      /* In calls to C++ non-static member functions check the this
	 pointer regardless of whether the function is declared with
	 attribute nonnull.  */
      firstarg = 1;
      if (!closure)
	check_function_arguments_recurse (check_nonnull_arg, &ctx, argarray[0],
					  firstarg);
    }

  tree attrs = lookup_attribute ("nonnull", TYPE_ATTRIBUTES (ctx.fntype));
  if (attrs == NULL_TREE)
    return ctx.warned_p;

  tree a = attrs;
  /* See if any of the nonnull attributes has no arguments.  If so,
     then every pointer argument is checked (in which case the check
     for pointer type is done in check_nonnull_arg).  */
  if (TREE_VALUE (a) != NULL_TREE)
    do
      a = lookup_attribute ("nonnull", TREE_CHAIN (a));
    while (a != NULL_TREE && TREE_VALUE (a) != NULL_TREE);

  if (a != NULL_TREE)
    for (int i = firstarg; i < nargs; i++)
      check_function_arguments_recurse (check_nonnull_arg, &ctx, argarray[i],
					i + 1);
  else
    {
      /* Walk the argument list.  If we encounter an argument number we
	 should check for non-null, do it.  */
      for (int i = firstarg; i < nargs; i++)
	{
	  for (a = attrs; ; a = TREE_CHAIN (a))
	    {
	      a = lookup_attribute ("nonnull", a);
	      if (a == NULL_TREE || nonnull_check_p (TREE_VALUE (a), i + 1))
		break;
	    }

	  if (a != NULL_TREE)
	    check_function_arguments_recurse (check_nonnull_arg, &ctx,
					      argarray[i], i + 1);
	}
    }
  return ctx.warned_p;
}

/* Check that the Nth argument of a function call (counting backwards
   from the end) is a (pointer)0.  The NARGS arguments are passed in the
   array ARGARRAY.  */

static void
check_function_sentinel (const_tree fntype, int nargs, tree *argarray)
{
  tree attr = lookup_attribute ("sentinel", TYPE_ATTRIBUTES (fntype));

  if (attr)
    {
      int len = 0;
      int pos = 0;
      tree sentinel;
      function_args_iterator iter;
      tree t;

      /* Skip over the named arguments.  */
      FOREACH_FUNCTION_ARGS (fntype, t, iter)
	{
	  if (len == nargs)
	    break;
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
	  warning (OPT_Wformat_,
		   "not enough variable arguments to fit a sentinel");
	  return;
	}

      /* Validate the sentinel.  */
      sentinel = fold_for_warn (argarray[nargs - 1 - pos]);
      if ((!POINTER_TYPE_P (TREE_TYPE (sentinel))
	   || !integer_zerop (sentinel))
	  /* Although __null (in C++) is only an integer we allow it
	     nevertheless, as we are guaranteed that it's exactly
	     as wide as a pointer, and we don't want to force
	     users to cast the NULL they have written there.
	     We warn with -Wstrict-null-sentinel, though.  */
	  && (warn_strict_null_sentinel || null_node != sentinel))
	warning (OPT_Wformat_, "missing sentinel in function call");
    }
}

/* Check that the same argument isn't passed to two or more
   restrict-qualified formal and issue a -Wrestrict warning
   if it is.  Return true if a warning has been issued.  */

static bool
check_function_restrict (const_tree fndecl, const_tree fntype,
			 int nargs, tree *unfolded_argarray)
{
  int i;
  tree parms = TYPE_ARG_TYPES (fntype);

  /* Call fold_for_warn on all of the arguments.  */
  auto_vec<tree> argarray (nargs);
  for (i = 0; i < nargs; i++)
    argarray.quick_push (fold_for_warn (unfolded_argarray[i]));

  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL)
    {
      /* Avoid diagnosing calls built-ins with a zero size/bound
	 here.  They are checked in more detail elsewhere.  */
      if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL)
	  && nargs == 3
	  && TREE_CODE (argarray[2]) == INTEGER_CST
	  && integer_zerop (argarray[2]))
	return false;

      if (DECL_ARGUMENTS (fndecl))
	parms = DECL_ARGUMENTS (fndecl);
    }

  for (i = 0; i < nargs; i++)
    TREE_VISITED (argarray[i]) = 0;

  bool warned = false;

  for (i = 0; i < nargs && parms && parms != void_list_node; i++)
    {
      tree type;
      if (TREE_CODE (parms) == PARM_DECL)
	{
	  type = TREE_TYPE (parms);
	  parms = DECL_CHAIN (parms);
	}
      else
	{
	  type = TREE_VALUE (parms);
	  parms = TREE_CHAIN (parms);
	}
      if (POINTER_TYPE_P (type)
	  && TYPE_RESTRICT (type)
	  && !TYPE_READONLY (TREE_TYPE (type)))
	warned |= warn_for_restrict (i, argarray.address (), nargs);
    }

  for (i = 0; i < nargs; i++)
    TREE_VISITED (argarray[i]) = 0;

  return warned;
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
      bool found = get_attribute_operand (TREE_VALUE (args), &arg_num);

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
check_nonnull_arg (void *ctx, tree param, unsigned HOST_WIDE_INT param_num)
{
  struct nonnull_arg_ctx *pctx = (struct nonnull_arg_ctx *) ctx;

  /* Just skip checking the argument if it's not a pointer.  This can
     happen if the "nonnull" attribute was given without an operand
     list (which means to check every pointer argument).  */

  tree paramtype = TREE_TYPE (param);
  if (TREE_CODE (paramtype) != POINTER_TYPE
      && TREE_CODE (paramtype) != NULLPTR_TYPE)
    return;

  /* Diagnose the simple cases of null arguments.  */
  if (!integer_zerop (fold_for_warn (param)))
    return;

  auto_diagnostic_group adg;

  const location_t loc = EXPR_LOC_OR_LOC (param, pctx->loc);

  if (TREE_CODE (pctx->fntype) == METHOD_TYPE)
    --param_num;

  bool warned;
  if (param_num == 0)
    {
      warned = warning_at (loc, OPT_Wnonnull,
			   "%qs pointer is null", "this");
      if (warned && pctx->fndecl)
	inform (DECL_SOURCE_LOCATION (pctx->fndecl),
		"in a call to non-static member function %qD",
		pctx->fndecl);
    }
  else
    {
      warned = warning_at (loc, OPT_Wnonnull,
			   "argument %u null where non-null expected",
			   (unsigned) param_num);
      if (warned && pctx->fndecl)
	inform (DECL_SOURCE_LOCATION (pctx->fndecl),
		"in a call to function %qD declared %qs",
		pctx->fndecl, "nonnull");
    }

  if (warned)
    pctx->warned_p = true;
}

/* Helper for attribute handling; fetch the operand number from
   the attribute argument list.  */

bool
get_attribute_operand (tree arg_num_expr, unsigned HOST_WIDE_INT *valp)
{
  /* Verify the arg number is a small constant.  */
  if (tree_fits_uhwi_p (arg_num_expr))
    {
      *valp = tree_to_uhwi (arg_num_expr);
      return true;
    }
  else
    return false;
}

/* Arguments being collected for optimization.  */
typedef const char *const_char_p;		/* For DEF_VEC_P.  */
static GTY(()) vec<const_char_p, va_gc> *optimize_args;


/* Inner function to convert a TREE_LIST to argv string to parse the optimize
   options in ARGS.  ATTR_P is true if this is for attribute(optimize), and
   false for #pragma GCC optimize.  */

bool
parse_optimize_options (tree args, bool attr_p)
{
  bool ret = true;
  unsigned opt_argc;
  unsigned i;
  const char **opt_argv;
  struct cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;
  tree ap;

  /* Build up argv vector.  Just in case the string is stored away, use garbage
     collected strings.  */
  vec_safe_truncate (optimize_args, 0);
  vec_safe_push (optimize_args, (const char *) NULL);

  for (ap = args; ap != NULL_TREE; ap = TREE_CHAIN (ap))
    {
      tree value = TREE_VALUE (ap);

      if (TREE_CODE (value) == INTEGER_CST)
	{
	  char buffer[20];
	  sprintf (buffer, "-O%ld", (long) TREE_INT_CST_LOW (value));
	  vec_safe_push (optimize_args, ggc_strdup (buffer));
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

	      /* If the user supplied -Oxxx or -fxxx, only allow -Oxxx or -fxxx
		 options.  */
	      if (*p == '-' && p[1] != 'O' && p[1] != 'f')
		{
		  ret = false;
		  if (attr_p)
		    warning (OPT_Wattributes,
			     "bad option %qs to attribute %<optimize%>", p);
		  else
		    warning (OPT_Wpragmas,
			     "bad option %qs to pragma %<optimize%>", p);
		  continue;
		}

	      /* Can't use GC memory here, see PR88007.  */
	      r = q = XOBNEWVEC (&opts_obstack, char, len2 + 3);

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
	      vec_safe_push (optimize_args, (const char *) q);
	    }

	}
    }

  opt_argc = optimize_args->length ();
  opt_argv = (const char **) alloca (sizeof (char *) * (opt_argc + 1));

  for (i = 1; i < opt_argc; i++)
    opt_argv[i] = (*optimize_args)[i];

  /* Now parse the options.  */
  decode_cmdline_options_to_array_default_mask (opt_argc, opt_argv,
						&decoded_options,
						&decoded_options_count);
  /* Drop non-Optimization options.  */
  unsigned j = 1;
  for (i = 1; i < decoded_options_count; ++i)
    {
      if (! (cl_options[decoded_options[i].opt_index].flags & CL_OPTIMIZATION))
	{
	  ret = false;
	  if (attr_p)
	    warning (OPT_Wattributes,
		     "bad option %qs to attribute %<optimize%>",
		     decoded_options[i].orig_option_with_args_text);
	  else
	    warning (OPT_Wpragmas,
		     "bad option %qs to pragma %<optimize%>",
		     decoded_options[i].orig_option_with_args_text);
	  continue;
	}
      if (i != j)
	decoded_options[j] = decoded_options[i];
      j++;
    }
  decoded_options_count = j;
  /* And apply them.  */
  decode_options (&global_options, &global_options_set,
		  decoded_options, decoded_options_count,
		  input_location, global_dc, NULL);
  free (decoded_options);

  targetm.override_options_after_change();

  optimize_args->truncate (0);
  return ret;
}

/* Check whether ATTR is a valid attribute fallthrough.  */

bool
attribute_fallthrough_p (tree attr)
{
  if (attr == error_mark_node)
   return false;
  tree t = lookup_attribute ("fallthrough", attr);
  if (t == NULL_TREE)
    return false;
  /* It is no longer true that "this attribute shall appear at most once in
     each attribute-list", but we still give a warning.  */
  if (lookup_attribute ("fallthrough", TREE_CHAIN (t)))
    warning (OPT_Wattributes, "attribute %<fallthrough%> specified multiple "
	     "times");
  /* No attribute-argument-clause shall be present.  */
  else if (TREE_VALUE (t) != NULL_TREE)
    warning (OPT_Wattributes, "%<fallthrough%> attribute specified with "
	     "a parameter");
  /* Warn if other attributes are found.  */
  for (t = attr; t != NULL_TREE; t = TREE_CHAIN (t))
    {
      tree name = get_attribute_name (t);
      if (!is_attribute_p ("fallthrough", name))
	{
	  if (!c_dialect_cxx () && get_attribute_namespace (t) == NULL_TREE)
	    /* The specifications of standard attributes in C mean
	       this is a constraint violation.  */
	    pedwarn (input_location, OPT_Wattributes, "%qE attribute ignored",
		     get_attribute_name (t));
	  else
	    warning (OPT_Wattributes, "%qE attribute ignored", name);
	}
    }
  return true;
}


/* Check for valid arguments being passed to a function with FNTYPE.
   There are NARGS arguments in the array ARGARRAY.  LOC should be used
   for diagnostics.  Return true if either -Wnonnull or -Wrestrict has
   been issued.

   The arguments in ARGARRAY may not have been folded yet (e.g. for C++,
   to preserve location wrappers); checks that require folded arguments
   should call fold_for_warn on them.  */

bool
check_function_arguments (location_t loc, const_tree fndecl, const_tree fntype,
			  int nargs, tree *argarray, vec<location_t> *arglocs)
{
  bool warned_p = false;

  /* Check for null being passed in a pointer argument that must be
     non-null.  In C++, this includes the this pointer.  We also need
     to do this if format checking is enabled.  */
  if (warn_nonnull)
    {
      nonnull_arg_ctx ctx = { loc, fndecl, fntype, false };
      warned_p = check_function_nonnull (ctx, nargs, argarray);
    }

  /* Check for errors in format strings.  */

  if (warn_format || warn_suggest_attribute_format)
    check_function_format (fntype, TYPE_ATTRIBUTES (fntype), nargs, argarray,
			   arglocs);

  if (warn_format)
    check_function_sentinel (fntype, nargs, argarray);

  if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_SPRINTF:
	case BUILT_IN_SPRINTF_CHK:
	case BUILT_IN_SNPRINTF:
	case BUILT_IN_SNPRINTF_CHK:
	  /* Let the sprintf pass handle these.  */
	  return warned_p;

	default:
	  break;
	}
    }

  /* check_function_restrict sets the DECL_READ_P for arguments
     so it must be called unconditionally.  */
  warned_p |= check_function_restrict (fndecl, fntype, nargs, argarray);

  return warned_p;
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
  if (TREE_NO_WARNING (param))
    return;

  if (CONVERT_EXPR_P (param)
      && (TYPE_PRECISION (TREE_TYPE (param))
	  == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (param, 0)))))
    {
      /* Strip coercion.  */
      check_function_arguments_recurse (callback, ctx,
					TREE_OPERAND (param, 0), param_num);
      return;
    }

  if (TREE_CODE (param) == CALL_EXPR && CALL_EXPR_FN (param))
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
	if (is_attribute_p ("format_arg", get_attribute_name (attrs)))
	  {
	    tree inner_arg;
	    tree format_num_expr;
	    int format_num;
	    int i;
	    call_expr_arg_iterator iter;

	    /* Extract the argument number, which was previously checked
	       to be valid.  */
	    format_num_expr = TREE_VALUE (TREE_VALUE (attrs));

	    format_num = tree_to_uhwi (format_num_expr);

	    for (inner_arg = first_call_expr_arg (param, &iter), i = 1;
		 inner_arg != NULL_TREE;
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
      /* Simplify to avoid warning for an impossible case.  */
      param = fold_for_warn (param);
      if (TREE_CODE (param) == COND_EXPR)
	{
	  /* Check both halves of the conditional expression.  */
	  check_function_arguments_recurse (callback, ctx,
					    TREE_OPERAND (param, 1),
					    param_num);
	  check_function_arguments_recurse (callback, ctx,
					    TREE_OPERAND (param, 2),
					    param_num);
	  return;
	}
    }

  (*callback) (ctx, param, param_num);
}

/* Checks for a builtin function FNDECL that the number of arguments
   NARGS against the required number REQUIRED and issues an error if
   there is a mismatch.  Returns true if the number of arguments is
   correct, otherwise false.  LOC is the location of FNDECL.  */

static bool
builtin_function_validate_nargs (location_t loc, tree fndecl, int nargs,
				 int required)
{
  if (nargs < required)
    {
      error_at (loc, "too few arguments to function %qE", fndecl);
      return false;
    }
  else if (nargs > required)
    {
      error_at (loc, "too many arguments to function %qE", fndecl);
      return false;
    }
  return true;
}

/* Helper macro for check_builtin_function_arguments.  */
#define ARG_LOCATION(N)					\
  (arg_loc.is_empty ()					\
   ? EXPR_LOC_OR_LOC (args[(N)], input_location)	\
   : expansion_point_location (arg_loc[(N)]))

/* Verifies the NARGS arguments ARGS to the builtin function FNDECL.
   Returns false if there was an error, otherwise true.  LOC is the
   location of the function; ARG_LOC is a vector of locations of the
   arguments.  If FNDECL is the result of resolving an overloaded
   target built-in, ORIG_FNDECL is the original function decl,
   otherwise it is null.  */

bool
check_builtin_function_arguments (location_t loc, vec<location_t> arg_loc,
				  tree fndecl, tree orig_fndecl,
				  int nargs, tree *args)
{
  if (!fndecl_built_in_p (fndecl))
    return true;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return (!targetm.check_builtin_call
	    || targetm.check_builtin_call (loc, arg_loc, fndecl,
					   orig_fndecl, nargs, args));

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_FRONTEND)
    return true;

  gcc_assert (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL);
  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX:
      if (!tree_fits_uhwi_p (args[2]))
	{
	  error_at (ARG_LOCATION (2),
		    "third argument to function %qE must be a constant integer",
		    fndecl);
	  return false;
	}
      /* fall through */

    case BUILT_IN_ALLOCA_WITH_ALIGN:
      {
	/* Get the requested alignment (in bits) if it's a constant
	   integer expression.  */
	unsigned HOST_WIDE_INT align
	  = tree_fits_uhwi_p (args[1]) ? tree_to_uhwi (args[1]) : 0;

	/* Determine if the requested alignment is a power of 2.  */
	if ((align & (align - 1)))
	  align = 0;

	/* The maximum alignment in bits corresponding to the same
	   maximum in bytes enforced in check_user_alignment().  */
	unsigned maxalign = (UINT_MAX >> 1) + 1;

	/* Reject invalid alignments.  */
	if (align < BITS_PER_UNIT || maxalign < align)
	  {
	    error_at (ARG_LOCATION (1),
		      "second argument to function %qE must be a constant "
		      "integer power of 2 between %qi and %qu bits",
		      fndecl, BITS_PER_UNIT, maxalign);
	    return false;
	  }
	return true;
      }

    case BUILT_IN_CONSTANT_P:
      return builtin_function_validate_nargs (loc, fndecl, nargs, 1);

    case BUILT_IN_ISFINITE:
    case BUILT_IN_ISINF:
    case BUILT_IN_ISINF_SIGN:
    case BUILT_IN_ISNAN:
    case BUILT_IN_ISNORMAL:
    case BUILT_IN_SIGNBIT:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 1))
	{
	  if (TREE_CODE (TREE_TYPE (args[0])) != REAL_TYPE)
	    {
	      error_at (ARG_LOCATION (0), "non-floating-point argument in "
			"call to function %qE", fndecl);
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
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 2))
	{
	  enum tree_code code0, code1;
	  code0 = TREE_CODE (TREE_TYPE (args[0]));
	  code1 = TREE_CODE (TREE_TYPE (args[1]));
	  if (!((code0 == REAL_TYPE && code1 == REAL_TYPE)
		|| (code0 == REAL_TYPE && code1 == INTEGER_TYPE)
		|| (code0 == INTEGER_TYPE && code1 == REAL_TYPE)))
	    {
	      error_at (loc, "non-floating-point arguments in call to "
			"function %qE", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_FPCLASSIFY:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 6))
	{
	  for (unsigned int i = 0; i < 5; i++)
	    if (TREE_CODE (args[i]) != INTEGER_CST)
	      {
		error_at (ARG_LOCATION (i), "non-const integer argument %u in "
			  "call to function %qE", i + 1, fndecl);
		return false;
	      }

	  if (TREE_CODE (TREE_TYPE (args[5])) != REAL_TYPE)
	    {
	      error_at (ARG_LOCATION (5), "non-floating-point argument in "
			"call to function %qE", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_ASSUME_ALIGNED:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 2 + (nargs > 2)))
	{
	  if (nargs >= 3 && TREE_CODE (TREE_TYPE (args[2])) != INTEGER_TYPE)
	    {
	      error_at (ARG_LOCATION (2), "non-integer argument 3 in call to "
			"function %qE", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_ADD_OVERFLOW:
    case BUILT_IN_SUB_OVERFLOW:
    case BUILT_IN_MUL_OVERFLOW:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 3))
	{
	  unsigned i;
	  for (i = 0; i < 2; i++)
	    if (!INTEGRAL_TYPE_P (TREE_TYPE (args[i])))
	      {
		error_at (ARG_LOCATION (i), "argument %u in call to function "
			  "%qE does not have integral type", i + 1, fndecl);
		return false;
	      }
	  if (TREE_CODE (TREE_TYPE (args[2])) != POINTER_TYPE
	      || !INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (args[2]))))
	    {
	      error_at (ARG_LOCATION (2), "argument 3 in call to function %qE "
			"does not have pointer to integral type", fndecl);
	      return false;
	    }
	  else if (TREE_CODE (TREE_TYPE (TREE_TYPE (args[2]))) == ENUMERAL_TYPE)
	    {
	      error_at (ARG_LOCATION (2), "argument 3 in call to function %qE "
			"has pointer to enumerated type", fndecl);
	      return false;
	    }
	  else if (TREE_CODE (TREE_TYPE (TREE_TYPE (args[2]))) == BOOLEAN_TYPE)
	    {
	      error_at (ARG_LOCATION (2), "argument 3 in call to function %qE "
			"has pointer to boolean type", fndecl);
	      return false;
	    }
	  else if (TYPE_READONLY (TREE_TYPE (TREE_TYPE (args[2]))))
	    {
	      error_at (ARG_LOCATION (2), "argument %u in call to function %qE "
			"has pointer to %qs type (%qT)", 3, fndecl, "const",
			TREE_TYPE (args[2]));
	      return false;
	    }
	  else if (TYPE_ATOMIC (TREE_TYPE (TREE_TYPE (args[2]))))
	    {
	      error_at (ARG_LOCATION (2), "argument %u in call to function %qE "
			"has pointer to %qs type (%qT)", 3, fndecl,
			"_Atomic", TREE_TYPE (args[2]));
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_ADD_OVERFLOW_P:
    case BUILT_IN_SUB_OVERFLOW_P:
    case BUILT_IN_MUL_OVERFLOW_P:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 3))
	{
	  unsigned i;
	  for (i = 0; i < 3; i++)
	    if (!INTEGRAL_TYPE_P (TREE_TYPE (args[i])))
	      {
		error_at (ARG_LOCATION (i), "argument %u in call to function "
			  "%qE does not have integral type", i + 1, fndecl);
		return false;
	      }
	  if (TREE_CODE (TREE_TYPE (args[2])) == ENUMERAL_TYPE)
	    {
	      error_at (ARG_LOCATION (2), "argument 3 in call to function "
			"%qE has enumerated type", fndecl);
	      return false;
	    }
	  else if (TREE_CODE (TREE_TYPE (args[2])) == BOOLEAN_TYPE)
	    {
	      error_at (ARG_LOCATION (2), "argument 3 in call to function "
			"%qE has boolean type", fndecl);
	      return false;
	    }
	  return true;
	}
      return false;

    case BUILT_IN_CLEAR_PADDING:
      if (builtin_function_validate_nargs (loc, fndecl, nargs, 1))
	{
	  if (!POINTER_TYPE_P (TREE_TYPE (args[0])))
	    {
	      error_at (ARG_LOCATION (0), "argument %u in call to function "
			"%qE does not have pointer type", 1, fndecl);
	      return false;
	    }
	  else if (!COMPLETE_TYPE_P (TREE_TYPE (TREE_TYPE (args[0]))))
	    {
	      error_at (ARG_LOCATION (0), "argument %u in call to function "
			"%qE points to incomplete type", 1, fndecl);
	      return false;
	    }
	  else if (TYPE_READONLY (TREE_TYPE (TREE_TYPE (args[0]))))
	    {
	      error_at (ARG_LOCATION (0), "argument %u in call to function %qE "
			"has pointer to %qs type (%qT)", 1, fndecl, "const",
			TREE_TYPE (args[0]));
	      return false;
	    }
	  else if (TYPE_ATOMIC (TREE_TYPE (TREE_TYPE (args[0]))))
	    {
	      error_at (ARG_LOCATION (0), "argument %u in call to function %qE "
			"has pointer to %qs type (%qT)", 1, fndecl,
			"_Atomic", TREE_TYPE (args[0]));
	      return false;
	    }
	  return true;
	}
      return false;

    default:
      return true;
    }
}

/* Subroutine of c_parse_error.
   Return the result of concatenating LHS and RHS. RHS is really
   a string literal, its first character is indicated by RHS_START and
   RHS_SIZE is its length (including the terminating NUL character).

   The caller is responsible for deleting the returned pointer.  */

static char *
catenate_strings (const char *lhs, const char *rhs_start, int rhs_size)
{
  const size_t lhs_size = strlen (lhs);
  char *result = XNEWVEC (char, lhs_size + rhs_size);
  memcpy (result, lhs, lhs_size);
  memcpy (result + lhs_size, rhs_start, rhs_size);
  return result;
}

/* Issue the error given by GMSGID at RICHLOC, indicating that it occurred
   before TOKEN, which had the associated VALUE.  */

void
c_parse_error (const char *gmsgid, enum cpp_ttype token_type,
	       tree value, unsigned char token_flags,
	       rich_location *richloc)
{
#define catenate_messages(M1, M2) catenate_strings ((M1), (M2), sizeof (M2))

  char *message = NULL;

  if (token_type == CPP_EOF)
    message = catenate_messages (gmsgid, " at end of input");
  else if (token_type == CPP_CHAR
	   || token_type == CPP_WCHAR
	   || token_type == CPP_CHAR16
	   || token_type == CPP_CHAR32
	   || token_type == CPP_UTF8CHAR)
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
	case CPP_UTF8CHAR:
	  prefix = "u8";
	  break;
        }

      if (val <= UCHAR_MAX && ISGRAPH (val))
	message = catenate_messages (gmsgid, " before %s'%c'");
      else
	message = catenate_messages (gmsgid, " before %s'\\x%x'");

      error_at (richloc, message, prefix, val);
      free (message);
      message = NULL;
    }
  else if (token_type == CPP_CHAR_USERDEF
	   || token_type == CPP_WCHAR_USERDEF
	   || token_type == CPP_CHAR16_USERDEF
	   || token_type == CPP_CHAR32_USERDEF
	   || token_type == CPP_UTF8CHAR_USERDEF)
    message = catenate_messages (gmsgid,
				 " before user-defined character literal");
  else if (token_type == CPP_STRING_USERDEF
	   || token_type == CPP_WSTRING_USERDEF
	   || token_type == CPP_STRING16_USERDEF
	   || token_type == CPP_STRING32_USERDEF
	   || token_type == CPP_UTF8STRING_USERDEF)
    message = catenate_messages (gmsgid, " before user-defined string literal");
  else if (token_type == CPP_STRING
	   || token_type == CPP_WSTRING
	   || token_type == CPP_STRING16
	   || token_type == CPP_STRING32
	   || token_type == CPP_UTF8STRING)
    message = catenate_messages (gmsgid, " before string constant");
  else if (token_type == CPP_NUMBER)
    message = catenate_messages (gmsgid, " before numeric constant");
  else if (token_type == CPP_NAME)
    {
      message = catenate_messages (gmsgid, " before %qE");
      error_at (richloc, message, value);
      free (message);
      message = NULL;
    }
  else if (token_type == CPP_PRAGMA)
    message = catenate_messages (gmsgid, " before %<#pragma%>");
  else if (token_type == CPP_PRAGMA_EOL)
    message = catenate_messages (gmsgid, " before end of line");
  else if (token_type == CPP_DECLTYPE)
    message = catenate_messages (gmsgid, " before %<decltype%>");
  else if (token_type < N_TTYPES)
    {
      message = catenate_messages (gmsgid, " before %qs token");
      error_at (richloc, message, cpp_type2name (token_type, token_flags));
      free (message);
      message = NULL;
    }
  else
    error_at (richloc, gmsgid);

  if (message)
    {
      error_at (richloc, message);
      free (message);
    }
#undef catenate_messages
}

/* Return the gcc option code associated with the reason for a cpp
   message, or 0 if none.  */

static int
c_option_controlling_cpp_diagnostic (enum cpp_warning_reason reason)
{
  const struct cpp_reason_option_codes_t *entry;

  for (entry = cpp_reason_option_codes; entry->reason != CPP_W_NONE; entry++)
    {
      if (entry->reason == reason)
	return entry->option_code;
    }
  return 0;
}

/* Callback from cpp_diagnostic for PFILE to print diagnostics from the
   preprocessor.  The diagnostic is of type LEVEL, with REASON set
   to the reason code if LEVEL is represents a warning, at location
   RICHLOC unless this is after lexing and the compiler's location
   should be used instead; MSG is the translated message and AP
   the arguments.  Returns true if a diagnostic was emitted, false
   otherwise.  */

bool
c_cpp_diagnostic (cpp_reader *pfile ATTRIBUTE_UNUSED,
		  enum cpp_diagnostic_level level,
		  enum cpp_warning_reason reason,
		  rich_location *richloc,
		  const char *msg, va_list *ap)
{
  diagnostic_info diagnostic;
  diagnostic_t dlevel;
  bool save_warn_system_headers = global_dc->dc_warn_system_headers;
  bool ret;

  switch (level)
    {
    case CPP_DL_WARNING_SYSHDR:
      if (flag_no_output)
	return false;
      global_dc->dc_warn_system_headers = 1;
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
    richloc->set_range (0, input_location, SHOW_RANGE_WITH_CARET);
  diagnostic_set_info_translated (&diagnostic, msg, ap,
				  richloc, dlevel);
  diagnostic_override_option_index
    (&diagnostic,
     c_option_controlling_cpp_diagnostic (reason));
  ret = diagnostic_report_diagnostic (global_dc, &diagnostic);
  if (level == CPP_DL_WARNING_SYSHDR)
    global_dc->dc_warn_system_headers = save_warn_system_headers;
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

/* Fold an offsetof-like expression.  EXPR is a nested sequence of component
   references with an INDIRECT_REF of a constant at the bottom; much like the
   traditional rendering of offsetof as a macro.  TYPE is the desired type of
   the whole expression.  Return the folded result.  */

tree
fold_offsetof (tree expr, tree type, enum tree_code ctx)
{
  tree base, off, t;
  tree_code code = TREE_CODE (expr);
  switch (code)
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

    case NOP_EXPR:
    case INDIRECT_REF:
      if (!TREE_CONSTANT (TREE_OPERAND (expr, 0)))
	{
	  error ("cannot apply %<offsetof%> to a non constant address");
	  return error_mark_node;
	}
      return convert (type, TREE_OPERAND (expr, 0));

    case COMPONENT_REF:
      base = fold_offsetof (TREE_OPERAND (expr, 0), type, code);
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
			    size_int (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (t))
				      / BITS_PER_UNIT));
      break;

    case ARRAY_REF:
      base = fold_offsetof (TREE_OPERAND (expr, 0), type, code);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      STRIP_ANY_LOCATION_WRAPPER (t);

      /* Check if the offset goes beyond the upper bound of the array.  */
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) >= 0)
	{
	  tree upbound = array_ref_up_bound (expr);
	  if (upbound != NULL_TREE
	      && TREE_CODE (upbound) == INTEGER_CST
	      && !tree_int_cst_equal (upbound,
				      TYPE_MAX_VALUE (TREE_TYPE (upbound))))
	    {
	      if (ctx != ARRAY_REF && ctx != COMPONENT_REF)
	        upbound = size_binop (PLUS_EXPR, upbound,
				      build_int_cst (TREE_TYPE (upbound), 1));
	      if (tree_int_cst_lt (upbound, t))
		{
		  tree v;

		  for (v = TREE_OPERAND (expr, 0);
		       TREE_CODE (v) == COMPONENT_REF;
		       v = TREE_OPERAND (v, 0))
		    if (TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			== RECORD_TYPE)
		      {
			tree fld_chain = DECL_CHAIN (TREE_OPERAND (v, 1));
			for (; fld_chain; fld_chain = DECL_CHAIN (fld_chain))
			  if (TREE_CODE (fld_chain) == FIELD_DECL)
			    break;

			if (fld_chain)
			  break;
		      }
		  /* Don't warn if the array might be considered a poor
		     man's flexible array member with a very permissive
		     definition thereof.  */
		  if (TREE_CODE (v) == ARRAY_REF
		      || TREE_CODE (v) == COMPONENT_REF)
		    warning (OPT_Warray_bounds,
			     "index %E denotes an offset "
			     "greater than size of %qT",
			     t, TREE_TYPE (TREE_OPERAND (expr, 0)));
		}
	    }
	}

      t = convert (sizetype, t);
      off = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (TREE_TYPE (expr)), t);
      break;

    case COMPOUND_EXPR:
      /* Handle static members of volatile structs.  */
      t = TREE_OPERAND (expr, 1);
      gcc_checking_assert (VAR_P (get_base_address (t)));
      return fold_offsetof (t, type);

    default:
      gcc_unreachable ();
    }

  if (!POINTER_TYPE_P (type))
    return size_binop (PLUS_EXPR, base, convert (type, off));
  return fold_build_pointer_plus (base, off);
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
  bool overflow_p = false;

  maxindex = size_zero_node;
  if (initial_value)
    {
      STRIP_ANY_LOCATION_WRAPPER (initial_value);

      if (TREE_CODE (initial_value) == STRING_CST)
	{
	  int eltsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (initial_value)));
	  maxindex = size_int (TREE_STRING_LENGTH (initial_value)/eltsize - 1);
	}
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (initial_value);

	  if (vec_safe_is_empty (v))
	    {
	      if (pedantic)
		failure = 3;
	      maxindex = ssize_int (-1);
	    }
	  else
	    {
	      tree curindex;
	      unsigned HOST_WIDE_INT cnt;
	      constructor_elt *ce;
	      bool fold_p = false;

	      if ((*v)[0].index)
		maxindex = (*v)[0].index, fold_p = true;

	      curindex = maxindex;

	      for (cnt = 1; vec_safe_iterate (v, cnt, &ce); cnt++)
		{
		  bool curfold_p = false;
		  if (ce->index)
		    curindex = ce->index, curfold_p = true;
		  else
		    {
		      if (fold_p)
			{
			  /* Since we treat size types now as ordinary
			     unsigned types, we need an explicit overflow
			     check.  */
			  tree orig = curindex;
		          curindex = fold_convert (sizetype, curindex);
			  overflow_p |= tree_int_cst_lt (curindex, orig);
			}
		      curindex = size_binop (PLUS_EXPR, curindex,
					     size_one_node);
		    }
		  if (tree_int_cst_lt (maxindex, curindex))
		    maxindex = curindex, fold_p = curfold_p;
		}
	      if (fold_p)
		{
		  tree orig = maxindex;
	          maxindex = fold_convert (sizetype, maxindex);
		  overflow_p |= tree_int_cst_lt (maxindex, orig);
		}
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
    unqual_elt = c_build_qualified_type (elt, KEEP_QUAL_ADDR_SPACE (quals));

  /* Using build_distinct_type_copy and modifying things afterward instead
     of using build_array_type to create a new type preserves all of the
     TYPE_LANG_FLAG_? bits that the front end may have set.  */
  main_type = build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
  TREE_TYPE (main_type) = unqual_elt;
  TYPE_DOMAIN (main_type)
    = build_range_type (TREE_TYPE (maxindex),
			build_int_cst (TREE_TYPE (maxindex), 0), maxindex);
  TYPE_TYPELESS_STORAGE (main_type) = TYPE_TYPELESS_STORAGE (type);
  layout_type (main_type);

  /* Make sure we have the canonical MAIN_TYPE. */
  hashval_t hashcode = type_hash_canon_hash (main_type);
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
			  TYPE_CANONICAL (TYPE_DOMAIN (main_type)),
			  TYPE_TYPELESS_STORAGE (main_type));
  else
    TYPE_CANONICAL (main_type) = main_type;

  if (quals == 0)
    type = main_type;
  else
    type = c_build_qualified_type (main_type, quals);

  if (COMPLETE_TYPE_P (type)
      && TREE_CODE (TYPE_SIZE_UNIT (type)) == INTEGER_CST
      && (overflow_p || TREE_OVERFLOW (TYPE_SIZE_UNIT (type))))
    {
      error ("size of array is too large");
      /* If we proceed with the array type as it is, we'll eventually
	 crash in tree_to_[su]hwi().  */
      type = error_mark_node;
    }

  *ptype = type;
  return failure;
}

/* INIT is an constructor of a structure with a flexible array member.
   Complete the flexible array member with a domain based on it's value.  */
void
complete_flexible_array_elts (tree init)
{
  tree elt, type;

  if (init == NULL_TREE || TREE_CODE (init) != CONSTRUCTOR)
    return;

  if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
    return;

  elt = CONSTRUCTOR_ELTS (init)->last ().value;
  type = TREE_TYPE (elt);
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_SIZE (type) == NULL_TREE)
    complete_array_type (&TREE_TYPE (elt), elt, false);
  else
    complete_flexible_array_elts (elt);
}

/* Like c_mark_addressable but don't check register qualifier.  */
void 
c_common_mark_addressable_vec (tree t)
{   
  if (TREE_CODE (t) == C_MAYBE_CONST_EXPR)
    t = C_MAYBE_CONST_EXPR_EXPR (t);
  while (handled_component_p (t))
    t = TREE_OPERAND (t, 0);
  if (!VAR_P (t)
      && TREE_CODE (t) != PARM_DECL
      && TREE_CODE (t) != COMPOUND_LITERAL_EXPR)
    return;
  if (!VAR_P (t) || !DECL_HARD_REGISTER (t))
    TREE_ADDRESSABLE (t) = 1;
  if (TREE_CODE (t) == COMPOUND_LITERAL_EXPR)
    TREE_ADDRESSABLE (COMPOUND_LITERAL_EXPR_DECL (t)) = 1;
}



/* Used to help initialize the builtin-types.def table.  When a type of
   the correct size doesn't exist, use error_mark_node instead of NULL.
   The later results in segfaults even when a decl using the type doesn't
   get invoked.  */

tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = c_common_type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* Work out the size of the first argument of a call to
   __builtin_speculation_safe_value.  Only pointers and integral types
   are permitted.  Return -1 if the argument type is not supported or
   the size is too large; 0 if the argument type is a pointer or the
   size if it is integral.  */
static enum built_in_function
speculation_safe_value_resolve_call (tree function, vec<tree, va_gc> *params)
{
  /* Type of the argument.  */
  tree type;
  int size;

  if (vec_safe_is_empty (params))
    {
      error ("too few arguments to function %qE", function);
      return BUILT_IN_NONE;
    }

  type = TREE_TYPE ((*params)[0]);
  if (TREE_CODE (type) == ARRAY_TYPE && c_dialect_cxx ())
    {
      /* Force array-to-pointer decay for C++.   */
      (*params)[0] = default_conversion ((*params)[0]);
      type = TREE_TYPE ((*params)[0]);
    }

  if (POINTER_TYPE_P (type))
    return BUILT_IN_SPECULATION_SAFE_VALUE_PTR;

  if (!INTEGRAL_TYPE_P (type))
    goto incompatible;

  if (!COMPLETE_TYPE_P (type))
    goto incompatible;

  size = tree_to_uhwi (TYPE_SIZE_UNIT (type));
  if (size == 1 || size == 2 || size == 4 || size == 8 || size == 16)
    return ((enum built_in_function)
	    ((int) BUILT_IN_SPECULATION_SAFE_VALUE_1 + exact_log2 (size)));

 incompatible:
  /* Issue the diagnostic only if the argument is valid, otherwise
     it would be redundant at best and could be misleading.  */
  if (type != error_mark_node)
    error ("operand type %qT is incompatible with argument %d of %qE",
	   type, 1, function);

  return BUILT_IN_NONE;
}

/* Validate and coerce PARAMS, the arguments to ORIG_FUNCTION to fit
   the prototype for FUNCTION.  The first argument is mandatory, a second
   argument, if present, must be type compatible with the first.  */
static bool
speculation_safe_value_resolve_params (location_t loc, tree orig_function,
				       vec<tree, va_gc> *params)
{
  tree val;

  if (params->length () == 0)
    {
      error_at (loc, "too few arguments to function %qE", orig_function);
      return false;
    }

  else if (params->length () > 2)
    {
      error_at (loc, "too many arguments to function %qE", orig_function);
      return false;
    }

  val = (*params)[0];
  if (TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE)
    val = default_conversion (val);
  if (!(TREE_CODE (TREE_TYPE (val)) == POINTER_TYPE
	|| TREE_CODE (TREE_TYPE (val)) == INTEGER_TYPE))
    {
      error_at (loc,
		"expecting argument of type pointer or of type integer "
		"for argument 1");
      return false;
    }
  (*params)[0] = val;

  if (params->length () == 2)
    {
      tree val2 = (*params)[1];
      if (TREE_CODE (TREE_TYPE (val2)) == ARRAY_TYPE)
	val2 = default_conversion (val2);
      if (error_operand_p (val2))
	return false;
      if (!(TREE_TYPE (val) == TREE_TYPE (val2)
	    || useless_type_conversion_p (TREE_TYPE (val), TREE_TYPE (val2))))
	{
	  error_at (loc, "both arguments must be compatible");
	  return false;
	}
      (*params)[1] = val2;
    }

  return true;
}

/* Cast the result of the builtin back to the type of the first argument,
   preserving any qualifiers that it might have.  */
static tree
speculation_safe_value_resolve_return (tree first_param, tree result)
{
  tree ptype = TREE_TYPE (first_param);
  tree rtype = TREE_TYPE (result);
  ptype = TYPE_MAIN_VARIANT (ptype);

  if (tree_int_cst_equal (TYPE_SIZE (ptype), TYPE_SIZE (rtype)))
    return convert (ptype, result);

  return result;
}

/* A helper function for resolve_overloaded_builtin in resolving the
   overloaded __sync_ builtins.  Returns a positive power of 2 if the
   first operand of PARAMS is a pointer to a supported data type.
   Returns 0 if an error is encountered.
   FETCH is true when FUNCTION is one of the _FETCH_OP_ or _OP_FETCH_
   built-ins.  */

static int
sync_resolve_size (tree function, vec<tree, va_gc> *params, bool fetch)
{
  /* Type of the argument.  */
  tree argtype;
  /* Type the argument points to.  */
  tree type;
  int size;

  if (vec_safe_is_empty (params))
    {
      error ("too few arguments to function %qE", function);
      return 0;
    }

  argtype = type = TREE_TYPE ((*params)[0]);
  if (TREE_CODE (type) == ARRAY_TYPE && c_dialect_cxx ())
    {
      /* Force array-to-pointer decay for C++.  */
      (*params)[0] = default_conversion ((*params)[0]);
      type = TREE_TYPE ((*params)[0]);
    }
  if (TREE_CODE (type) != POINTER_TYPE)
    goto incompatible;

  type = TREE_TYPE (type);
  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    goto incompatible;

  if (!COMPLETE_TYPE_P (type))
    goto incompatible;

  if (fetch && TREE_CODE (type) == BOOLEAN_TYPE)
    goto incompatible;

  size = tree_to_uhwi (TYPE_SIZE_UNIT (type));
  if (size == 1 || size == 2 || size == 4 || size == 8 || size == 16)
    return size;

 incompatible:
  /* Issue the diagnostic only if the argument is valid, otherwise
     it would be redundant at best and could be misleading.  */
  if (argtype != error_mark_node)
    error ("operand type %qT is incompatible with argument %d of %qE",
	   argtype, 1, function);
  return 0;
}

/* A helper function for resolve_overloaded_builtin.  Adds casts to
   PARAMS to make arguments match up with those of FUNCTION.  Drops
   the variadic arguments at the end.  Returns false if some error
   was encountered; true on success.  */

static bool
sync_resolve_params (location_t loc, tree orig_function, tree function,
		     vec<tree, va_gc> *params, bool orig_format)
{
  function_args_iterator iter;
  tree ptype;
  unsigned int parmnum;

  function_args_iter_init (&iter, TREE_TYPE (function));
  /* We've declared the implementation functions to use "volatile void *"
     as the pointer parameter, so we shouldn't get any complaints from the
     call to check_function_arguments what ever type the user used.  */
  function_args_iter_next (&iter);
  ptype = TREE_TYPE (TREE_TYPE ((*params)[0]));
  ptype = TYPE_MAIN_VARIANT (ptype);

  /* For the rest of the values, we need to cast these to FTYPE, so that we
     don't get warnings for passing pointer types, etc.  */
  parmnum = 0;
  while (1)
    {
      tree val, arg_type;

      arg_type = function_args_iter_cond (&iter);
      /* XXX void_type_node belies the abstraction.  */
      if (arg_type == void_type_node)
	break;

      ++parmnum;
      if (params->length () <= parmnum)
	{
	  error_at (loc, "too few arguments to function %qE", orig_function);
	  return false;
	}

      /* Only convert parameters if arg_type is unsigned integer type with
	 new format sync routines, i.e. don't attempt to convert pointer
	 arguments (e.g. EXPECTED argument of __atomic_compare_exchange_n),
	 bool arguments (e.g. WEAK argument) or signed int arguments (memmodel
	 kinds).  */
      if (TREE_CODE (arg_type) == INTEGER_TYPE && TYPE_UNSIGNED (arg_type))
	{
	  /* Ideally for the first conversion we'd use convert_for_assignment
	     so that we get warnings for anything that doesn't match the pointer
	     type.  This isn't portable across the C and C++ front ends atm.  */
	  val = (*params)[parmnum];
	  val = convert (ptype, val);
	  val = convert (arg_type, val);
	  (*params)[parmnum] = val;
	}

      function_args_iter_next (&iter);
    }

  /* __atomic routines are not variadic.  */
  if (!orig_format && params->length () != parmnum + 1)
    {
      error_at (loc, "too many arguments to function %qE", orig_function);
      return false;
    }

  /* The definition of these primitives is variadic, with the remaining
     being "an optional list of variables protected by the memory barrier".
     No clue what that's supposed to mean, precisely, but we consider all
     call-clobbered variables to be protected so we're safe.  */
  params->truncate (parmnum + 1);

  return true;
}

/* A helper function for resolve_overloaded_builtin.  Adds a cast to
   RESULT to make it match the type of the first pointer argument in
   PARAMS.  */

static tree
sync_resolve_return (tree first_param, tree result, bool orig_format)
{
  tree ptype = TREE_TYPE (TREE_TYPE (first_param));
  tree rtype = TREE_TYPE (result);
  ptype = TYPE_MAIN_VARIANT (ptype);

  /* New format doesn't require casting unless the types are the same size.  */
  if (orig_format || tree_int_cst_equal (TYPE_SIZE (ptype), TYPE_SIZE (rtype)))
    return convert (ptype, result);
  else
    return result;
}

/* This function verifies the PARAMS to generic atomic FUNCTION.
   It returns the size if all the parameters are the same size, otherwise
   0 is returned if the parameters are invalid.  */

static int
get_atomic_generic_size (location_t loc, tree function,
			 vec<tree, va_gc> *params)
{
  unsigned int n_param;
  unsigned int n_model;
  unsigned int outputs = 0; // bitset of output parameters
  unsigned int x;
  int size_0;
  tree type_0;

  /* Determine the parameter makeup.  */
  switch (DECL_FUNCTION_CODE (function))
    {
    case BUILT_IN_ATOMIC_EXCHANGE:
      n_param = 4;
      n_model = 1;
      outputs = 5;
      break;
    case BUILT_IN_ATOMIC_LOAD:
      n_param = 3;
      n_model = 1;
      outputs = 2;
      break;
    case BUILT_IN_ATOMIC_STORE:
      n_param = 3;
      n_model = 1;
      outputs = 1;
      break;
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE:
      n_param = 6;
      n_model = 2;
      outputs = 3;
      break;
    default:
      gcc_unreachable ();
    }

  if (vec_safe_length (params) != n_param)
    {
      error_at (loc, "incorrect number of arguments to function %qE", function);
      return 0;
    }

  /* Get type of first parameter, and determine its size.  */
  type_0 = TREE_TYPE ((*params)[0]);
  if (TREE_CODE (type_0) == ARRAY_TYPE && c_dialect_cxx ())
    {
      /* Force array-to-pointer decay for C++.  */
      (*params)[0] = default_conversion ((*params)[0]);
      type_0 = TREE_TYPE ((*params)[0]);
    }
  if (TREE_CODE (type_0) != POINTER_TYPE || VOID_TYPE_P (TREE_TYPE (type_0)))
    {
      error_at (loc, "argument 1 of %qE must be a non-void pointer type",
		function);
      return 0;
    }

  if (!COMPLETE_TYPE_P (TREE_TYPE (type_0)))
    {
      error_at (loc, "argument 1 of %qE must be a pointer to a complete type",
		function);
      return 0;
    }

  /* Types must be compile time constant sizes. */
  if (!tree_fits_uhwi_p ((TYPE_SIZE_UNIT (TREE_TYPE (type_0)))))
    {
      error_at (loc, 
		"argument 1 of %qE must be a pointer to a constant size type",
		function);
      return 0;
    }

  size_0 = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (type_0)));

  /* Zero size objects are not allowed.  */
  if (size_0 == 0)
    {
      error_at (loc, 
		"argument 1 of %qE must be a pointer to a nonzero size object",
		function);
      return 0;
    }

  /* Check each other parameter is a pointer and the same size.  */
  for (x = 0; x < n_param - n_model; x++)
    {
      int size;
      tree type = TREE_TYPE ((*params)[x]);
      /* __atomic_compare_exchange has a bool in the 4th position, skip it.  */
      if (n_param == 6 && x == 3)
        continue;
      if (TREE_CODE (type) == ARRAY_TYPE && c_dialect_cxx ())
	{
	  /* Force array-to-pointer decay for C++.  */
	  (*params)[x] = default_conversion ((*params)[x]);
	  type = TREE_TYPE ((*params)[x]);
	}
      if (!POINTER_TYPE_P (type))
	{
	  error_at (loc, "argument %d of %qE must be a pointer type", x + 1,
		    function);
	  return 0;
	}
      else if (TYPE_SIZE_UNIT (TREE_TYPE (type))
	       && TREE_CODE ((TYPE_SIZE_UNIT (TREE_TYPE (type))))
		  != INTEGER_CST)
	{
	  error_at (loc, "argument %d of %qE must be a pointer to a constant "
		    "size type", x + 1, function);
	  return 0;
	}
      else if (FUNCTION_POINTER_TYPE_P (type))
	{
	  error_at (loc, "argument %d of %qE must not be a pointer to a "
		    "function", x + 1, function);
	  return 0;
	}
      tree type_size = TYPE_SIZE_UNIT (TREE_TYPE (type));
      size = type_size ? tree_to_uhwi (type_size) : 0;
      if (size != size_0)
	{
	  error_at (loc, "size mismatch in argument %d of %qE", x + 1,
		    function);
	  return 0;
	}

      {
	auto_diagnostic_group d;
	int quals = TYPE_QUALS (TREE_TYPE (type));
	/* Must not write to an argument of a const-qualified type.  */
	if (outputs & (1 << x) && quals & TYPE_QUAL_CONST)
	  {
	    if (c_dialect_cxx ())
	      {
		error_at (loc, "argument %d of %qE must not be a pointer to "
			  "a %<const%> type", x + 1, function);
		return 0;
	      }
	    else
	      pedwarn (loc, OPT_Wincompatible_pointer_types, "argument %d "
		       "of %qE discards %<const%> qualifier", x + 1,
		       function);
	  }
	/* Only the first argument is allowed to be volatile.  */
	if (x > 0 && quals & TYPE_QUAL_VOLATILE)
	  {
	    if (c_dialect_cxx ())
	      {
		error_at (loc, "argument %d of %qE must not be a pointer to "
			  "a %<volatile%> type", x + 1, function);
		return 0;
	      }
	    else
	      pedwarn (loc, OPT_Wincompatible_pointer_types, "argument %d "
		       "of %qE discards %<volatile%> qualifier", x + 1,
		       function);
	  }
      }
    }

  /* Check memory model parameters for validity.  */
  for (x = n_param - n_model ; x < n_param; x++)
    {
      tree p = (*params)[x];
      if (!INTEGRAL_TYPE_P (TREE_TYPE (p)))
	{
	  error_at (loc, "non-integer memory model argument %d of %qE", x + 1,
		    function);
	  return 0;
	}
      p = fold_for_warn (p);
      if (TREE_CODE (p) == INTEGER_CST)
	{
	  /* memmodel_base masks the low 16 bits, thus ignore any bits above
	     it by using TREE_INT_CST_LOW instead of tree_to_*hwi.  Those high
	     bits will be checked later during expansion in target specific
	     way.  */
	  if (memmodel_base (TREE_INT_CST_LOW (p)) >= MEMMODEL_LAST)
	    warning_at (loc, OPT_Winvalid_memory_model,
			"invalid memory model argument %d of %qE", x + 1,
			function);
	}
    }

  return size_0;
}


/* This will take an __atomic_ generic FUNCTION call, and add a size parameter N
   at the beginning of the parameter list PARAMS representing the size of the
   objects.  This is to match the library ABI requirement.  LOC is the location
   of the function call.  
   The new function is returned if it needed rebuilding, otherwise NULL_TREE is
   returned to allow the external call to be constructed.  */

static tree
add_atomic_size_parameter (unsigned n, location_t loc, tree function, 
			   vec<tree, va_gc> *params)
{
  tree size_node;

  /* Insert a SIZE_T parameter as the first param.  If there isn't
     enough space, allocate a new vector and recursively re-build with that.  */
  if (!params->space (1))
    {
      unsigned int z, len;
      vec<tree, va_gc> *v;
      tree f;

      len = params->length ();
      vec_alloc (v, len + 1);
      v->quick_push (build_int_cst (size_type_node, n));
      for (z = 0; z < len; z++)
	v->quick_push ((*params)[z]);
      f = build_function_call_vec (loc, vNULL, function, v, NULL);
      vec_free (v);
      return f;
    }

  /* Add the size parameter and leave as a function call for processing.  */
  size_node = build_int_cst (size_type_node, n);
  params->quick_insert (0, size_node);
  return NULL_TREE;
}


/* Return whether atomic operations for naturally aligned N-byte
   arguments are supported, whether inline or through libatomic.  */
static bool
atomic_size_supported_p (int n)
{
  switch (n)
    {
    case 1:
    case 2:
    case 4:
    case 8:
      return true;

    case 16:
      return targetm.scalar_mode_supported_p (TImode);

    default:
      return false;
    }
}

/* This will process an __atomic_exchange function call, determine whether it
   needs to be mapped to the _N variation, or turned into a library call.
   LOC is the location of the builtin call.
   FUNCTION is the DECL that has been invoked;
   PARAMS is the argument list for the call.  The return value is non-null
   TRUE is returned if it is translated into the proper format for a call to the
   external library, and NEW_RETURN is set the tree for that function.
   FALSE is returned if processing for the _N variation is required, and 
   NEW_RETURN is set to the return value the result is copied into.  */
static bool
resolve_overloaded_atomic_exchange (location_t loc, tree function, 
				    vec<tree, va_gc> *params, tree *new_return)
{	
  tree p0, p1, p2, p3;
  tree I_type, I_type_ptr;
  int n = get_atomic_generic_size (loc, function, params);

  /* Size of 0 is an error condition.  */
  if (n == 0)
    {
      *new_return = error_mark_node;
      return true;
    }

  /* If not a lock-free size, change to the library generic format.  */
  if (!atomic_size_supported_p (n))
    {
      *new_return = add_atomic_size_parameter (n, loc, function, params);
      return true;
    }

  /* Otherwise there is a lockfree match, transform the call from:
       void fn(T* mem, T* desired, T* return, model)
     into
       *return = (T) (fn (In* mem, (In) *desired, model))  */

  p0 = (*params)[0];
  p1 = (*params)[1];
  p2 = (*params)[2];
  p3 = (*params)[3];
  
  /* Create pointer to appropriate size.  */
  I_type = builtin_type_for_size (BITS_PER_UNIT * n, 1);
  I_type_ptr = build_pointer_type (I_type);

  /* Convert object pointer to required type.  */
  p0 = build1 (VIEW_CONVERT_EXPR, I_type_ptr, p0);
  (*params)[0] = p0; 
  /* Convert new value to required type, and dereference it.  */
  p1 = build_indirect_ref (loc, p1, RO_UNARY_STAR);
  p1 = build1 (VIEW_CONVERT_EXPR, I_type, p1);
  (*params)[1] = p1;

  /* Move memory model to the 3rd position, and end param list.  */
  (*params)[2] = p3;
  params->truncate (3);

  /* Convert return pointer and dereference it for later assignment.  */
  *new_return = build_indirect_ref (loc, p2, RO_UNARY_STAR);

  return false;
}


/* This will process an __atomic_compare_exchange function call, determine 
   whether it needs to be mapped to the _N variation, or turned into a lib call.
   LOC is the location of the builtin call.
   FUNCTION is the DECL that has been invoked;
   PARAMS is the argument list for the call.  The return value is non-null
   TRUE is returned if it is translated into the proper format for a call to the
   external library, and NEW_RETURN is set the tree for that function.
   FALSE is returned if processing for the _N variation is required.  */

static bool
resolve_overloaded_atomic_compare_exchange (location_t loc, tree function, 
					    vec<tree, va_gc> *params, 
					    tree *new_return)
{	
  tree p0, p1, p2;
  tree I_type, I_type_ptr;
  int n = get_atomic_generic_size (loc, function, params);

  /* Size of 0 is an error condition.  */
  if (n == 0)
    {
      *new_return = error_mark_node;
      return true;
    }

  /* If not a lock-free size, change to the library generic format.  */
  if (!atomic_size_supported_p (n))
    {
      /* The library generic format does not have the weak parameter, so 
	 remove it from the param list.  Since a parameter has been removed,
	 we can be sure that there is room for the SIZE_T parameter, meaning
	 there will not be a recursive rebuilding of the parameter list, so
	 there is no danger this will be done twice.  */
      if (n > 0)
        {
	  (*params)[3] = (*params)[4];
	  (*params)[4] = (*params)[5];
	  params->truncate (5);
	}
      *new_return = add_atomic_size_parameter (n, loc, function, params);
      return true;
    }

  /* Otherwise, there is a match, so the call needs to be transformed from:
       bool fn(T* mem, T* desired, T* return, weak, success, failure)
     into
       bool fn ((In *)mem, (In *)expected, (In) *desired, weak, succ, fail)  */

  p0 = (*params)[0];
  p1 = (*params)[1];
  p2 = (*params)[2];
  
  /* Create pointer to appropriate size.  */
  I_type = builtin_type_for_size (BITS_PER_UNIT * n, 1);
  I_type_ptr = build_pointer_type (I_type);

  /* Convert object pointer to required type.  */
  p0 = build1 (VIEW_CONVERT_EXPR, I_type_ptr, p0);
  (*params)[0] = p0;

  /* Convert expected pointer to required type.  */
  p1 = build1 (VIEW_CONVERT_EXPR, I_type_ptr, p1);
  (*params)[1] = p1;

  /* Convert desired value to required type, and dereference it.  */
  p2 = build_indirect_ref (loc, p2, RO_UNARY_STAR);
  p2 = build1 (VIEW_CONVERT_EXPR, I_type, p2);
  (*params)[2] = p2;

  /* The rest of the parameters are fine. NULL means no special return value
     processing.*/
  *new_return = NULL;
  return false;
}


/* This will process an __atomic_load function call, determine whether it
   needs to be mapped to the _N variation, or turned into a library call.
   LOC is the location of the builtin call.
   FUNCTION is the DECL that has been invoked;
   PARAMS is the argument list for the call.  The return value is non-null
   TRUE is returned if it is translated into the proper format for a call to the
   external library, and NEW_RETURN is set the tree for that function.
   FALSE is returned if processing for the _N variation is required, and 
   NEW_RETURN is set to the return value the result is copied into.  */

static bool
resolve_overloaded_atomic_load (location_t loc, tree function, 
				vec<tree, va_gc> *params, tree *new_return)
{	
  tree p0, p1, p2;
  tree I_type, I_type_ptr;
  int n = get_atomic_generic_size (loc, function, params);

  /* Size of 0 is an error condition.  */
  if (n == 0)
    {
      *new_return = error_mark_node;
      return true;
    }

  /* If not a lock-free size, change to the library generic format.  */
  if (!atomic_size_supported_p (n))
    {
      *new_return = add_atomic_size_parameter (n, loc, function, params);
      return true;
    }

  /* Otherwise, there is a match, so the call needs to be transformed from:
       void fn(T* mem, T* return, model)
     into
       *return = (T) (fn ((In *) mem, model))  */

  p0 = (*params)[0];
  p1 = (*params)[1];
  p2 = (*params)[2];
  
  /* Create pointer to appropriate size.  */
  I_type = builtin_type_for_size (BITS_PER_UNIT * n, 1);
  I_type_ptr = build_pointer_type (I_type);

  /* Convert object pointer to required type.  */
  p0 = build1 (VIEW_CONVERT_EXPR, I_type_ptr, p0);
  (*params)[0] = p0;

  /* Move memory model to the 2nd position, and end param list.  */
  (*params)[1] = p2;
  params->truncate (2);

  /* Convert return pointer and dereference it for later assignment.  */
  *new_return = build_indirect_ref (loc, p1, RO_UNARY_STAR);

  return false;
}


/* This will process an __atomic_store function call, determine whether it
   needs to be mapped to the _N variation, or turned into a library call.
   LOC is the location of the builtin call.
   FUNCTION is the DECL that has been invoked;
   PARAMS is the argument list for the call.  The return value is non-null
   TRUE is returned if it is translated into the proper format for a call to the
   external library, and NEW_RETURN is set the tree for that function.
   FALSE is returned if processing for the _N variation is required, and 
   NEW_RETURN is set to the return value the result is copied into.  */

static bool
resolve_overloaded_atomic_store (location_t loc, tree function, 
				 vec<tree, va_gc> *params, tree *new_return)
{	
  tree p0, p1;
  tree I_type, I_type_ptr;
  int n = get_atomic_generic_size (loc, function, params);

  /* Size of 0 is an error condition.  */
  if (n == 0)
    {
      *new_return = error_mark_node;
      return true;
    }

  /* If not a lock-free size, change to the library generic format.  */
  if (!atomic_size_supported_p (n))
    {
      *new_return = add_atomic_size_parameter (n, loc, function, params);
      return true;
    }

  /* Otherwise, there is a match, so the call needs to be transformed from:
       void fn(T* mem, T* value, model)
     into
       fn ((In *) mem, (In) *value, model)  */

  p0 = (*params)[0];
  p1 = (*params)[1];
  
  /* Create pointer to appropriate size.  */
  I_type = builtin_type_for_size (BITS_PER_UNIT * n, 1);
  I_type_ptr = build_pointer_type (I_type);

  /* Convert object pointer to required type.  */
  p0 = build1 (VIEW_CONVERT_EXPR, I_type_ptr, p0);
  (*params)[0] = p0;

  /* Convert new value to required type, and dereference it.  */
  p1 = build_indirect_ref (loc, p1, RO_UNARY_STAR);
  p1 = build1 (VIEW_CONVERT_EXPR, I_type, p1);
  (*params)[1] = p1;
  
  /* The memory model is in the right spot already. Return is void.  */
  *new_return = NULL_TREE;

  return false;
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
resolve_overloaded_builtin (location_t loc, tree function,
			    vec<tree, va_gc> *params)
{
  /* Is function one of the _FETCH_OP_ or _OP_FETCH_ built-ins?
     Those are not valid to call with a pointer to _Bool (or C++ bool)
     and so must be rejected.  */
  bool fetch_op = true;
  bool orig_format = true;
  tree new_return = NULL_TREE;

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
  enum built_in_function orig_code = DECL_FUNCTION_CODE (function);
  switch (orig_code)
    {
    case BUILT_IN_SPECULATION_SAFE_VALUE_N:
      {
	tree new_function, first_param, result;
	enum built_in_function fncode
	  = speculation_safe_value_resolve_call (function, params);

	if (fncode == BUILT_IN_NONE)
	  return error_mark_node;

	first_param = (*params)[0];
	if (!speculation_safe_value_resolve_params (loc, function, params))
	  return error_mark_node;

	if (targetm.have_speculation_safe_value (true))
	  {
	    new_function = builtin_decl_explicit (fncode);
	    result = build_function_call_vec (loc, vNULL, new_function, params,
					      NULL);

	    if (result == error_mark_node)
	      return result;

	    return speculation_safe_value_resolve_return (first_param, result);
	  }
	else
	  {
	    /* This target doesn't have, or doesn't need, active mitigation
	       against incorrect speculative execution.  Simply return the
	       first parameter to the builtin.  */
	    if (!targetm.have_speculation_safe_value (false))
	      /* The user has invoked __builtin_speculation_safe_value
		 even though __HAVE_SPECULATION_SAFE_VALUE is not
		 defined: emit a warning.  */
	      warning_at (input_location, 0,
			  "this target does not define a speculation barrier; "
			  "your program will still execute correctly, "
			  "but incorrect speculation may not be "
			  "restricted");

	    /* If the optional second argument is present, handle any side
	       effects now.  */
	    if (params->length () == 2
		&& TREE_SIDE_EFFECTS ((*params)[1]))
	      return build2 (COMPOUND_EXPR, TREE_TYPE (first_param),
			     (*params)[1], first_param);

	    return first_param;
	  }
      }

    case BUILT_IN_ATOMIC_EXCHANGE:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE:
    case BUILT_IN_ATOMIC_LOAD:
    case BUILT_IN_ATOMIC_STORE:
      {
	/* Handle these 4 together so that they can fall through to the next
	   case if the call is transformed to an _N variant.  */
        switch (orig_code)
	  {
	  case BUILT_IN_ATOMIC_EXCHANGE:
	    {
	      if (resolve_overloaded_atomic_exchange (loc, function, params,
						      &new_return))
		return new_return;
	      /* Change to the _N variant.  */
	      orig_code = BUILT_IN_ATOMIC_EXCHANGE_N;
	      break;
	    }

	  case BUILT_IN_ATOMIC_COMPARE_EXCHANGE:
	    {
	      if (resolve_overloaded_atomic_compare_exchange (loc, function,
							      params,
							      &new_return))
		return new_return;
	      /* Change to the _N variant.  */
	      orig_code = BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N;
	      break;
	    }
	  case BUILT_IN_ATOMIC_LOAD:
	    {
	      if (resolve_overloaded_atomic_load (loc, function, params,
						  &new_return))
		return new_return;
	      /* Change to the _N variant.  */
	      orig_code = BUILT_IN_ATOMIC_LOAD_N;
	      break;
	    }
	  case BUILT_IN_ATOMIC_STORE:
	    {
	      if (resolve_overloaded_atomic_store (loc, function, params,
						   &new_return))
		return new_return;
	      /* Change to the _N variant.  */
	      orig_code = BUILT_IN_ATOMIC_STORE_N;
	      break;
	    }
	  default:
	    gcc_unreachable ();
	  }
      }
      /* FALLTHRU */
    case BUILT_IN_ATOMIC_EXCHANGE_N:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N:
    case BUILT_IN_ATOMIC_LOAD_N:
    case BUILT_IN_ATOMIC_STORE_N:
      fetch_op = false;
      /* FALLTHRU */
    case BUILT_IN_ATOMIC_ADD_FETCH_N:
    case BUILT_IN_ATOMIC_SUB_FETCH_N:
    case BUILT_IN_ATOMIC_AND_FETCH_N:
    case BUILT_IN_ATOMIC_NAND_FETCH_N:
    case BUILT_IN_ATOMIC_XOR_FETCH_N:
    case BUILT_IN_ATOMIC_OR_FETCH_N:
    case BUILT_IN_ATOMIC_FETCH_ADD_N:
    case BUILT_IN_ATOMIC_FETCH_SUB_N:
    case BUILT_IN_ATOMIC_FETCH_AND_N:
    case BUILT_IN_ATOMIC_FETCH_NAND_N:
    case BUILT_IN_ATOMIC_FETCH_XOR_N:
    case BUILT_IN_ATOMIC_FETCH_OR_N:
      orig_format = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_N:
    case BUILT_IN_SYNC_FETCH_AND_SUB_N:
    case BUILT_IN_SYNC_FETCH_AND_OR_N:
    case BUILT_IN_SYNC_FETCH_AND_AND_N:
    case BUILT_IN_SYNC_FETCH_AND_XOR_N:
    case BUILT_IN_SYNC_FETCH_AND_NAND_N:
    case BUILT_IN_SYNC_ADD_AND_FETCH_N:
    case BUILT_IN_SYNC_SUB_AND_FETCH_N:
    case BUILT_IN_SYNC_OR_AND_FETCH_N:
    case BUILT_IN_SYNC_AND_AND_FETCH_N:
    case BUILT_IN_SYNC_XOR_AND_FETCH_N:
    case BUILT_IN_SYNC_NAND_AND_FETCH_N:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_N:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_N:
    case BUILT_IN_SYNC_LOCK_RELEASE_N:
      {
	/* The following are not _FETCH_OPs and must be accepted with
	   pointers to _Bool (or C++ bool).  */
	if (fetch_op)
	  fetch_op =
	    (orig_code != BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N
	     && orig_code != BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_N
	     && orig_code != BUILT_IN_SYNC_LOCK_TEST_AND_SET_N
	     && orig_code != BUILT_IN_SYNC_LOCK_RELEASE_N);

	int n = sync_resolve_size (function, params, fetch_op);
	tree new_function, first_param, result;
	enum built_in_function fncode;

	if (n == 0)
	  return error_mark_node;

	fncode = (enum built_in_function)((int)orig_code + exact_log2 (n) + 1);
	new_function = builtin_decl_explicit (fncode);
	if (!sync_resolve_params (loc, function, new_function, params,
				  orig_format))
	  return error_mark_node;

	first_param = (*params)[0];
	result = build_function_call_vec (loc, vNULL, new_function, params,
					  NULL);
	if (result == error_mark_node)
	  return result;
	if (orig_code != BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N
	    && orig_code != BUILT_IN_SYNC_LOCK_RELEASE_N
	    && orig_code != BUILT_IN_ATOMIC_STORE_N
	    && orig_code != BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N)
	  result = sync_resolve_return (first_param, result, orig_format);

	if (fetch_op)
	  /* Prevent -Wunused-value warning.  */
	  TREE_USED (result) = true;

	/* If new_return is set, assign function to that expr and cast the
	   result to void since the generic interface returned void.  */
	if (new_return)
	  {
	    /* Cast function result from I{1,2,4,8,16} to the required type.  */
	    result = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (new_return), result);
	    result = build2 (MODIFY_EXPR, TREE_TYPE (new_return), new_return,
			     result);
	    TREE_SIDE_EFFECTS (result) = 1;
	    protected_set_expr_location (result, loc);
	    result = convert (void_type_node, result);
	  }
	return result;
      }

    default:
      return NULL_TREE;
    }
}

/* vector_types_compatible_elements_p is used in type checks of vectors
   values used as operands of binary operators.  Where it returns true, and
   the other checks of the caller succeed (being vector types in he first
   place, and matching number of elements), we can just treat the types
   as essentially the same.
   Contrast with vector_targets_convertible_p, which is used for vector
   pointer types,  and vector_types_convertible_p, which will allow
   language-specific matches under the control of flag_lax_vector_conversions,
   and might still require a conversion.  */
/* True if vector types T1 and T2 can be inputs to the same binary
   operator without conversion.
   We don't check the overall vector size here because some of our callers
   want to give different error messages when the vectors are compatible
   except for the element count.  */

bool
vector_types_compatible_elements_p (tree t1, tree t2)
{
  bool opaque = TYPE_VECTOR_OPAQUE (t1) || TYPE_VECTOR_OPAQUE (t2);
  t1 = TREE_TYPE (t1);
  t2 = TREE_TYPE (t2);

  enum tree_code c1 = TREE_CODE (t1), c2 = TREE_CODE (t2);

  gcc_assert ((INTEGRAL_TYPE_P (t1)
	       || c1 == REAL_TYPE
	       || c1 == FIXED_POINT_TYPE)
	      && (INTEGRAL_TYPE_P (t2)
		  || c2 == REAL_TYPE
		  || c2 == FIXED_POINT_TYPE));

  t1 = c_common_signed_type (t1);
  t2 = c_common_signed_type (t2);
  /* Equality works here because c_common_signed_type uses
     TYPE_MAIN_VARIANT.  */
  if (t1 == t2)
    return true;
  if (opaque && c1 == c2
      && (INTEGRAL_TYPE_P (t1) || c1 == REAL_TYPE)
      && TYPE_PRECISION (t1) == TYPE_PRECISION (t2))
    return true;
  return false;
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
    if (is_attribute_p ("format", get_attribute_name (ra)))
      break;
  if (ra)
    {
      tree la;
      for (la = TYPE_ATTRIBUTES (ttl); la; la = TREE_CHAIN (la))
	if (is_attribute_p ("format", get_attribute_name (la)))
	  break;
      return !la;
    }
  else
    return false;
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
  if (DECL_IS_UNDECLARED_BUILTIN (x) && TREE_CODE (TREE_TYPE (x)) != ARRAY_TYPE)
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

      /* Mark the type as used only when its type decl is decorated
	 with attribute unused.  */
      if (lookup_attribute ("unused", DECL_ATTRIBUTES (x)))
	TREE_USED (tt) = 1;

      TREE_TYPE (x) = tt;
    }
}

/* Return true if it is worth exposing the DECL_ORIGINAL_TYPE of TYPE to
   the user in diagnostics, false if it would be better to use TYPE itself.
   TYPE is known to satisfy typedef_variant_p.  */

bool
user_facing_original_type_p (const_tree type)
{
  gcc_assert (typedef_variant_p (type));
  tree decl = TYPE_NAME (type);

  /* Look through any typedef in "user" code.  */
  if (!DECL_IN_SYSTEM_HEADER (decl) && !DECL_IS_UNDECLARED_BUILTIN (decl))
    return true;

  /* If the original type is also named and is in the user namespace,
     assume it too is a user-facing type.  */
  tree orig_type = DECL_ORIGINAL_TYPE (decl);
  if (tree orig_id = TYPE_IDENTIFIER (orig_type))
    if (!name_reserved_for_implementation_p (IDENTIFIER_POINTER (orig_id)))
      return true;

  switch (TREE_CODE (orig_type))
    {
    /* Don't look through to an anonymous vector type, since the syntax
       we use for them in diagnostics isn't real C or C++ syntax.
       And if ORIG_TYPE is named but in the implementation namespace,
       TYPE is likely to be more meaningful to the user.  */
    case VECTOR_TYPE:
      return false;

    /* Don't expose anonymous tag types that are presumably meant to be
       known by their typedef name.  Also don't expose tags that are in
       the implementation namespace, such as:

         typedef struct __foo foo;  */
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      return false;

    /* Look through to anything else.  */
    default:
      return true;
    }
}

/* Record the types used by the current global variable declaration
   being parsed, so that we can decide later to emit their debug info.
   Those types are in types_used_by_cur_var_decl, and we are going to
   store them in the types_used_by_vars_hash hash table.
   DECL is the declaration of the global variable that has been parsed.  */

void
record_types_used_by_current_var_decl (tree decl)
{
  gcc_assert (decl && DECL_P (decl) && TREE_STATIC (decl));

  while (types_used_by_cur_var_decl && !types_used_by_cur_var_decl->is_empty ())
    {
      tree type = types_used_by_cur_var_decl->pop ();
      types_used_by_var_decl_insert (type, decl);
    }
}

/* The C and C++ parsers both use vectors to hold function arguments.
   For efficiency, we keep a cache of unused vectors.  This is the
   cache.  */

typedef vec<tree, va_gc> *tree_gc_vec;
static GTY((deletable)) vec<tree_gc_vec, va_gc> *tree_vector_cache;

/* Return a new vector from the cache.  If the cache is empty,
   allocate a new vector.  These vectors are GC'ed, so it is OK if the
   pointer is not released..  */

vec<tree, va_gc> *
make_tree_vector (void)
{
  if (tree_vector_cache && !tree_vector_cache->is_empty ())
    return tree_vector_cache->pop ();
  else
    {
      /* Passing 0 to vec::alloc returns NULL, and our callers require
	 that we always return a non-NULL value.  The vector code uses
	 4 when growing a NULL vector, so we do too.  */
      vec<tree, va_gc> *v;
      vec_alloc (v, 4);
      return v;
    }
}

/* Release a vector of trees back to the cache.  */

void
release_tree_vector (vec<tree, va_gc> *vec)
{
  if (vec != NULL)
    {
      vec->truncate (0);
      vec_safe_push (tree_vector_cache, vec);
    }
}

/* Get a new tree vector holding a single tree.  */

vec<tree, va_gc> *
make_tree_vector_single (tree t)
{
  vec<tree, va_gc> *ret = make_tree_vector ();
  ret->quick_push (t);
  return ret;
}

/* Get a new tree vector of the TREE_VALUEs of a TREE_LIST chain.  */

vec<tree, va_gc> *
make_tree_vector_from_list (tree list)
{
  vec<tree, va_gc> *ret = make_tree_vector ();
  for (; list; list = TREE_CHAIN (list))
    vec_safe_push (ret, TREE_VALUE (list));
  return ret;
}

/* Get a new tree vector of the values of a CONSTRUCTOR.  */

vec<tree, va_gc> *
make_tree_vector_from_ctor (tree ctor)
{
  vec<tree,va_gc> *ret = make_tree_vector ();
  vec_safe_reserve (ret, CONSTRUCTOR_NELTS (ctor));
  for (unsigned i = 0; i < CONSTRUCTOR_NELTS (ctor); ++i)
    ret->quick_push (CONSTRUCTOR_ELT (ctor, i)->value);
  return ret;
}

/* Get a new tree vector which is a copy of an existing one.  */

vec<tree, va_gc> *
make_tree_vector_copy (const vec<tree, va_gc> *orig)
{
  vec<tree, va_gc> *ret;
  unsigned int ix;
  tree t;

  ret = make_tree_vector ();
  vec_safe_reserve (ret, vec_safe_length (orig));
  FOR_EACH_VEC_SAFE_ELT (orig, ix, t)
    ret->quick_push (t);
  return ret;
}

/* Return true if KEYWORD starts a type specifier.  */

bool
keyword_begins_type_specifier (enum rid keyword)
{
  switch (keyword)
    {
    case RID_AUTO_TYPE:
    case RID_INT:
    case RID_CHAR:
    case RID_FLOAT:
    case RID_DOUBLE:
    case RID_VOID:
    case RID_UNSIGNED:
    case RID_LONG:
    case RID_SHORT:
    case RID_SIGNED:
    CASE_RID_FLOATN_NX:
    case RID_DFLOAT32:
    case RID_DFLOAT64:
    case RID_DFLOAT128:
    case RID_FRACT:
    case RID_ACCUM:
    case RID_BOOL:
    case RID_WCHAR:
    case RID_CHAR8:
    case RID_CHAR16:
    case RID_CHAR32:
    case RID_SAT:
    case RID_COMPLEX:
    case RID_TYPEOF:
    case RID_STRUCT:
    case RID_CLASS:
    case RID_UNION:
    case RID_ENUM:
      return true;
    default:
      if (keyword >= RID_FIRST_INT_N
	  && keyword < RID_FIRST_INT_N + NUM_INT_N_ENTS
	  && int_n_enabled_p[keyword-RID_FIRST_INT_N])
	return true;
      return false;
    }
}

/* Return true if KEYWORD names a type qualifier.  */

bool
keyword_is_type_qualifier (enum rid keyword)
{
  switch (keyword)
    {
    case RID_CONST:
    case RID_VOLATILE:
    case RID_RESTRICT:
    case RID_ATOMIC:
      return true;
    default:
      return false;
    }
}

/* Return true if KEYWORD names a storage class specifier.

   RID_TYPEDEF is not included in this list despite `typedef' being
   listed in C99 6.7.1.1.  6.7.1.3 indicates that `typedef' is listed as
   such for syntactic convenience only.  */

bool
keyword_is_storage_class_specifier (enum rid keyword)
{
  switch (keyword)
    {
    case RID_STATIC:
    case RID_EXTERN:
    case RID_REGISTER:
    case RID_AUTO:
    case RID_MUTABLE:
    case RID_THREAD:
      return true;
    default:
      return false;
    }
}

/* Return true if KEYWORD names a function-specifier [dcl.fct.spec].  */

static bool
keyword_is_function_specifier (enum rid keyword)
{
  switch (keyword)
    {
    case RID_INLINE:
    case RID_NORETURN:
    case RID_VIRTUAL:
    case RID_EXPLICIT:
      return true;
    default:
      return false;
    }
}

/* Return true if KEYWORD names a decl-specifier [dcl.spec] or a
   declaration-specifier (C99 6.7).  */

bool
keyword_is_decl_specifier (enum rid keyword)
{
  if (keyword_is_storage_class_specifier (keyword)
      || keyword_is_type_qualifier (keyword)
      || keyword_is_function_specifier (keyword))
    return true;

  switch (keyword)
    {
    case RID_TYPEDEF:
    case RID_FRIEND:
    case RID_CONSTEXPR:
    case RID_CONSTINIT:
      return true;
    default:
      return false;
    }
}

/* Initialize language-specific-bits of tree_contains_struct.  */

void
c_common_init_ts (void)
{
  MARK_TS_EXP (SIZEOF_EXPR);
  MARK_TS_EXP (PAREN_SIZEOF_EXPR);
  MARK_TS_EXP (C_MAYBE_CONST_EXPR);
  MARK_TS_EXP (EXCESS_PRECISION_EXPR);
  MARK_TS_EXP (BREAK_STMT);
  MARK_TS_EXP (CONTINUE_STMT);
  MARK_TS_EXP (DO_STMT);
  MARK_TS_EXP (FOR_STMT);
  MARK_TS_EXP (SWITCH_STMT);
  MARK_TS_EXP (WHILE_STMT);
}

/* Build a user-defined numeric literal out of an integer constant type VALUE
   with identifier SUFFIX.  */

tree
build_userdef_literal (tree suffix_id, tree value,
		       enum overflow_type overflow, tree num_string)
{
  tree literal = make_node (USERDEF_LITERAL);
  USERDEF_LITERAL_SUFFIX_ID (literal) = suffix_id;
  USERDEF_LITERAL_VALUE (literal) = value;
  USERDEF_LITERAL_OVERFLOW (literal) = overflow;
  USERDEF_LITERAL_NUM_STRING (literal) = num_string;
  return literal;
}

/* For vector[index], convert the vector to an array of the underlying type.
   Return true if the resulting ARRAY_REF should not be an lvalue.  */

bool
convert_vector_to_array_for_subscript (location_t loc,
				       tree *vecp, tree index)
{
  bool ret = false;
  if (gnu_vector_type_p (TREE_TYPE (*vecp)))
    {
      tree type = TREE_TYPE (*vecp);

      ret = !lvalue_p (*vecp);

      index = fold_for_warn (index);
      if (TREE_CODE (index) == INTEGER_CST)
        if (!tree_fits_uhwi_p (index)
	    || maybe_ge (tree_to_uhwi (index), TYPE_VECTOR_SUBPARTS (type)))
          warning_at (loc, OPT_Warray_bounds, "index value is out of bound");

      /* We are building an ARRAY_REF so mark the vector as addressable
         to not run into the gimplifiers premature setting of DECL_GIMPLE_REG_P
	 for function parameters.  */
      c_common_mark_addressable_vec (*vecp);

      *vecp = build1 (VIEW_CONVERT_EXPR,
		      build_array_type_nelts (TREE_TYPE (type),
					      TYPE_VECTOR_SUBPARTS (type)),
		      *vecp);
    }
  return ret;
}

/* Determine which of the operands, if any, is a scalar that needs to be
   converted to a vector, for the range of operations.  */
enum stv_conv
scalar_to_vector (location_t loc, enum tree_code code, tree op0, tree op1,
		  bool complain)
{
  tree type0 = TREE_TYPE (op0);
  tree type1 = TREE_TYPE (op1);
  bool integer_only_op = false;
  enum stv_conv ret = stv_firstarg;

  gcc_assert (gnu_vector_type_p (type0) || gnu_vector_type_p (type1));
  switch (code)
    {
      /* Most GENERIC binary expressions require homogeneous arguments.
	 LSHIFT_EXPR and RSHIFT_EXPR are exceptions and accept a first
	 argument that is a vector and a second one that is a scalar, so
	 we never return stv_secondarg for them.  */
      case RSHIFT_EXPR:
      case LSHIFT_EXPR:
	if (TREE_CODE (type0) == INTEGER_TYPE
	    && TREE_CODE (TREE_TYPE (type1)) == INTEGER_TYPE)
	  {
	    if (unsafe_conversion_p (TREE_TYPE (type1), op0,
				     NULL_TREE, false))
	      {
		if (complain)
		  error_at (loc, "conversion of scalar %qT to vector %qT "
			    "involves truncation", type0, type1);
		return stv_error;
	      }
	    else
	      return stv_firstarg;
	  }
	break;

      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case BIT_AND_EXPR:
	integer_only_op = true;
	/* fall through */

      case VEC_COND_EXPR:

      case PLUS_EXPR:
      case MINUS_EXPR:
      case MULT_EXPR:
      case TRUNC_DIV_EXPR:
      case CEIL_DIV_EXPR:
      case FLOOR_DIV_EXPR:
      case ROUND_DIV_EXPR:
      case EXACT_DIV_EXPR:
      case TRUNC_MOD_EXPR:
      case FLOOR_MOD_EXPR:
      case RDIV_EXPR:
      case EQ_EXPR:
      case NE_EXPR:
      case LE_EXPR:
      case GE_EXPR:
      case LT_EXPR:
      case GT_EXPR:
      /* What about UNLT_EXPR?  */
	if (gnu_vector_type_p (type0))
	  {
	    ret = stv_secondarg;
	    std::swap (type0, type1);
	    std::swap (op0, op1);
	  }

	if (TREE_CODE (type0) == INTEGER_TYPE
	    && TREE_CODE (TREE_TYPE (type1)) == INTEGER_TYPE)
	  {
	    if (unsafe_conversion_p (TREE_TYPE (type1), op0,
				     NULL_TREE, false))
	      {
		if (complain)
		  error_at (loc, "conversion of scalar %qT to vector %qT "
			    "involves truncation", type0, type1);
		return stv_error;
	      }
	    return ret;
	  }
	else if (!integer_only_op
		    /* Allow integer --> real conversion if safe.  */
		 && (TREE_CODE (type0) == REAL_TYPE
		     || TREE_CODE (type0) == INTEGER_TYPE)
		 && SCALAR_FLOAT_TYPE_P (TREE_TYPE (type1)))
	  {
	    if (unsafe_conversion_p (TREE_TYPE (type1), op0,
				     NULL_TREE, false))
	      {
		if (complain)
		  error_at (loc, "conversion of scalar %qT to vector %qT "
			    "involves truncation", type0, type1);
		return stv_error;
	      }
	    return ret;
	  }
      default:
	break;
    }

  return stv_nothing;
}

/* Return the alignment of std::max_align_t.

   [support.types.layout] The type max_align_t is a POD type whose alignment
   requirement is at least as great as that of every scalar type, and whose
   alignment requirement is supported in every context.  */

unsigned
max_align_t_align ()
{
  unsigned int max_align = MAX (TYPE_ALIGN (long_long_integer_type_node),
				TYPE_ALIGN (long_double_type_node));
  if (float128_type_node != NULL_TREE)
    max_align = MAX (max_align, TYPE_ALIGN (float128_type_node));
  return max_align;
}

/* Return true iff ALIGN is an integral constant that is a fundamental
   alignment, as defined by [basic.align] in the c++-11
   specifications.

   That is:

       [A fundamental alignment is represented by an alignment less than or
        equal to the greatest alignment supported by the implementation
        in all contexts, which is equal to alignof(max_align_t)].  */

bool
cxx_fundamental_alignment_p (unsigned align)
{
  return (align <= max_align_t_align ());
}

/* Return true if T is a pointer to a zero-sized aggregate.  */

bool
pointer_to_zero_sized_aggr_p (tree t)
{
  if (!POINTER_TYPE_P (t))
    return false;
  t = TREE_TYPE (t);
  return (TYPE_SIZE (t) && integer_zerop (TYPE_SIZE (t)));
}

/* For an EXPR of a FUNCTION_TYPE that references a GCC built-in function
   with no library fallback or for an ADDR_EXPR whose operand is such type
   issues an error pointing to the location LOC.
   Returns true when the expression has been diagnosed and false
   otherwise.  */

bool
reject_gcc_builtin (const_tree expr, location_t loc /* = UNKNOWN_LOCATION */)
{
  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);

  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_TYPE (expr)
      && TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE
      && TREE_CODE (expr) == FUNCTION_DECL
      /* The intersection of DECL_BUILT_IN and DECL_IS_UNDECLARED_BUILTIN avoids
	 false positives for user-declared built-ins such as abs or
	 strlen, and for C++ operators new and delete.
	 The c_decl_implicit() test avoids false positives for implicitly
	 declared built-ins with library fallbacks (such as abs).  */
      && fndecl_built_in_p (expr)
      && DECL_IS_UNDECLARED_BUILTIN (expr)
      && !c_decl_implicit (expr)
      && !DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      if (loc == UNKNOWN_LOCATION)
	loc = EXPR_LOC_OR_LOC (expr, input_location);

      /* Reject arguments that are built-in functions with
	 no library fallback.  */
      error_at (loc, "built-in function %qE must be directly called", expr);

      return true;
    }

  return false;
}

/* Issue an ERROR for an invalid SIZE of array NAME which is null
   for unnamed arrays.  */

void
invalid_array_size_error (location_t loc, cst_size_error error,
			  const_tree size, const_tree name)
{
  tree maxsize = max_object_size ();
  switch (error)
    {
    case cst_size_not_constant:
      if (name)
	error_at (loc, "size of array %qE is not a constant expression",
		  name);
      else
	error_at (loc, "size of array is not a constant expression");
      break;
    case cst_size_negative:
      if (name)
	error_at (loc, "size %qE of array %qE is negative",
		  size, name);
      else
	error_at (loc, "size %qE of array is negative",
		  size);
      break;
    case cst_size_too_big:
      if (name)
	error_at (loc, "size %qE of array %qE exceeds maximum "
		  "object size %qE", size, name, maxsize);
      else
	error_at (loc, "size %qE of array exceeds maximum "
		  "object size %qE", size, maxsize);
      break;
    case cst_size_overflow:
      if (name)
	error_at (loc, "size of array %qE exceeds maximum "
		  "object size %qE", name, maxsize);
      else
	error_at (loc, "size of array exceeds maximum "
		  "object size %qE", maxsize);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Check if array size calculations overflow or if the array covers more
   than half of the address space.  Return true if the size of the array
   is valid, false otherwise.  T is either the type of the array or its
   size, and NAME is the name of the array, or null for unnamed arrays.  */

bool
valid_array_size_p (location_t loc, const_tree t, tree name, bool complain)
{
  if (t == error_mark_node)
    return true;

  const_tree size;
  if (TYPE_P (t))
    {
      if (!COMPLETE_TYPE_P (t))
	return true;
      size = TYPE_SIZE_UNIT (t);
    }
  else
    size = t;

  if (TREE_CODE (size) != INTEGER_CST)
    return true;

  cst_size_error error;
  if (valid_constant_size_p (size, &error))
    return true;

  if (!complain)
    return false;

  if (TREE_CODE (TREE_TYPE (size)) == ENUMERAL_TYPE)
    /* Show the value of the enumerator rather than its name.  */
    size = convert (ssizetype, const_cast<tree> (size));

  invalid_array_size_error (loc, error, size, name);
  return false;
}

/* Read SOURCE_DATE_EPOCH from environment to have a deterministic
   timestamp to replace embedded current dates to get reproducible
   results.  Returns -1 if SOURCE_DATE_EPOCH is not defined.  */

time_t
cb_get_source_date_epoch (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  char *source_date_epoch;
  int64_t epoch;
  char *endptr;

  source_date_epoch = getenv ("SOURCE_DATE_EPOCH");
  if (!source_date_epoch)
    return (time_t) -1;

  errno = 0;
#if defined(INT64_T_IS_LONG)
  epoch = strtol (source_date_epoch, &endptr, 10);
#else
  epoch = strtoll (source_date_epoch, &endptr, 10);
#endif
  if (errno != 0 || endptr == source_date_epoch || *endptr != '\0'
      || epoch < 0 || epoch > MAX_SOURCE_DATE_EPOCH)
    {
      error_at (input_location, "environment variable %qs must "
	        "expand to a non-negative integer less than or equal to %wd",
		"SOURCE_DATE_EPOCH", MAX_SOURCE_DATE_EPOCH);
      return (time_t) -1;
    }

  return (time_t) epoch;
}

/* Callback for libcpp for offering spelling suggestions for misspelled
   directives.  GOAL is an unrecognized string; CANDIDATES is a
   NULL-terminated array of candidate strings.  Return the closest
   match to GOAL within CANDIDATES, or NULL if none are good
   suggestions.  */

const char *
cb_get_suggestion (cpp_reader *, const char *goal,
		   const char *const *candidates)
{
  best_match<const char *, const char *> bm (goal);
  while (*candidates)
    bm.consider (*candidates++);
  return bm.get_best_meaningful_candidate ();
}

/* Return the latice point which is the wider of the two FLT_EVAL_METHOD
   modes X, Y.  This isn't just  >, as the FLT_EVAL_METHOD values added
   by C TS 18661-3 for interchange  types that are computed in their
   native precision are larger than the C11 values for evaluating in the
   precision of float/double/long double.  If either mode is
   FLT_EVAL_METHOD_UNPREDICTABLE, return that.  */

enum flt_eval_method
excess_precision_mode_join (enum flt_eval_method x,
			    enum flt_eval_method y)
{
  if (x == FLT_EVAL_METHOD_UNPREDICTABLE
      || y == FLT_EVAL_METHOD_UNPREDICTABLE)
    return FLT_EVAL_METHOD_UNPREDICTABLE;

  /* GCC only supports one interchange type right now, _Float16.  If
     we're evaluating _Float16 in 16-bit precision, then flt_eval_method
     will be FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16.  */
  if (x == FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16)
    return y;
  if (y == FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16)
    return x;

  /* Other values for flt_eval_method are directly comparable, and we want
     the maximum.  */
  return MAX (x, y);
}

/* Return the value that should be set for FLT_EVAL_METHOD in the
   context of ISO/IEC TS 18861-3.

   This relates to the effective excess precision seen by the user,
   which is the join point of the precision the target requests for
   -fexcess-precision={standard,fast} and the implicit excess precision
   the target uses.  */

static enum flt_eval_method
c_ts18661_flt_eval_method (void)
{
  enum flt_eval_method implicit
    = targetm.c.excess_precision (EXCESS_PRECISION_TYPE_IMPLICIT);

  enum excess_precision_type flag_type
    = (flag_excess_precision == EXCESS_PRECISION_STANDARD
       ? EXCESS_PRECISION_TYPE_STANDARD
       : EXCESS_PRECISION_TYPE_FAST);

  enum flt_eval_method requested
    = targetm.c.excess_precision (flag_type);

  return excess_precision_mode_join (implicit, requested);
}

/* As c_cpp_ts18661_flt_eval_method, but clamps the expected values to
   those that were permitted by C11.  That is to say, eliminates
   FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16.  */

static enum flt_eval_method
c_c11_flt_eval_method (void)
{
  return excess_precision_mode_join (c_ts18661_flt_eval_method (),
				     FLT_EVAL_METHOD_PROMOTE_TO_FLOAT);
}

/* Return the value that should be set for FLT_EVAL_METHOD.
   MAYBE_C11_ONLY_P is TRUE if we should check
   FLAG_PERMITTED_EVAL_METHODS as to whether we should limit the possible
   values we can return to those from C99/C11, and FALSE otherwise.
   See the comments on c_ts18661_flt_eval_method for what value we choose
   to set here.  */

int
c_flt_eval_method (bool maybe_c11_only_p)
{
  if (maybe_c11_only_p
      && flag_permitted_flt_eval_methods
	  == PERMITTED_FLT_EVAL_METHODS_C11)
    return c_c11_flt_eval_method ();
  else
    return c_ts18661_flt_eval_method ();
}

/* An enum for get_missing_token_insertion_kind for describing the best
   place to insert a missing token, if there is one.  */

enum missing_token_insertion_kind
{
  MTIK_IMPOSSIBLE,
  MTIK_INSERT_BEFORE_NEXT,
  MTIK_INSERT_AFTER_PREV
};

/* Given a missing token of TYPE, determine if it is reasonable to
   emit a fix-it hint suggesting the insertion of the token, and,
   if so, where the token should be inserted relative to other tokens.

   It only makes sense to do this for values of TYPE that are symbols.

   Some symbols should go before the next token, e.g. in:
     if flag)
   we want to insert the missing '(' immediately before "flag",
   giving:
     if (flag)
   rather than:
     if( flag)
   These use MTIK_INSERT_BEFORE_NEXT.

   Other symbols should go after the previous token, e.g. in:
     if (flag
       do_something ();
   we want to insert the missing ')' immediately after the "flag",
   giving:
     if (flag)
       do_something ();
   rather than:
     if (flag
       )do_something ();
   These use MTIK_INSERT_AFTER_PREV.  */

static enum missing_token_insertion_kind
get_missing_token_insertion_kind (enum cpp_ttype type)
{
  switch (type)
    {
      /* Insert missing "opening" brackets immediately
	 before the next token.  */
    case CPP_OPEN_SQUARE:
    case CPP_OPEN_PAREN:
      return MTIK_INSERT_BEFORE_NEXT;

      /* Insert other missing symbols immediately after
	 the previous token.  */
    case CPP_CLOSE_PAREN:
    case CPP_CLOSE_SQUARE:
    case CPP_SEMICOLON:
    case CPP_COMMA:
    case CPP_COLON:
      return MTIK_INSERT_AFTER_PREV;

      /* Other kinds of token don't get fix-it hints.  */
    default:
      return MTIK_IMPOSSIBLE;
    }
}

/* Given RICHLOC, a location for a diagnostic describing a missing token
   of kind TOKEN_TYPE, potentially add a fix-it hint suggesting the
   insertion of the token.

   The location of the attempted fix-it hint depends on TOKEN_TYPE:
   it will either be:
     (a) immediately after PREV_TOKEN_LOC, or

     (b) immediately before the primary location within RICHLOC (taken to
	 be that of the token following where the token was expected).

   If we manage to add a fix-it hint, then the location of the
   fix-it hint is likely to be more useful as the primary location
   of the diagnostic than that of the following token, so we swap
   these locations.

   For example, given this bogus code:
       123456789012345678901234567890
   1 | int missing_semicolon (void)
   2 | {
   3 |   return 42
   4 | }

   we will emit:

     "expected ';' before '}'"

   RICHLOC's primary location is at the closing brace, so before "swapping"
   we would emit the error at line 4 column 1:

       123456789012345678901234567890
   3 |   return 42  |< fix-it hint emitted for this line
     |            ; |
   4 | }            |< "expected ';' before '}'" emitted at this line
     | ^            |

   It's more useful for the location of the diagnostic to be at the
   fix-it hint, so we swap the locations, so the primary location
   is at the fix-it hint, with the old primary location inserted
   as a secondary location, giving this, with the error at line 3
   column 12:

       123456789012345678901234567890
   3 |   return 42   |< "expected ';' before '}'" emitted at this line,
     |            ^  |   with fix-it hint
   4 |            ;  |
     | }             |< secondary range emitted here
     | ~             |.  */

void
maybe_suggest_missing_token_insertion (rich_location *richloc,
				       enum cpp_ttype token_type,
				       location_t prev_token_loc)
{
  gcc_assert (richloc);

  enum missing_token_insertion_kind mtik
    = get_missing_token_insertion_kind (token_type);

  switch (mtik)
    {
    default:
      gcc_unreachable ();
      break;

    case MTIK_IMPOSSIBLE:
      return;

    case MTIK_INSERT_BEFORE_NEXT:
      /* Attempt to add the fix-it hint before the primary location
	 of RICHLOC.  */
      richloc->add_fixit_insert_before (cpp_type2name (token_type, 0));
      break;

    case MTIK_INSERT_AFTER_PREV:
      /* Attempt to add the fix-it hint after PREV_TOKEN_LOC.  */
      richloc->add_fixit_insert_after (prev_token_loc,
				       cpp_type2name (token_type, 0));
      break;
    }

  /* If we were successful, use the fix-it hint's location as the
     primary location within RICHLOC, adding the old primary location
     back as a secondary location.  */
  if (!richloc->seen_impossible_fixit_p ())
    {
      fixit_hint *hint = richloc->get_last_fixit_hint ();
      location_t hint_loc = hint->get_start_loc ();
      location_t old_loc = richloc->get_loc ();

      richloc->set_range (0, hint_loc, SHOW_RANGE_WITH_CARET);
      richloc->add_range (old_loc);
    }
}

#if CHECKING_P

namespace selftest {

/* Verify that fold_for_warn on error_mark_node is safe.  */

static void
test_fold_for_warn ()
{
  ASSERT_EQ (error_mark_node, fold_for_warn (error_mark_node));
}

/* Run all of the selftests within this file.  */

static void
c_common_c_tests ()
{
  test_fold_for_warn ();
}

/* Run all of the tests within c-family.  */

void
c_family_tests (void)
{
  c_common_c_tests ();
  c_format_c_tests ();
  c_indentation_c_tests ();
  c_pretty_print_c_tests ();
  c_spellcheck_cc_tests ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

/* Attempt to locate a suitable location within FILE for a
   #include directive to be inserted before.  
   LOC is the location of the relevant diagnostic.

   Attempt to return the location within FILE immediately
   after the last #include within that file, or the start of
   that file if it has no #include directives.

   Return UNKNOWN_LOCATION if no suitable location is found,
   or if an error occurs.  */

static location_t
try_to_locate_new_include_insertion_point (const char *file, location_t loc)
{
  /* Locate the last ordinary map within FILE that ended with a #include.  */
  const line_map_ordinary *last_include_ord_map = NULL;

  /* ...and the next ordinary map within FILE after that one.  */
  const line_map_ordinary *last_ord_map_after_include = NULL;

  /* ...and the first ordinary map within FILE.  */
  const line_map_ordinary *first_ord_map_in_file = NULL;

  /*  Get ordinary map containing LOC (or its expansion).  */
  const line_map_ordinary *ord_map_for_loc = NULL;
  linemap_resolve_location (line_table, loc, LRK_MACRO_EXPANSION_POINT,
			    &ord_map_for_loc);
  gcc_assert (ord_map_for_loc);

  for (unsigned int i = 0; i < LINEMAPS_ORDINARY_USED (line_table); i++)
    {
      const line_map_ordinary *ord_map
	= LINEMAPS_ORDINARY_MAP_AT (line_table, i);

      if (const line_map_ordinary *from
	  = linemap_included_from_linemap (line_table, ord_map))
	/* We cannot use pointer equality, because with preprocessed
	   input all filename strings are unique.  */
	if (0 == strcmp (from->to_file, file))
	  {
	    last_include_ord_map = from;
	    last_ord_map_after_include = NULL;
	  }

      /* Likewise, use strcmp, and reject any line-zero introductory
	 map.  */
      if (ord_map->to_line && 0 == strcmp (ord_map->to_file, file))
	{
	  if (!first_ord_map_in_file)
	    first_ord_map_in_file = ord_map;
	  if (last_include_ord_map && !last_ord_map_after_include)
	    last_ord_map_after_include = ord_map;
	}

      /* Stop searching when reaching the ord_map containing LOC,
	 as it makes no sense to provide fix-it hints that appear
	 after the diagnostic in question.  */
      if (ord_map == ord_map_for_loc)
	break;
    }

  /* Determine where to insert the #include.  */
  const line_map_ordinary *ord_map_for_insertion;

  /* We want the next ordmap in the file after the last one that's a
     #include, but failing that, the start of the file.  */
  if (last_ord_map_after_include)
    ord_map_for_insertion = last_ord_map_after_include;
  else
    ord_map_for_insertion = first_ord_map_in_file;

  if (!ord_map_for_insertion)
    return UNKNOWN_LOCATION;

  /* The "start_location" is column 0, meaning "the whole line".
     rich_location and edit_context can't cope with this, so use
     column 1 instead.  */
  location_t col_0 = ord_map_for_insertion->start_location;
  return linemap_position_for_loc_and_offset (line_table, col_0, 1);
}

/* A map from filenames to sets of headers added to them, for
   ensuring idempotency within maybe_add_include_fixit.  */

/* The values within the map.  We need string comparison as there's
   no guarantee that two different diagnostics that are recommending
   adding e.g. "<stdio.h>" are using the same buffer.  */

typedef hash_set <const char *, false, nofree_string_hash> per_file_includes_t;

/* The map itself.  We don't need string comparison for the filename keys,
   as they come from libcpp.  */

typedef hash_map <const char *, per_file_includes_t *> added_includes_t;
static added_includes_t *added_includes;

/* Attempt to add a fix-it hint to RICHLOC, adding "#include HEADER\n"
   in a suitable location within the file of RICHLOC's primary
   location.

   This function is idempotent: a header will be added at most once to
   any given file.

   If OVERRIDE_LOCATION is true, then if a fix-it is added and will be
   printed, then RICHLOC's primary location will be replaced by that of
   the fix-it hint (for use by "inform" notes where the location of the
   issue has already been reported).  */

void
maybe_add_include_fixit (rich_location *richloc, const char *header,
			 bool override_location)
{
  location_t loc = richloc->get_loc ();
  const char *file = LOCATION_FILE (loc);
  if (!file)
    return;

  /* Idempotency: don't add the same header more than once to a given file.  */
  if (!added_includes)
    added_includes = new added_includes_t ();
  per_file_includes_t *&set = added_includes->get_or_insert (file);
  if (set)
    if (set->contains (header))
      /* ...then we've already added HEADER to that file.  */
      return;
  if (!set)
    set = new per_file_includes_t ();
  set->add (header);

  /* Attempt to locate a suitable place for the new directive.  */
  location_t include_insert_loc
    = try_to_locate_new_include_insertion_point (file, loc);
  if (include_insert_loc == UNKNOWN_LOCATION)
    return;

  char *text = xasprintf ("#include %s\n", header);
  richloc->add_fixit_insert_before (include_insert_loc, text);
  free (text);

  if (override_location && global_dc->show_caret)
    {
      /* Replace the primary location with that of the insertion point for the
	 fix-it hint.

	 We use SHOW_LINES_WITHOUT_RANGE so that we don't meaningless print a
	 caret for the insertion point (or colorize it).

	 Hence we print e.g.:

	 ../x86_64-pc-linux-gnu/libstdc++-v3/include/vector:74:1: note: msg 2
	  73 | # include <debug/vector>
	 +++ |+#include <vector>
	  74 | #endif

	 rather than:

	 ../x86_64-pc-linux-gnu/libstdc++-v3/include/vector:74:1: note: msg 2
	  73 | # include <debug/vector>
	 +++ |+#include <vector>
	  74 | #endif
	     | ^

	 avoiding the caret on the first column of line 74.  */
      richloc->set_range (0, include_insert_loc, SHOW_LINES_WITHOUT_RANGE);
    }
}

/* Attempt to convert a braced array initializer list CTOR for array
   TYPE into a STRING_CST for convenience and efficiency.  Return
   the converted string on success or the original ctor on failure.  */

static tree
braced_list_to_string (tree type, tree ctor, bool member)
{
  /* Ignore non-members with unknown size like arrays with unspecified
     bound.  */
  tree typesize = TYPE_SIZE_UNIT (type);
  if (!member && !tree_fits_uhwi_p (typesize))
    return ctor;

  /* If the array has an explicit bound, use it to constrain the size
     of the string.  If it doesn't, be sure to create a string that's
     as long as implied by the index of the last zero specified via
     a designator, as in:
       const char a[] = { [7] = 0 };  */
  unsigned HOST_WIDE_INT maxelts;
  if (typesize)
    {
      maxelts = tree_to_uhwi (typesize);
      maxelts /= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (type)));
    }
  else
    maxelts = HOST_WIDE_INT_M1U;

  /* Avoid converting initializers for zero-length arrays (but do
     create them for flexible array members).  */
  if (!maxelts)
    return ctor;

  unsigned HOST_WIDE_INT nelts = CONSTRUCTOR_NELTS (ctor);

  auto_vec<char> str;
  str.reserve (nelts + 1);

  unsigned HOST_WIDE_INT i;
  tree index, value;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), i, index, value)
    {
      unsigned HOST_WIDE_INT idx = i;
      if (index)
	{
	  if (!tree_fits_uhwi_p (index))
	    return ctor;
	  idx = tree_to_uhwi (index);
	}

      /* auto_vec is limited to UINT_MAX elements.  */
      if (idx > UINT_MAX)
	return ctor;

     /* Avoid non-constant initializers.  */
     if (!tree_fits_shwi_p (value))
	return ctor;

      /* Skip over embedded nuls except the last one (initializer
	 elements are in ascending order of indices).  */
      HOST_WIDE_INT val = tree_to_shwi (value);
      if (!val && i + 1 < nelts)
	continue;

      if (idx < str.length())
	return ctor;

      /* Bail if the CTOR has a block of more than 256 embedded nuls
	 due to implicitly initialized elements.  */
      unsigned nchars = (idx - str.length ()) + 1;
      if (nchars > 256)
	return ctor;

      if (nchars > 1)
	{
	  str.reserve (idx);
	  str.quick_grow_cleared (idx);
	}

      if (idx >= maxelts)
	return ctor;

      str.safe_insert (idx, val);
    }

  /* Append a nul string termination.  */
  if (maxelts != HOST_WIDE_INT_M1U && str.length () < maxelts)
    str.safe_push (0);

  /* Build a STRING_CST with the same type as the array.  */
  tree res = build_string (str.length (), str.begin ());
  TREE_TYPE (res) = type;
  return res;
}

/* Implementation of the two-argument braced_lists_to_string withe
   the same arguments plus MEMBER which is set for struct members
   to allow initializers for flexible member arrays.  */

static tree
braced_lists_to_strings (tree type, tree ctor, bool member)
{
  if (TREE_CODE (ctor) != CONSTRUCTOR)
    return ctor;

  tree_code code = TREE_CODE (type);

  tree ttp;
  if (code == ARRAY_TYPE)
    ttp = TREE_TYPE (type);
  else if (code == RECORD_TYPE)
    {
      ttp = TREE_TYPE (ctor);
      if (TREE_CODE (ttp) == ARRAY_TYPE)
	{
	  type = ttp;
	  ttp = TREE_TYPE (ttp);
	}
    }
  else
    return ctor;

  if ((TREE_CODE (ttp) == ARRAY_TYPE || TREE_CODE (ttp) == INTEGER_TYPE)
      && TYPE_STRING_FLAG (ttp))
    return braced_list_to_string (type, ctor, member);

  code = TREE_CODE (ttp);
  if (code == ARRAY_TYPE || RECORD_OR_UNION_TYPE_P (ttp))
    {
      bool rec = RECORD_OR_UNION_TYPE_P (ttp);

      /* Handle array of arrays or struct member initializers.  */
      tree val;
      unsigned HOST_WIDE_INT idx;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), idx, val)
	{
	  val = braced_lists_to_strings (ttp, val, rec);
	  CONSTRUCTOR_ELT (ctor, idx)->value = val;
	}
    }

  return ctor;
}

/* Attempt to convert a CTOR containing braced array initializer lists
   for array TYPE into one containing STRING_CSTs, for convenience and
   efficiency.  Recurse for arrays of arrays and member initializers.
   Return the converted CTOR or STRING_CST on success or the original
   CTOR otherwise.  */

tree
braced_lists_to_strings (tree type, tree ctor)
{
  return braced_lists_to_strings (type, ctor, false);
}


/* Emit debug for functions before finalizing early debug.  */

void
c_common_finalize_early_debug (void)
{
  /* Emit early debug for reachable functions, and by consequence,
     locally scoped symbols.  Also emit debug for extern declared
     functions that are still reachable at this point.  */
  struct cgraph_node *cnode;
  FOR_EACH_FUNCTION (cnode)
    if (!cnode->alias && !cnode->thunk
	&& (cnode->has_gimple_body_p ()
	    || !DECL_IS_UNDECLARED_BUILTIN (cnode->decl)))
      (*debug_hooks->early_global_decl) (cnode->decl);
}

#include "gt-c-family-c-common.h"
