/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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


/* This file is the lexical analyzer for GNU C++.  */

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

#include "config.h"
#include "system.h"
#include "input.h"
#include "tree.h"
#include "cp-tree.h"
#include "cpplib.h"
#include "c-lex.h"
#include "lex.h"
#include "parse.h"
#include "flags.h"
#include "c-pragma.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"
#include "tm_p.h"
#include "timevar.h"
#include "diagnostic.h"

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif

extern void yyprint PARAMS ((FILE *, int, YYSTYPE));

static int interface_strcmp PARAMS ((const char *));
static int *init_cpp_parse PARAMS ((void));
static void init_cp_pragma PARAMS ((void));

static tree parse_strconst_pragma PARAMS ((const char *, int));
static void handle_pragma_vtable PARAMS ((cpp_reader *));
static void handle_pragma_unit PARAMS ((cpp_reader *));
static void handle_pragma_interface PARAMS ((cpp_reader *));
static void handle_pragma_implementation PARAMS ((cpp_reader *));
static void handle_pragma_java_exceptions PARAMS ((cpp_reader *));

#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
static int reduce_cmp PARAMS ((int *, int *));
static int token_cmp PARAMS ((int *, int *));
#endif
#endif
static int is_global PARAMS ((tree));
static void init_operators PARAMS ((void));
static void copy_lang_type PARAMS ((tree));

/* A constraint that can be tested at compile time.  */
#ifdef __STDC__
#define CONSTRAINT(name, expr) extern int constraint_##name [(expr) ? 1 : -1]
#else
#define CONSTRAINT(name, expr) extern int constraint_/**/name [(expr) ? 1 : -1]
#endif

#include "cpplib.h"

extern int yychar;		/*  the lookahead symbol		*/
extern YYSTYPE yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/

/* These flags are used by c-lex.c.  In C++, they're always off and on,
   respectively.  */
int warn_traditional = 0;
int flag_digraphs = 1;

/* the declaration found for the last IDENTIFIER token read in.  yylex
   must look this up to detect typedefs, which get token type
   tTYPENAME, so it is left around in case the identifier is not a
   typedef but is used in a context which makes it a reference to a
   variable.  */
tree lastiddecl;

/* Array for holding counts of the numbers of tokens seen.  */
extern int *token_count;

/* Functions and data structures for #pragma interface.

   `#pragma implementation' means that the main file being compiled
   is considered to implement (provide) the classes that appear in
   its main body.  I.e., if this is file "foo.cc", and class `bar'
   is defined in "foo.cc", then we say that "foo.cc implements bar".

   All main input files "implement" themselves automagically.

   `#pragma interface' means that unless this file (of the form "foo.h"
   is not presently being included by file "foo.cc", the
   CLASSTYPE_INTERFACE_ONLY bit gets set.  The effect is that none
   of the vtables nor any of the inline functions defined in foo.h
   will ever be output.

   There are cases when we want to link files such as "defs.h" and
   "main.cc".  In this case, we give "defs.h" a `#pragma interface',
   and "main.cc" has `#pragma implementation "defs.h"'.  */

struct impl_files
{
  const char *filename;
  struct impl_files *next;
};

static struct impl_files *impl_file_chain;


/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   CV_QUALIFIERS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers.

   We return an INDIRECT_REF whose "contents" are TARGET
   and whose type is the modifier list.  */

tree
make_pointer_declarator (cv_qualifiers, target)
     tree cv_qualifiers, target;
{
  if (target && TREE_CODE (target) == IDENTIFIER_NODE
      && ANON_AGGRNAME_P (target))
    error ("type name expected before `*'");
  target = build_nt (INDIRECT_REF, target);
  TREE_TYPE (target) = cv_qualifiers;
  return target;
}

/* Return something to represent absolute declarators containing a &.
   TARGET is the absolute declarator that the & contains.
   CV_QUALIFIERS is a list of modifiers such as const or volatile
   to apply to the reference type, represented as identifiers.

   We return an ADDR_EXPR whose "contents" are TARGET
   and whose type is the modifier list.  */

tree
make_reference_declarator (cv_qualifiers, target)
     tree cv_qualifiers, target;
{
  if (target)
    {
      if (TREE_CODE (target) == ADDR_EXPR)
	{
	  error ("cannot declare references to references");
	  return target;
	}
      if (TREE_CODE (target) == INDIRECT_REF)
	{
	  error ("cannot declare pointers to references");
	  return target;
	}
      if (TREE_CODE (target) == IDENTIFIER_NODE && ANON_AGGRNAME_P (target))
	  error ("type name expected before `&'");
    }
  target = build_nt (ADDR_EXPR, target);
  TREE_TYPE (target) = cv_qualifiers;
  return target;
}

tree
make_call_declarator (target, parms, cv_qualifiers, exception_specification)
     tree target, parms, cv_qualifiers, exception_specification;
{
  target = build_nt (CALL_EXPR, target,
		     tree_cons (parms, cv_qualifiers, NULL_TREE),
		     /* The third operand is really RTL.  We
			shouldn't put anything there.  */
		     NULL_TREE);
  CALL_DECLARATOR_EXCEPTION_SPEC (target) = exception_specification;
  return target;
}

void
set_quals_and_spec (call_declarator, cv_qualifiers, exception_specification)
     tree call_declarator, cv_qualifiers, exception_specification;
{
  CALL_DECLARATOR_QUALS (call_declarator) = cv_qualifiers;
  CALL_DECLARATOR_EXCEPTION_SPEC (call_declarator) = exception_specification;
}

int interface_only;		/* whether or not current file is only for
				   interface definitions.  */
int interface_unknown;		/* whether or not we know this class
				   to behave according to #pragma interface.  */

/* Tree code classes. */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

static const char cplus_tree_code_type[] = {
  'x',
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

static const int cplus_tree_code_length[] = {
  0,
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

static const char *const cplus_tree_code_name[] = {
  "@@dummy",
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Initialization before switch parsing.  */
void
cxx_init_options ()
{
  c_common_init_options (clk_cplusplus);

  /* Default exceptions on.  */
  flag_exceptions = 1;
  /* By default wrap lines at 80 characters.  Is getenv ("COLUMNS")
     preferable?  */
  diagnostic_line_cutoff (global_dc) = 80;
  /* By default, emit location information once for every
     diagnostic message.  */
  diagnostic_prefixing_rule (global_dc) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
}

void
cxx_finish ()
{
  c_common_finish ();
}

static int *
init_cpp_parse ()
{
#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
  reduce_count = (int *) xcalloc (sizeof (int), (REDUCE_LENGTH + 1));
  reduce_count += 1;
  token_count = (int *) xcalloc (sizeof (int), (TOKEN_LENGTH + 1));
  token_count += 1;
#endif
#endif
  return token_count;
}

/* A mapping from tree codes to operator name information.  */
operator_name_info_t operator_name_info[(int) LAST_CPLUS_TREE_CODE];
/* Similar, but for assignment operators.  */
operator_name_info_t assignment_operator_name_info[(int) LAST_CPLUS_TREE_CODE];

/* Initialize data structures that keep track of operator names.  */

#define DEF_OPERATOR(NAME, C, M, AR, AP) \
 CONSTRAINT (C, sizeof "operator " + sizeof NAME <= 256);
#include "operators.def"
#undef DEF_OPERATOR

static void
init_operators ()
{
  tree identifier;
  char buffer[256];
  struct operator_name_info_t *oni;

#define DEF_OPERATOR(NAME, CODE, MANGLING, ARITY, ASSN_P)		    \
  sprintf (buffer, ISALPHA (NAME[0]) ? "operator %s" : "operator%s", NAME); \
  identifier = get_identifier (buffer);					    \
  IDENTIFIER_OPNAME_P (identifier) = 1;					    \
									    \
  oni = (ASSN_P								    \
	 ? &assignment_operator_name_info[(int) CODE]			    \
	 : &operator_name_info[(int) CODE]);				    \
  oni->identifier = identifier;						    \
  oni->name = NAME;							    \
  oni->mangled_name = MANGLING;

#include "operators.def"
#undef DEF_OPERATOR

  operator_name_info[(int) ERROR_MARK].identifier
    = get_identifier ("<invalid operator>");

  /* Handle some special cases.  These operators are not defined in
     the language, but can be produced internally.  We may need them
     for error-reporting.  (Eventually, we should ensure that this
     does not happen.  Error messages involving these operators will
     be confusing to users.)  */

  operator_name_info [(int) INIT_EXPR].name
    = operator_name_info [(int) MODIFY_EXPR].name;
  operator_name_info [(int) EXACT_DIV_EXPR].name = "(ceiling /)";
  operator_name_info [(int) CEIL_DIV_EXPR].name = "(ceiling /)";
  operator_name_info [(int) FLOOR_DIV_EXPR].name = "(floor /)";
  operator_name_info [(int) ROUND_DIV_EXPR].name = "(round /)";
  operator_name_info [(int) CEIL_MOD_EXPR].name = "(ceiling %)";
  operator_name_info [(int) FLOOR_MOD_EXPR].name = "(floor %)";
  operator_name_info [(int) ROUND_MOD_EXPR].name = "(round %)";
  operator_name_info [(int) ABS_EXPR].name = "abs";
  operator_name_info [(int) FFS_EXPR].name = "ffs";
  operator_name_info [(int) BIT_ANDTC_EXPR].name = "&~";
  operator_name_info [(int) TRUTH_AND_EXPR].name = "strict &&";
  operator_name_info [(int) TRUTH_OR_EXPR].name = "strict ||";
  operator_name_info [(int) IN_EXPR].name = "in";
  operator_name_info [(int) RANGE_EXPR].name = "...";
  operator_name_info [(int) CONVERT_EXPR].name = "+";

  assignment_operator_name_info [(int) EXACT_DIV_EXPR].name
    = "(exact /=)";
  assignment_operator_name_info [(int) CEIL_DIV_EXPR].name
    = "(ceiling /=)";
  assignment_operator_name_info [(int) FLOOR_DIV_EXPR].name
    = "(floor /=)";
  assignment_operator_name_info [(int) ROUND_DIV_EXPR].name
    = "(round /=)";
  assignment_operator_name_info [(int) CEIL_MOD_EXPR].name
    = "(ceiling %=)";
  assignment_operator_name_info [(int) FLOOR_MOD_EXPR].name
    = "(floor %=)";
  assignment_operator_name_info [(int) ROUND_MOD_EXPR].name
    = "(round %=)";
}

/* The reserved keyword table.  */
struct resword
{
  const char *const word;
  const ENUM_BITFIELD(rid) rid : 16;
  const unsigned int disable   : 16;
};

/* Disable mask.  Keywords are disabled if (reswords[i].disable & mask) is
   _true_.  */
#define D_EXT		0x01	/* GCC extension */
#define D_ASM		0x02	/* in C99, but has a switch to turn it off */
#define D_OPNAME	0x04	/* operator names */

CONSTRAINT(ridbits_fit, RID_LAST_MODIFIER < sizeof(unsigned long) * CHAR_BIT);

static const struct resword reswords[] =
{
  { "_Complex",		RID_COMPLEX,	0 },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof", 	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME,	0 },
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
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	D_ASM },
  { "and",		RID_AND,	D_OPNAME },
  { "and_eq",		RID_AND_EQ,	D_OPNAME },
  { "auto",		RID_AUTO,	0 },
  { "bitand",		RID_BITAND,	D_OPNAME },
  { "bitor",		RID_BITOR,	D_OPNAME },
  { "bool",		RID_BOOL,	0 },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "catch",		RID_CATCH,	0 },
  { "char",		RID_CHAR,	0 },
  { "class",		RID_CLASS,	0 },
  { "compl",		RID_COMPL,	D_OPNAME },
  { "const",		RID_CONST,	0 },
  { "const_cast",	RID_CONSTCAST,	0 },
  { "continue",		RID_CONTINUE,	0 },
  { "default",		RID_DEFAULT,	0 },
  { "delete",		RID_DELETE,	0 },
  { "do",		RID_DO,		0 },
  { "double",		RID_DOUBLE,	0 },
  { "dynamic_cast",	RID_DYNCAST,	0 },
  { "else",		RID_ELSE,	0 },
  { "enum",		RID_ENUM,	0 },
  { "explicit",		RID_EXPLICIT,	0 },
  { "export",		RID_EXPORT,	0 },
  { "extern",		RID_EXTERN,	0 },
  { "false",		RID_FALSE,	0 },
  { "float",		RID_FLOAT,	0 },
  { "for",		RID_FOR,	0 },
  { "friend",		RID_FRIEND,	0 },
  { "goto",		RID_GOTO,	0 },
  { "if",		RID_IF,		0 },
  { "inline",		RID_INLINE,	0 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "mutable",		RID_MUTABLE,	0 },
  { "namespace",	RID_NAMESPACE,	0 },
  { "new",		RID_NEW,	0 },
  { "not",		RID_NOT,	D_OPNAME },
  { "not_eq",		RID_NOT_EQ,	D_OPNAME },
  { "operator",		RID_OPERATOR,	0 },
  { "or",		RID_OR,		D_OPNAME },
  { "or_eq",		RID_OR_EQ,	D_OPNAME },
  { "private",		RID_PRIVATE,	0 },
  { "protected",	RID_PROTECTED,	0 },
  { "public",		RID_PUBLIC,	0 },
  { "register",		RID_REGISTER,	0 },
  { "reinterpret_cast",	RID_REINTCAST,	0 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	0 },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "static_cast",	RID_STATCAST,	0 },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "template",		RID_TEMPLATE,	0 },
  { "this",		RID_THIS,	0 },
  { "throw",		RID_THROW,	0 },
  { "true",		RID_TRUE,	0 },
  { "try",		RID_TRY,	0 },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typename",		RID_TYPENAME,	0 },
  { "typeid",		RID_TYPEID,	0 },
  { "typeof",		RID_TYPEOF,	D_ASM|D_EXT },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "using",		RID_USING,	0 },
  { "virtual",		RID_VIRTUAL,	0 },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	0 },
  { "wchar_t",          RID_WCHAR,	0 },
  { "while",		RID_WHILE,	0 },
  { "xor",		RID_XOR,	D_OPNAME },
  { "xor_eq",		RID_XOR_EQ,	D_OPNAME },

};
#define N_reswords (sizeof reswords / sizeof (struct resword))

/* Table mapping from RID_* constants to yacc token numbers.
   Unfortunately we have to have entries for all the keywords in all
   three languages.  */
const short rid_to_yy[RID_MAX] =
{
  /* RID_STATIC */	SCSPEC,
  /* RID_UNSIGNED */	TYPESPEC,
  /* RID_LONG */	TYPESPEC,
  /* RID_CONST */	CV_QUALIFIER,
  /* RID_EXTERN */	SCSPEC,
  /* RID_REGISTER */	SCSPEC,
  /* RID_TYPEDEF */	SCSPEC,
  /* RID_SHORT */	TYPESPEC,
  /* RID_INLINE */	SCSPEC,
  /* RID_VOLATILE */	CV_QUALIFIER,
  /* RID_SIGNED */	TYPESPEC,
  /* RID_AUTO */	SCSPEC,
  /* RID_RESTRICT */	CV_QUALIFIER,

  /* C extensions.  Bounded pointers are not yet in C++ */
  /* RID_BOUNDED */	0,
  /* RID_UNBOUNDED */	0,
  /* RID_COMPLEX */	TYPESPEC,

  /* C++ */
  /* RID_FRIEND */	SCSPEC,
  /* RID_VIRTUAL */	SCSPEC,
  /* RID_EXPLICIT */	SCSPEC,
  /* RID_EXPORT */	EXPORT,
  /* RID_MUTABLE */	SCSPEC,

  /* ObjC */
  /* RID_IN */		0,
  /* RID_OUT */		0,
  /* RID_INOUT */	0,
  /* RID_BYCOPY */	0,
  /* RID_BYREF */	0,
  /* RID_ONEWAY */	0,

  /* C */
  /* RID_INT */		TYPESPEC,
  /* RID_CHAR */	TYPESPEC,
  /* RID_FLOAT */	TYPESPEC,
  /* RID_DOUBLE */	TYPESPEC,
  /* RID_VOID */	TYPESPEC,
  /* RID_ENUM */	ENUM,
  /* RID_STRUCT */	AGGR,
  /* RID_UNION */	AGGR,
  /* RID_IF */		IF,
  /* RID_ELSE */	ELSE,
  /* RID_WHILE */	WHILE,
  /* RID_DO */		DO,
  /* RID_FOR */		FOR,
  /* RID_SWITCH */	SWITCH,
  /* RID_CASE */	CASE,
  /* RID_DEFAULT */	DEFAULT,
  /* RID_BREAK */	BREAK,
  /* RID_CONTINUE */	CONTINUE,
  /* RID_RETURN */	RETURN_KEYWORD,
  /* RID_GOTO */	GOTO,
  /* RID_SIZEOF */	SIZEOF,

  /* C extensions */
  /* RID_ASM */		ASM_KEYWORD,
  /* RID_TYPEOF */	TYPEOF,
  /* RID_ALIGNOF */	ALIGNOF,
  /* RID_ATTRIBUTE */	ATTRIBUTE,
  /* RID_VA_ARG */	VA_ARG,
  /* RID_EXTENSION */	EXTENSION,
  /* RID_IMAGPART */	IMAGPART,
  /* RID_REALPART */	REALPART,
  /* RID_LABEL */	LABEL,
  /* RID_PTRBASE */	0,
  /* RID_PTREXTENT */	0,
  /* RID_PTRVALUE */	0,
  /* RID_CHOOSE_EXPR */	0,
  /* RID_TYPES_COMPATIBLE_P */ 0,

  /* RID_FUNCTION_NAME */	VAR_FUNC_NAME,
  /* RID_PRETTY_FUNCTION_NAME */ VAR_FUNC_NAME,
  /* RID_c99_FUNCTION_NAME */	VAR_FUNC_NAME,

  /* C++ */
  /* RID_BOOL */	TYPESPEC,
  /* RID_WCHAR */	TYPESPEC,
  /* RID_CLASS */	AGGR,
  /* RID_PUBLIC */	VISSPEC,
  /* RID_PRIVATE */	VISSPEC,
  /* RID_PROTECTED */	VISSPEC,
  /* RID_TEMPLATE */	TEMPLATE,
  /* RID_NULL */	CONSTANT,
  /* RID_CATCH */	CATCH,
  /* RID_DELETE */	DELETE,
  /* RID_FALSE */	CXX_FALSE,
  /* RID_NAMESPACE */	NAMESPACE,
  /* RID_NEW */		NEW,
  /* RID_OPERATOR */	OPERATOR,
  /* RID_THIS */	THIS,
  /* RID_THROW */	THROW,
  /* RID_TRUE */	CXX_TRUE,
  /* RID_TRY */		TRY,
  /* RID_TYPENAME */	TYPENAME_KEYWORD,
  /* RID_TYPEID */	TYPEID,
  /* RID_USING */	USING,

  /* casts */
  /* RID_CONSTCAST */	CONST_CAST,
  /* RID_DYNCAST */	DYNAMIC_CAST,
  /* RID_REINTCAST */	REINTERPRET_CAST,
  /* RID_STATCAST */	STATIC_CAST,

  /* alternate spellings */
  /* RID_AND */		ANDAND,
  /* RID_AND_EQ */	ASSIGN,
  /* RID_NOT */		'!',
  /* RID_NOT_EQ */	EQCOMPARE,
  /* RID_OR */		OROR,
  /* RID_OR_EQ */	ASSIGN,
  /* RID_XOR */		'^',
  /* RID_XOR_EQ */	ASSIGN,
  /* RID_BITAND */	'&',
  /* RID_BITOR */	'|',
  /* RID_COMPL */	'~',

  /* Objective C */
  /* RID_ID */			0,
  /* RID_AT_ENCODE */		0,
  /* RID_AT_END */		0,
  /* RID_AT_CLASS */		0,
  /* RID_AT_ALIAS */		0,
  /* RID_AT_DEFS */		0,
  /* RID_AT_PRIVATE */		0,
  /* RID_AT_PROTECTED */	0,
  /* RID_AT_PUBLIC */		0,
  /* RID_AT_PROTOCOL */		0,
  /* RID_AT_SELECTOR */		0,
  /* RID_AT_INTERFACE */	0,
  /* RID_AT_IMPLEMENTATION */	0
};

void
init_reswords ()
{
  unsigned int i;
  tree id;
  int mask = ((flag_operator_names ? 0 : D_OPNAME)
	      | (flag_no_asm ? D_ASM : 0)
	      | (flag_no_gnu_keywords ? D_EXT : 0));

  /* It is not necessary to register ridpointers as a GC root, because
     all the trees it points to are permanently interned in the
     get_identifier hash anyway.  */
  ridpointers = (tree *) xcalloc ((int) RID_MAX, sizeof (tree));
  for (i = 0; i < N_reswords; i++)
    {
      id = get_identifier (reswords[i].word);
      C_RID_CODE (id) = reswords[i].rid;
      ridpointers [(int) reswords[i].rid] = id;
      if (! (reswords[i].disable & mask))
	C_IS_RESERVED_WORD (id) = 1;
    }
}

static void
init_cp_pragma ()
{
  cpp_register_pragma (parse_in, 0, "vtable", handle_pragma_vtable);
  cpp_register_pragma (parse_in, 0, "unit", handle_pragma_unit);

  cpp_register_pragma (parse_in, 0, "interface", handle_pragma_interface);
  cpp_register_pragma (parse_in, 0, "implementation",
		       handle_pragma_implementation);

  cpp_register_pragma (parse_in, "GCC", "interface", handle_pragma_interface);
  cpp_register_pragma (parse_in, "GCC", "implementation",
		       handle_pragma_implementation);
  cpp_register_pragma (parse_in, "GCC", "java_exceptions",
		       handle_pragma_java_exceptions);
}

/* Initialize the C++ front end.  This function is very sensitive to
   the exact order that things are done here.  It would be nice if the
   initialization done by this routine were moved to its subroutines,
   and the ordering dependencies clarified and reduced.  */
const char *
cxx_init (filename)
     const char *filename;
{
  decl_printable_name = lang_printable_name;
  input_filename = "<internal>";

  init_reswords ();
  init_spew ();
  init_tree ();
  init_cplus_expand ();
  init_cp_semantics ();

  add_c_tree_codes ();

  memcpy (tree_code_type + (int) LAST_C_TREE_CODE,
	  cplus_tree_code_type,
	  (int)LAST_CPLUS_TREE_CODE - (int)LAST_C_TREE_CODE);
  memcpy (tree_code_length + (int) LAST_C_TREE_CODE,
	  cplus_tree_code_length,
	  (LAST_CPLUS_TREE_CODE - (int)LAST_C_TREE_CODE) * sizeof (int));
  memcpy (tree_code_name + (int) LAST_C_TREE_CODE,
	  cplus_tree_code_name,
	  (LAST_CPLUS_TREE_CODE - (int)LAST_C_TREE_CODE) * sizeof (char *));

  init_operators ();
  init_method ();
  init_error ();

  current_function_decl = NULL;

  class_type_node = build_int_2 (class_type, 0);
  TREE_TYPE (class_type_node) = class_type_node;
  ridpointers[(int) RID_CLASS] = class_type_node;

  record_type_node = build_int_2 (record_type, 0);
  TREE_TYPE (record_type_node) = record_type_node;
  ridpointers[(int) RID_STRUCT] = record_type_node;

  union_type_node = build_int_2 (union_type, 0);
  TREE_TYPE (union_type_node) = union_type_node;
  ridpointers[(int) RID_UNION] = union_type_node;

  enum_type_node = build_int_2 (enum_type, 0);
  TREE_TYPE (enum_type_node) = enum_type_node;
  ridpointers[(int) RID_ENUM] = enum_type_node;

  cxx_init_decl_processing ();

  /* Create the built-in __null node.  */
  null_node = build_int_2 (0, 0);
  TREE_TYPE (null_node) = type_for_size (POINTER_SIZE, 0);
  ridpointers[RID_NULL] = null_node;

  token_count = init_cpp_parse ();
  interface_unknown = 1;

  filename = c_common_init (filename);

  init_cp_pragma ();

  init_repo (filename);

  return filename;
}

inline void
yyprint (file, yychar, yylval)
     FILE *file;
     int yychar;
     YYSTYPE yylval;
{
  tree t;
  switch (yychar)
    {
    case IDENTIFIER:
    case tTYPENAME:
    case TYPESPEC:
    case PTYPENAME:
    case PFUNCNAME:
    case IDENTIFIER_DEFN:
    case TYPENAME_DEFN:
    case PTYPENAME_DEFN:
    case SCSPEC:
    case PRE_PARSED_CLASS_DECL:
      t = yylval.ttype;
      if (TREE_CODE (t) == TYPE_DECL || TREE_CODE (t) == TEMPLATE_DECL)
	{
	  fprintf (file, " `%s'", IDENTIFIER_POINTER (DECL_NAME (t)));
	  break;
	}
      my_friendly_assert (TREE_CODE (t) == IDENTIFIER_NODE, 224);
      if (IDENTIFIER_POINTER (t))
	  fprintf (file, " `%s'", IDENTIFIER_POINTER (t));
      break;

    case AGGR:
      if (yylval.ttype == class_type_node)
	fprintf (file, " `class'");
      else if (yylval.ttype == record_type_node)
	fprintf (file, " `struct'");
      else if (yylval.ttype == union_type_node)
	fprintf (file, " `union'");
      else if (yylval.ttype == enum_type_node)
	fprintf (file, " `enum'");
      else
	abort ();
      break;

    case CONSTANT:
      t = yylval.ttype;
      if (TREE_CODE (t) == INTEGER_CST)
	fprintf (file,
#if HOST_BITS_PER_WIDE_INT == 64
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 " 0x%x%016x",
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
		 " 0x%lx%016lx",
#else
		 " 0x%llx%016llx",
#endif
#endif
#else
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		 " 0x%lx%08lx",
#else
		 " 0x%x%08x",
#endif
#endif
		 TREE_INT_CST_HIGH (t), TREE_INT_CST_LOW (t));
      break;
    }
}

#if defined(GATHER_STATISTICS) && defined(REDUCE_LENGTH)
static int *reduce_count;
#endif

int *token_count;

#if 0
#define REDUCE_LENGTH (sizeof (yyr2) / sizeof (yyr2[0]))
#define TOKEN_LENGTH (256 + sizeof (yytname) / sizeof (yytname[0]))
#endif

#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
void
yyhook (yyn)
     int yyn;
{
  reduce_count[yyn] += 1;
}

static int
reduce_cmp (p, q)
     int *p, *q;
{
  return reduce_count[*q] - reduce_count[*p];
}

static int
token_cmp (p, q)
     int *p, *q;
{
  return token_count[*q] - token_count[*p];
}
#endif
#endif

void
print_parse_statistics ()
{
#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
#if YYDEBUG != 0
  int i;
  int maxlen = REDUCE_LENGTH;
  unsigned *sorted;

  if (reduce_count[-1] == 0)
    return;

  if (TOKEN_LENGTH > REDUCE_LENGTH)
    maxlen = TOKEN_LENGTH;
  sorted = (unsigned *) alloca (sizeof (int) * maxlen);

  for (i = 0; i < TOKEN_LENGTH; i++)
    sorted[i] = i;
  qsort (sorted, TOKEN_LENGTH, sizeof (int), token_cmp);
  for (i = 0; i < TOKEN_LENGTH; i++)
    {
      int idx = sorted[i];
      if (token_count[idx] == 0)
	break;
      if (token_count[idx] < token_count[-1])
	break;
      fprintf (stderr, "token %d, `%s', count = %d\n",
	       idx, yytname[YYTRANSLATE (idx)], token_count[idx]);
    }
  fprintf (stderr, "\n");
  for (i = 0; i < REDUCE_LENGTH; i++)
    sorted[i] = i;
  qsort (sorted, REDUCE_LENGTH, sizeof (int), reduce_cmp);
  for (i = 0; i < REDUCE_LENGTH; i++)
    {
      int idx = sorted[i];
      if (reduce_count[idx] == 0)
	break;
      if (reduce_count[idx] < reduce_count[-1])
	break;
      fprintf (stderr, "rule %d, line %d, count = %d\n",
	       idx, yyrline[idx], reduce_count[idx]);
    }
  fprintf (stderr, "\n");
#endif
#endif
#endif
}

/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */

void
cxx_set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  extern int yydebug;
  yydebug = value;
#else
  warning ("YYDEBUG not defined");
#endif
}

/* Helper function to load global variables with interface
   information.  */

void
extract_interface_info ()
{
  struct c_fileinfo *finfo = 0;

  if (flag_alt_external_templates)
    {
      tree til = tinst_for_decl ();

      if (til)
	finfo = get_fileinfo (TINST_FILE (til));
    }
  if (!finfo)
    finfo = get_fileinfo (input_filename);

  interface_only = finfo->interface_only;
  interface_unknown = finfo->interface_unknown;
}

/* Return nonzero if S is not considered part of an
   INTERFACE/IMPLEMENTATION pair.  Otherwise, return 0.  */

static int
interface_strcmp (s)
     const char *s;
{
  /* Set the interface/implementation bits for this scope.  */
  struct impl_files *ifiles;
  const char *s1;

  for (ifiles = impl_file_chain; ifiles; ifiles = ifiles->next)
    {
      const char *t1 = ifiles->filename;
      s1 = s;

      if (*s1 != *t1 || *s1 == 0)
	continue;

      while (*s1 == *t1 && *s1 != 0)
	s1++, t1++;

      /* A match.  */
      if (*s1 == *t1)
	return 0;

      /* Don't get faked out by xxx.yyy.cc vs xxx.zzz.cc.  */
      if (strchr (s1, '.') || strchr (t1, '.'))
	continue;

      if (*s1 == '\0' || s1[-1] != '.' || t1[-1] != '.')
	continue;

      /* A match.  */
      return 0;
    }

  /* No matches.  */
  return 1;
}

/* Heuristic to tell whether the user is missing a semicolon
   after a struct or enum declaration.  Emit an error message
   if we know the user has blown it.  */

void
check_for_missing_semicolon (type)
     tree type;
{
  if (yychar < 0)
    yychar = yylex ();

  if ((yychar > 255
       && yychar != SCSPEC
       && yychar != IDENTIFIER
       && yychar != tTYPENAME
       && yychar != CV_QUALIFIER
       && yychar != SELFNAME)
      || yychar == 0  /* EOF */)
    {
      if (TYPE_ANONYMOUS_P (type))
	error ("semicolon missing after %s declaration",
	       TREE_CODE (type) == ENUMERAL_TYPE ? "enum" : "struct");
      else
	error ("semicolon missing after declaration of `%T'", type);
      shadow_tag (build_tree_list (0, type));
    }
  /* Could probably also hack cases where class { ... } f (); appears.  */
  clear_anon_tags ();
}

void
note_got_semicolon (type)
     tree type;
{
  if (!TYPE_P (type))
    abort ();
  if (CLASS_TYPE_P (type))
    CLASSTYPE_GOT_SEMICOLON (type) = 1;
}

void
note_list_got_semicolon (declspecs)
     tree declspecs;
{
  tree link;

  for (link = declspecs; link; link = TREE_CHAIN (link))
    {
      tree type = TREE_VALUE (link);
      if (type && TYPE_P (type))
	note_got_semicolon (type);
    }
  clear_anon_tags ();
}


/* Parse a #pragma whose sole argument is a string constant.
   If OPT is true, the argument is optional.  */
static tree
parse_strconst_pragma (name, opt)
     const char *name;
     int opt;
{
  tree result, x;
  enum cpp_ttype t;

  t = c_lex (&x);
  if (t == CPP_STRING)
    {
      result = x;
      if (c_lex (&x) != CPP_EOF)
	warning ("junk at end of #pragma %s", name);
      return result;
    }

  if (t == CPP_EOF && opt)
    return 0;

  error ("invalid #pragma %s", name);
  return (tree)-1;
}

static void
handle_pragma_vtable (dfile)
     cpp_reader *dfile ATTRIBUTE_UNUSED;
{
  parse_strconst_pragma ("vtable", 0);
  sorry ("#pragma vtable no longer supported");
}

static void
handle_pragma_unit (dfile)
     cpp_reader *dfile ATTRIBUTE_UNUSED;
{
  /* Validate syntax, but don't do anything.  */
  parse_strconst_pragma ("unit", 0);
}

static void
handle_pragma_interface (dfile)
     cpp_reader *dfile ATTRIBUTE_UNUSED;
{
  tree fname = parse_strconst_pragma ("interface", 1);
  struct c_fileinfo *finfo;
  const char *main_filename;

  if (fname == (tree)-1)
    return;
  else if (fname == 0)
    main_filename = lbasename (input_filename);
  else
    main_filename = TREE_STRING_POINTER (fname);

  finfo = get_fileinfo (input_filename);

  if (impl_file_chain == 0)
    {
      /* If this is zero at this point, then we are
	 auto-implementing.  */
      if (main_input_filename == 0)
	main_input_filename = input_filename;
    }

  interface_only = interface_strcmp (main_filename);
#ifdef MULTIPLE_SYMBOL_SPACES
  if (! interface_only)
#endif
    interface_unknown = 0;

  finfo->interface_only = interface_only;
  finfo->interface_unknown = interface_unknown;
}

/* Note that we have seen a #pragma implementation for the key MAIN_FILENAME.
   We used to only allow this at toplevel, but that restriction was buggy
   in older compilers and it seems reasonable to allow it in the headers
   themselves, too.  It only needs to precede the matching #p interface.

   We don't touch interface_only or interface_unknown; the user must specify
   a matching #p interface for this to have any effect.  */

static void
handle_pragma_implementation (dfile)
     cpp_reader *dfile ATTRIBUTE_UNUSED;
{
  tree fname = parse_strconst_pragma ("implementation", 1);
  const char *main_filename;
  struct impl_files *ifiles = impl_file_chain;

  if (fname == (tree)-1)
    return;

  if (fname == 0)
    {
      if (main_input_filename)
	main_filename = main_input_filename;
      else
	main_filename = input_filename;
      main_filename = lbasename (main_filename);
    }
  else
    {
      main_filename = TREE_STRING_POINTER (fname);
      if (cpp_included (parse_in, main_filename))
	warning ("#pragma implementation for %s appears after file is included",
		 main_filename);
    }

  for (; ifiles; ifiles = ifiles->next)
    {
      if (! strcmp (ifiles->filename, main_filename))
	break;
    }
  if (ifiles == 0)
    {
      ifiles = (struct impl_files*) xmalloc (sizeof (struct impl_files));
      ifiles->filename = main_filename;
      ifiles->next = impl_file_chain;
      impl_file_chain = ifiles;
    }
}

/* Indicate that this file uses Java-personality exception handling.  */
static void
handle_pragma_java_exceptions (dfile)
     cpp_reader *dfile ATTRIBUTE_UNUSED;
{
  tree x;
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma GCC java_exceptions");

  choose_personality_routine (lang_java);
}

void
do_pending_lang_change ()
{
  for (; pending_lang_change > 0; --pending_lang_change)
    push_lang_context (lang_name_c);
  for (; pending_lang_change < 0; ++pending_lang_change)
    pop_lang_context ();
}

/* Return true if d is in a global scope. */

static int
is_global (d)
  tree d;
{
  while (1)
    switch (TREE_CODE (d))
      {
      case ERROR_MARK:
	return 1;

      case OVERLOAD: d = OVL_FUNCTION (d); continue;
      case TREE_LIST: d = TREE_VALUE (d); continue;
      default:
        my_friendly_assert (DECL_P (d), 980629);

	return DECL_NAMESPACE_SCOPE_P (d);
      }
}

tree
do_identifier (token, parsing, args)
     register tree token;
     int parsing;
     tree args;
{
  register tree id;
  int lexing = (parsing == 1 || parsing == 3);

  if (! lexing)
    id = lookup_name (token, 0);
  else
    id = lastiddecl;

  if (lexing && id && TREE_DEPRECATED (id))
    warn_deprecated_use (id);

  /* Do Koenig lookup if appropriate (inside templates we build lookup
     expressions instead).

     [basic.lookup.koenig]: If the ordinary unqualified lookup of the name
     finds the declaration of a class member function, the associated
     namespaces and classes are not considered.  */

  if (args && !current_template_parms && (!id || is_global (id)))
    id = lookup_arg_dependent (token, id, args);

  /* Remember that this name has been used in the class definition, as per
     [class.scope0] */
  if (id && parsing && parsing != 3)
    maybe_note_name_used_in_class (token, id);

  if (id == error_mark_node)
    {
      /* lookup_name quietly returns error_mark_node if we're parsing,
	 as we don't want to complain about an identifier that ends up
	 being used as a declarator.  So we call it again to get the error
	 message.  */
      id = lookup_name (token, 0);
      return error_mark_node;
    }

  if (!id || (TREE_CODE (id) == FUNCTION_DECL
	      && DECL_ANTICIPATED (id)))
    {
      if (current_template_parms)
	return build_min_nt (LOOKUP_EXPR, token);
      else if (IDENTIFIER_OPNAME_P (token))
	{
	  if (token != ansi_opname (ERROR_MARK))
	    error ("`%D' not defined", token);
	  id = error_mark_node;
	}
      else if (current_function_decl == 0)
	{
	  error ("`%D' was not declared in this scope", token);
	  id = error_mark_node;
	}
      else
	{
	  if (IDENTIFIER_NAMESPACE_VALUE (token) != error_mark_node
	      || IDENTIFIER_ERROR_LOCUS (token) != current_function_decl)
	    {
	      static int undeclared_variable_notice;

	      error ("`%D' undeclared (first use this function)", token);

	      if (! undeclared_variable_notice)
		{
		  error ("(Each undeclared identifier is reported only once for each function it appears in.)");
		  undeclared_variable_notice = 1;
		}
	    }
	  id = error_mark_node;
	  /* Prevent repeated error messages.  */
	  SET_IDENTIFIER_NAMESPACE_VALUE (token, error_mark_node);
	  SET_IDENTIFIER_ERROR_LOCUS (token, current_function_decl);
	}
    }

  if (TREE_CODE (id) == VAR_DECL && DECL_DEAD_FOR_LOCAL (id))
    {
      tree shadowed = DECL_SHADOWED_FOR_VAR (id);
      while (shadowed != NULL_TREE && TREE_CODE (shadowed) == VAR_DECL
	     && DECL_DEAD_FOR_LOCAL (shadowed))
	shadowed = DECL_SHADOWED_FOR_VAR (shadowed);
      if (!shadowed)
	shadowed = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (id));
      if (shadowed)
	{
	  if (!DECL_ERROR_REPORTED (id))
	    {
	      warning ("name lookup of `%s' changed",
		       IDENTIFIER_POINTER (token));
	      cp_warning_at ("  matches this `%D' under ISO standard rules",
			     shadowed);
	      cp_warning_at ("  matches this `%D' under old rules", id);
	      DECL_ERROR_REPORTED (id) = 1;
	    }
	  id = shadowed;
	}
      else if (!DECL_ERROR_REPORTED (id))
	{
	  DECL_ERROR_REPORTED (id) = 1;
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (id)))
	    {
	      error ("name lookup of `%s' changed for new ISO `for' scoping",
		     IDENTIFIER_POINTER (token));
	      cp_error_at ("  cannot use obsolete binding at `%D' because it has a destructor", id);
	      id = error_mark_node;
	    }
	  else
	    {
	      pedwarn ("name lookup of `%s' changed for new ISO `for' scoping",
		       IDENTIFIER_POINTER (token));
	      cp_pedwarn_at ("  using obsolete binding at `%D'", id);
	    }
	}
    }
  /* TREE_USED is set in `hack_identifier'.  */
  if (TREE_CODE (id) == CONST_DECL)
    {
      /* Check access.  */
      if (IDENTIFIER_CLASS_VALUE (token) == id)
	enforce_access (CP_DECL_CONTEXT(id), id);
      if (!processing_template_decl || DECL_TEMPLATE_PARM_P (id))
	id = DECL_INITIAL (id);
    }
  else
    id = hack_identifier (id, token);

  /* We must look up dependent names when the template is
     instantiated, not while parsing it.  For now, we don't
     distinguish between dependent and independent names.  So, for
     example, we look up all overloaded functions at
     instantiation-time, even though in some cases we should just use
     the DECL we have here.  We also use LOOKUP_EXPRs to find things
     like local variables, rather than creating TEMPLATE_DECLs for the
     local variables and then finding matching instantiations.  */
  if (current_template_parms
      && (is_overloaded_fn (id)
	  || (TREE_CODE (id) == VAR_DECL
	      && CP_DECL_CONTEXT (id)
	      && TREE_CODE (CP_DECL_CONTEXT (id)) == FUNCTION_DECL)
	  || TREE_CODE (id) == PARM_DECL
	  || TREE_CODE (id) == RESULT_DECL
	  || TREE_CODE (id) == USING_DECL))
    id = build_min_nt (LOOKUP_EXPR, token);

  return id;
}

tree
do_scoped_id (token, parsing)
     tree token;
     int parsing;
{
  tree id;
  /* during parsing, this is ::name. Otherwise, it is black magic. */
  if (parsing)
    {
      id = make_node (CPLUS_BINDING);
      if (!qualified_lookup_using_namespace (token, global_namespace, id, 0))
	id = NULL_TREE;
      else
	id = BINDING_VALUE (id);
    }
  else
    id = IDENTIFIER_GLOBAL_VALUE (token);
  if (parsing && yychar == YYEMPTY)
    yychar = yylex ();
  if (!id || (TREE_CODE (id) == FUNCTION_DECL
	      && DECL_ANTICIPATED (id)))
    {
      if (processing_template_decl)
	{
	  id = build_min_nt (LOOKUP_EXPR, token);
	  LOOKUP_EXPR_GLOBAL (id) = 1;
	  return id;
	}
      if (IDENTIFIER_NAMESPACE_VALUE (token) != error_mark_node)
        error ("`::%D' undeclared (first use here)", token);
      id = error_mark_node;
      /* Prevent repeated error messages.  */
      SET_IDENTIFIER_NAMESPACE_VALUE (token, error_mark_node);
    }
  else
    {
      if (TREE_CODE (id) == ADDR_EXPR)
	mark_used (TREE_OPERAND (id, 0));
      else if (TREE_CODE (id) != OVERLOAD)
	mark_used (id);
    }
  if (TREE_CODE (id) == CONST_DECL && ! processing_template_decl)
    {
      /* XXX CHS - should we set TREE_USED of the constant? */
      id = DECL_INITIAL (id);
      /* This is to prevent an enum whose value is 0
	 from being considered a null pointer constant.  */
      id = build1 (NOP_EXPR, TREE_TYPE (id), id);
      TREE_CONSTANT (id) = 1;
    }

  if (processing_template_decl)
    {
      if (is_overloaded_fn (id))
	{
	  id = build_min_nt (LOOKUP_EXPR, token);
	  LOOKUP_EXPR_GLOBAL (id) = 1;
	  return id;
	}
      /* else just use the decl */
    }
  return convert_from_reference (id);
}

tree
identifier_typedecl_value (node)
     tree node;
{
  tree t, type;
  type = IDENTIFIER_TYPE_VALUE (node);
  if (type == NULL_TREE)
    return NULL_TREE;

  if (IDENTIFIER_BINDING (node))
    {
      t = IDENTIFIER_VALUE (node);
      if (t && TREE_CODE (t) == TYPE_DECL && TREE_TYPE (t) == type)
	return t;
    }
  if (IDENTIFIER_NAMESPACE_VALUE (node))
    {
      t = IDENTIFIER_NAMESPACE_VALUE (node);
      if (t && TREE_CODE (t) == TYPE_DECL && TREE_TYPE (t) == type)
	return t;
    }

  /* Will this one ever happen?  */
  if (TYPE_MAIN_DECL (type))
    return TYPE_MAIN_DECL (type);

  /* We used to do an internal error of 62 here, but instead we will
     handle the return of a null appropriately in the callers.  */
  return NULL_TREE;
}

#ifdef GATHER_STATISTICS
/* The original for tree_node_kind is in the toplevel tree.c; changes there
   need to be brought into here, unless this were actually put into a header
   instead.  */
/* Statistics-gathering stuff.  */
typedef enum
{
  d_kind,
  t_kind,
  b_kind,
  s_kind,
  r_kind,
  e_kind,
  c_kind,
  id_kind,
  op_id_kind,
  perm_list_kind,
  temp_list_kind,
  vec_kind,
  x_kind,
  lang_decl,
  lang_type,
  all_kinds
} tree_node_kind;

extern int tree_node_counts[];
extern int tree_node_sizes[];
#endif

tree
build_lang_decl (code, name, type)
     enum tree_code code;
     tree name;
     tree type;
{
  tree t;

  t = build_decl (code, name, type);
  retrofit_lang_decl (t);

  return t;
}

/* Add DECL_LANG_SPECIFIC info to T.  Called from build_lang_decl
   and pushdecl (for functions generated by the backend).  */

void
retrofit_lang_decl (t)
     tree t;
{
  struct lang_decl *ld;
  size_t size;

  if (CAN_HAVE_FULL_LANG_DECL_P (t))
    size = sizeof (struct lang_decl);
  else
    size = sizeof (struct lang_decl_flags);

  ld = (struct lang_decl *) ggc_alloc_cleared (size);

  DECL_LANG_SPECIFIC (t) = ld;
  if (current_lang_name == lang_name_cplusplus)
    SET_DECL_LANGUAGE (t, lang_cplusplus);
  else if (current_lang_name == lang_name_c)
    SET_DECL_LANGUAGE (t, lang_c);
  else if (current_lang_name == lang_name_java)
    SET_DECL_LANGUAGE (t, lang_java);
  else abort ();

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += size;
#endif
}

void
copy_lang_decl (node)
     tree node;
{
  int size;
  struct lang_decl *ld;

  if (! DECL_LANG_SPECIFIC (node))
    return;

  if (!CAN_HAVE_FULL_LANG_DECL_P (node))
    size = sizeof (struct lang_decl_flags);
  else
    size = sizeof (struct lang_decl);
  ld = (struct lang_decl *) ggc_alloc (size);
  memcpy (ld, DECL_LANG_SPECIFIC (node), size);
  DECL_LANG_SPECIFIC (node) = ld;

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += size;
#endif
}

/* Copy DECL, including any language-specific parts.  */

tree
copy_decl (decl)
     tree decl;
{
  tree copy;

  copy = copy_node (decl);
  copy_lang_decl (copy);
  return copy;
}

/* Replace the shared language-specific parts of NODE with a new copy.  */

static void
copy_lang_type (node)
     tree node;
{
  int size;
  struct lang_type *lt;

  if (! TYPE_LANG_SPECIFIC (node))
    return;

  size = sizeof (struct lang_type);
  lt = (struct lang_type *) ggc_alloc (size);
  memcpy (lt, TYPE_LANG_SPECIFIC (node), size);
  TYPE_LANG_SPECIFIC (node) = lt;

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_type] += 1;
  tree_node_sizes[(int)lang_type] += size;
#endif
}

/* Copy TYPE, including any language-specific parts.  */

tree
copy_type (type)
     tree type;
{
  tree copy;

  copy = copy_node (type);
  copy_lang_type (copy);
  return copy;
}

tree
cp_make_lang_type (code)
     enum tree_code code;
{
  register tree t = make_node (code);

  /* Create lang_type structure.  */
  if (IS_AGGR_TYPE_CODE (code)
      || code == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      struct lang_type *pi;

      pi = ((struct lang_type *)
	    ggc_alloc_cleared (sizeof (struct lang_type)));

      TYPE_LANG_SPECIFIC (t) = pi;

#ifdef GATHER_STATISTICS
      tree_node_counts[(int)lang_type] += 1;
      tree_node_sizes[(int)lang_type] += sizeof (struct lang_type);
#endif
    }

  /* Set up some flags that give proper default behavior.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;

      /* Make sure this is laid out, for ease of use later.  In the
	 presence of parse errors, the normal was of assuring this
	 might not ever get executed, so we lay it out *immediately*.  */
      build_pointer_type (t);
    }
  else
    /* We use TYPE_ALIAS_SET for the CLASSTYPE_MARKED bits.  But,
       TYPE_ALIAS_SET is initialized to -1 by default, so we must
       clear it here.  */
    TYPE_ALIAS_SET (t) = 0;

  /* We need to allocate a TYPE_BINFO even for TEMPLATE_TYPE_PARMs
     since they can be virtual base types, and we then need a
     canonical binfo for them.  Ideally, this would be done lazily for
     all types.  */
  if (IS_AGGR_TYPE_CODE (code) || code == TEMPLATE_TYPE_PARM
      || code == BOUND_TEMPLATE_TEMPLATE_PARM
      || code == TYPENAME_TYPE)
    TYPE_BINFO (t) = make_binfo (size_zero_node, t, NULL_TREE, NULL_TREE);

  return t;
}

tree
make_aggr_type (code)
     enum tree_code code;
{
  tree t = cp_make_lang_type (code);

  if (IS_AGGR_TYPE_CODE (code))
    SET_IS_AGGR_TYPE (t, 1);

  return t;
}

void
compiler_error VPARAMS ((const char *msg, ...))
{
  char buf[1024];

  VA_OPEN (ap, msg);
  VA_FIXEDARG (ap, const char *, msg);

  vsprintf (buf, msg, ap);
  VA_CLOSE (ap);

  error_with_file_and_line (input_filename, lineno, "%s (compiler error)", buf);
}

/* Return the type-qualifier corresponding to the identifier given by
   RID.  */

int
cp_type_qual_from_rid (rid)
     tree rid;
{
  if (rid == ridpointers[(int) RID_CONST])
    return TYPE_QUAL_CONST;
  else if (rid == ridpointers[(int) RID_VOLATILE])
    return TYPE_QUAL_VOLATILE;
  else if (rid == ridpointers[(int) RID_RESTRICT])
    return TYPE_QUAL_RESTRICT;

  abort ();
  return TYPE_UNQUALIFIED;
}
