/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
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
#include "lex.h"
#include "cp-tree.h"
#include "parse.h"
#include "flags.h"
#include "obstack.h"
#include "c-pragma.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"
#include "tm_p.h"
#include "timevar.h"

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void yyprint PARAMS ((FILE *, int, YYSTYPE));

static tree get_time_identifier PARAMS ((const char *));
static int check_newline PARAMS ((void));
static int whitespace_cr		PARAMS ((int));
static int skip_white_space PARAMS ((int));
static void finish_defarg PARAMS ((void));
static int interface_strcmp PARAMS ((const char *));
static int readescape PARAMS ((int *));
static char *extend_token_buffer PARAMS ((const char *));
static void consume_string PARAMS ((struct obstack *, int));
static void feed_defarg PARAMS ((tree, tree));
static void store_pending_inline PARAMS ((tree, struct pending_inline *));
static void reinit_parse_for_expr PARAMS ((struct obstack *));
static int *init_cpp_parse PARAMS ((void));
static void cp_pragma_interface PARAMS ((char *));
static void cp_pragma_implementation PARAMS ((char *));
static int handle_cp_pragma PARAMS ((const char *));
#ifdef HANDLE_GENERIC_PRAGMAS
static int handle_generic_pragma PARAMS ((int));
#endif
#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
static int reduce_cmp PARAMS ((int *, int *));
static int token_cmp PARAMS ((int *, int *));
#endif
#endif
static void begin_definition_of_inclass_inline PARAMS ((struct pending_inline*));
static void parse_float PARAMS ((PTR));
static int is_global PARAMS ((tree));
static void init_filename_times PARAMS ((void));
static void extend_token_buffer_to PARAMS ((int));
#ifdef HANDLE_PRAGMA
static int pragma_getc PARAMS ((void));
static void pragma_ungetc PARAMS ((int));
#endif
static int read_line_number PARAMS ((int *));
static int token_getch PARAMS ((void));
static void token_put_back PARAMS ((int));
static void mark_impl_file_chain PARAMS ((void *));
static int read_ucs PARAMS ((int));
static int is_extended_char PARAMS ((int));
static int is_extended_char_1 PARAMS ((int));

/* Given a file name X, return the nondirectory portion.
   Keep in mind that X can be computed more than once.  */
char *
file_name_nondirectory (x)
     const char *x;
{
  char *tmp = (char *) rindex (x, '/');
  if (DIR_SEPARATOR != '/' && ! tmp)
    tmp = (char *) rindex (x, DIR_SEPARATOR);
  if (tmp)
    return (char *) (tmp + 1);
  else
    return (char *) x;
}

/* This obstack is needed to hold text.  It is not safe to use
   TOKEN_BUFFER because `check_newline' calls `yylex'.  */
struct obstack inline_text_obstack;
char *inline_text_firstobj;

/* Nonzero if parse output is being saved to an obstack for later parsing. */
static int saving_parse_to_obstack = 0;

#if USE_CPPLIB
#include "cpplib.h"
extern cpp_reader  parse_in;
extern cpp_options parse_options;
extern unsigned char *yy_cur, *yy_lim;
extern enum cpp_token cpp_token;
#else
FILE *finput;
#endif
int end_of_file;

int linemode;

/* Pending language change.
   Positive is push count, negative is pop count.  */
int pending_lang_change = 0;

/* Wrap the current header file in extern "C".  */
static int c_header_level = 0;

extern int first_token;
extern struct obstack token_obstack;

/* ??? Don't really know where this goes yet.  */
#include "input.c"

/* Holds translations from TREE_CODEs to operator name strings,
   i.e., opname_tab[PLUS_EXPR] == "+".  */
const char **opname_tab;
const char **assignop_tab;

extern int yychar;		/*  the lookahead symbol		*/
extern YYSTYPE yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#if 0
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif


/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
tree lastiddecl;

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree ridpointers[(int) RID_MAX];

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static tree filename_times;
static tree this_filename_time;

/* Array for holding counts of the numbers of tokens seen.  */
extern int *token_count;

/* When we see a default argument in a method declaration, we snarf it as
   text using snarf_defarg.  When we get up to namespace scope, we then go
   through and parse all of them using do_pending_defargs.  Since yacc
   parsers are not reentrant, we retain defargs state in these two
   variables so that subsequent calls to do_pending_defargs can resume
   where the previous call left off.  */

static tree defarg_fns;
static tree defarg_parm;

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
  char *filename;
  struct impl_files *next;
};

static struct impl_files *impl_file_chain;

/* The string used to represent the filename of internally generated
   tree nodes.  The variable, which is dynamically allocated, should
   be used; the macro is only used to initialize it.  */
static char *internal_filename;
#define INTERNAL_FILENAME ("<internal>")

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
  target = build_parse_node (INDIRECT_REF, target);
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
  target = build_parse_node (ADDR_EXPR, target);
  TREE_TYPE (target) = cv_qualifiers;
  return target;
}

tree
make_call_declarator (target, parms, cv_qualifiers, exception_specification)
     tree target, parms, cv_qualifiers, exception_specification;
{
  target = build_parse_node (CALL_EXPR, target, 
			     /* Both build_parse_node and
				decl_tree_cons build on the
				temp_decl_obstack.  */
			     decl_tree_cons (parms, cv_qualifiers, NULL_TREE),
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

/* Build names and nodes for overloaded operators.  */

tree ansi_opname[LAST_CPLUS_TREE_CODE];
tree ansi_assopname[LAST_CPLUS_TREE_CODE];

const char *
operator_name_string (name)
     tree name;
{
  char *opname = IDENTIFIER_POINTER (name) + 2;
  tree *opname_table;
  int i, assign;

  /* Works for builtin and user defined types.  */
  if (IDENTIFIER_GLOBAL_VALUE (name)
      && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (name)) == TYPE_DECL)
    return IDENTIFIER_POINTER (name);

  if (opname[0] == 'a' && opname[2] != '\0' && opname[2] != '_')
    {
      opname += 1;
      assign = 1;
      opname_table = ansi_assopname;
    }
  else
    {
      assign = 0;
      opname_table = ansi_opname;
    }

  for (i = 0; i < (int) LAST_CPLUS_TREE_CODE; i++)
    {
      if (opname[0] == IDENTIFIER_POINTER (opname_table[i])[2+assign]
	  && opname[1] == IDENTIFIER_POINTER (opname_table[i])[3+assign])
	break;
    }

  if (i == LAST_CPLUS_TREE_CODE)
    return "<invalid operator>";

  if (assign)
    return assignop_tab[i];
  else
    return opname_tab[i];
}

int interface_only;		/* whether or not current file is only for
				   interface definitions.  */
int interface_unknown;		/* whether or not we know this class
				   to behave according to #pragma interface.  */

/* lexical analyzer */

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

static int maxtoken;		/* Current nominal length of token buffer.  */
char *token_buffer;		/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2.  */

static int indent_level;	/* Number of { minus number of }. */

#include "hash.h"


/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag;

static tree
get_time_identifier (name)
     const char *name;
{
  tree time_identifier;
  int len = strlen (name);
  char *buf = (char *) alloca (len + 6);
  strcpy (buf, "file ");
  bcopy (name, buf+5, len);
  buf[len+5] = '\0';
  time_identifier = get_identifier (buf);
  if (TIME_IDENTIFIER_TIME (time_identifier) == NULL_TREE)
    {
      TIME_IDENTIFIER_TIME (time_identifier) = build_int_2 (0, 0);
      TIME_IDENTIFIER_FILEINFO (time_identifier) 
	= build_int_2 (0, 1);
      SET_IDENTIFIER_GLOBAL_VALUE (time_identifier, filename_times);
      filename_times = time_identifier;
    }
  return time_identifier;
}

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp/cp-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

static char cplus_tree_code_type[] = {
  'x',
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

static int cplus_tree_code_length[] = {
  0,
#include "cp-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

static const char *cplus_tree_code_name[] = {
  "@@dummy",
#include "cp-tree.def"
};
#undef DEFTREECODE

/* toplev.c needs to call these.  */

void
lang_init_options ()
{
#if USE_CPPLIB
  cpp_reader_init (&parse_in);
  parse_in.opts = &parse_options;
  cpp_options_init (&parse_options);
  parse_options.cplusplus = 1;
#endif

  /* Default exceptions on.  */
  flag_exceptions = 1;
  /* Mark as "unspecified".  */
  flag_bounds_check = -1;
  /* By default wrap lines at 72 characters.  */
  set_message_length (72);
}

void
lang_init ()
{
  /* If still "unspecified", make it match -fbounded-pointers.  */
  if (flag_bounds_check < 0)
    flag_bounds_check = flag_bounded_pointers;

  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  put_back (check_newline ());
  if (flag_gnu_xref) GNU_xref_begin (input_filename);
  init_repo (input_filename);
}

void
lang_finish ()
{
  if (flag_gnu_xref) GNU_xref_end (errorcount+sorrycount);
}

const char *
lang_identify ()
{
  return "cplusplus";
}

static void
init_filename_times ()
{
  this_filename_time = get_time_identifier ("<top level>");
  if (flag_detailed_statistics)
    {
      header_time = 0;
      body_time = get_run_time ();
      TREE_INT_CST_LOW (TIME_IDENTIFIER_TIME (this_filename_time)) 
	= body_time;
    }
}

/* Change by Bryan Boreham, Kewill, Thu Jul 27 09:46:05 1989.
   Stuck this hack in to get the files open correctly; this is called
   in place of init_parse if we are an unexec'd binary.    */

#if 0
void
reinit_lang_specific ()
{
  init_filename_times ();
  reinit_search_statistics ();
}
#endif

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

char *
init_parse (filename)
     char *filename;
{
  extern int flag_no_gnu_keywords;
  extern int flag_operator_names;

  int i;

#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
  literal_codeset = getenv ("LANG");
#endif

#if !USE_CPPLIB
  /* Open input file.  */
  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    pfatal_with_name (filename);

#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif
#else /* !USE_CPPLIB */
  parse_in.show_column = 1;
  if (! cpp_start_read (&parse_in, filename))
    abort ();

  if (filename == 0 || !strcmp (filename, "-"))
    filename = "stdin";

  /* cpp_start_read always puts at least one line directive into the
     token buffer.  We must arrange to read it out here. */
  yy_cur = parse_in.token_buffer;
  yy_lim = CPP_PWRITTEN (&parse_in);
  cpp_token = CPP_DIRECTIVE;

#endif /* !USE_CPPLIB */

  /* Initialize the lookahead machinery.  */
  init_spew ();

  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));
  decl_printable_name = lang_printable_name;

  init_tree ();
  init_cplus_expand ();

  memcpy (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE,
	  cplus_tree_code_type,
	  (int)LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE);
  memcpy (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE,
	  cplus_tree_code_length,
	  (LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (int));
  memcpy (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE,
	  cplus_tree_code_name,
	  (LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));

  opname_tab = (const char **)oballoc ((int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  memset (opname_tab, 0, (int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  assignop_tab = (const char **)oballoc ((int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  memset (assignop_tab, 0, (int)LAST_CPLUS_TREE_CODE * sizeof (char *));

  ansi_opname[0] = get_identifier ("<invalid operator>");
  for (i = 0; i < (int) LAST_CPLUS_TREE_CODE; i++)
    {
      ansi_opname[i] = ansi_opname[0];
      ansi_assopname[i] = ansi_opname[0];
    }

  ansi_opname[(int) MULT_EXPR] = get_identifier ("__ml");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MULT_EXPR]) = 1;
  ansi_opname[(int) INDIRECT_REF] = ansi_opname[(int) MULT_EXPR];
  ansi_assopname[(int) MULT_EXPR] = get_identifier ("__aml");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) MULT_EXPR]) = 1;
  ansi_assopname[(int) INDIRECT_REF] = ansi_assopname[(int) MULT_EXPR];
  ansi_opname[(int) TRUNC_MOD_EXPR] = get_identifier ("__md");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TRUNC_MOD_EXPR]) = 1;
  ansi_assopname[(int) TRUNC_MOD_EXPR] = get_identifier ("__amd");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) TRUNC_MOD_EXPR]) = 1;
  ansi_opname[(int) CEIL_MOD_EXPR] = ansi_opname[(int) TRUNC_MOD_EXPR];
  ansi_opname[(int) FLOOR_MOD_EXPR] = ansi_opname[(int) TRUNC_MOD_EXPR];
  ansi_opname[(int) ROUND_MOD_EXPR] = ansi_opname[(int) TRUNC_MOD_EXPR];
  ansi_opname[(int) MINUS_EXPR] = get_identifier ("__mi");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MINUS_EXPR]) = 1;
  ansi_opname[(int) NEGATE_EXPR] = ansi_opname[(int) MINUS_EXPR];
  ansi_assopname[(int) MINUS_EXPR] = get_identifier ("__ami");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) MINUS_EXPR]) = 1;
  ansi_assopname[(int) NEGATE_EXPR] = ansi_assopname[(int) MINUS_EXPR];
  ansi_opname[(int) RSHIFT_EXPR] = get_identifier ("__rs");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) RSHIFT_EXPR]) = 1;
  ansi_assopname[(int) RSHIFT_EXPR] = get_identifier ("__ars");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) RSHIFT_EXPR]) = 1;
  ansi_opname[(int) NE_EXPR] = get_identifier ("__ne");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) NE_EXPR]) = 1;
  ansi_opname[(int) GT_EXPR] = get_identifier ("__gt");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) GT_EXPR]) = 1;
  ansi_opname[(int) GE_EXPR] = get_identifier ("__ge");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) GE_EXPR]) = 1;
  ansi_opname[(int) BIT_IOR_EXPR] = get_identifier ("__or");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) BIT_IOR_EXPR]) = 1;
  ansi_assopname[(int) BIT_IOR_EXPR] = get_identifier ("__aor");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) BIT_IOR_EXPR]) = 1;
  ansi_opname[(int) TRUTH_ANDIF_EXPR] = get_identifier ("__aa");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TRUTH_ANDIF_EXPR]) = 1;
  ansi_opname[(int) TRUTH_NOT_EXPR] = get_identifier ("__nt");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TRUTH_NOT_EXPR]) = 1;
  ansi_opname[(int) PREINCREMENT_EXPR] = get_identifier ("__pp");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) PREINCREMENT_EXPR]) = 1;
  ansi_opname[(int) POSTINCREMENT_EXPR] = ansi_opname[(int) PREINCREMENT_EXPR];
  ansi_opname[(int) MODIFY_EXPR] = get_identifier ("__as");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MODIFY_EXPR]) = 1;
  ansi_assopname[(int) NOP_EXPR] = ansi_opname[(int) MODIFY_EXPR];
  ansi_opname[(int) COMPOUND_EXPR] = get_identifier ("__cm");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) COMPOUND_EXPR]) = 1;
  ansi_opname[(int) EXACT_DIV_EXPR] = get_identifier ("__dv");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) EXACT_DIV_EXPR]) = 1;
  ansi_assopname[(int) EXACT_DIV_EXPR] = get_identifier ("__adv");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) EXACT_DIV_EXPR]) = 1;
  ansi_opname[(int) TRUNC_DIV_EXPR] = ansi_opname[(int) EXACT_DIV_EXPR];
  ansi_opname[(int) CEIL_DIV_EXPR] = ansi_opname[(int) EXACT_DIV_EXPR];
  ansi_opname[(int) FLOOR_DIV_EXPR] = ansi_opname[(int) EXACT_DIV_EXPR];
  ansi_opname[(int) ROUND_DIV_EXPR] = ansi_opname[(int) EXACT_DIV_EXPR];
  ansi_opname[(int) PLUS_EXPR] = get_identifier ("__pl");
  ansi_assopname[(int) TRUNC_DIV_EXPR] = ansi_assopname[(int) EXACT_DIV_EXPR];
  ansi_assopname[(int) CEIL_DIV_EXPR] = ansi_assopname[(int) EXACT_DIV_EXPR];
  ansi_assopname[(int) FLOOR_DIV_EXPR] = ansi_assopname[(int) EXACT_DIV_EXPR];
  ansi_assopname[(int) ROUND_DIV_EXPR] = ansi_assopname[(int) EXACT_DIV_EXPR];
  IDENTIFIER_OPNAME_P (ansi_opname[(int) PLUS_EXPR]) = 1;
  ansi_assopname[(int) PLUS_EXPR] = get_identifier ("__apl");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) PLUS_EXPR]) = 1;
  ansi_opname[(int) CONVERT_EXPR] = ansi_opname[(int) PLUS_EXPR];
  ansi_assopname[(int) CONVERT_EXPR] = ansi_assopname[(int) PLUS_EXPR];
  ansi_opname[(int) LSHIFT_EXPR] = get_identifier ("__ls");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) LSHIFT_EXPR]) = 1;
  ansi_assopname[(int) LSHIFT_EXPR] = get_identifier ("__als");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) LSHIFT_EXPR]) = 1;
  ansi_opname[(int) EQ_EXPR] = get_identifier ("__eq");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) EQ_EXPR]) = 1;
  ansi_opname[(int) LT_EXPR] = get_identifier ("__lt");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) LT_EXPR]) = 1;
  ansi_opname[(int) LE_EXPR] = get_identifier ("__le");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) LE_EXPR]) = 1;
  ansi_opname[(int) BIT_AND_EXPR] = get_identifier ("__ad");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) BIT_AND_EXPR]) = 1;
  ansi_assopname[(int) BIT_AND_EXPR] = get_identifier ("__aad");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) BIT_AND_EXPR]) = 1;
  ansi_opname[(int) ADDR_EXPR] = ansi_opname[(int) BIT_AND_EXPR];
  ansi_assopname[(int) ADDR_EXPR] = ansi_assopname[(int) BIT_AND_EXPR];
  ansi_opname[(int) BIT_XOR_EXPR] = get_identifier ("__er");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) BIT_XOR_EXPR]) = 1;
  ansi_assopname[(int) BIT_XOR_EXPR] = get_identifier ("__aer");
  IDENTIFIER_OPNAME_P (ansi_assopname[(int) BIT_XOR_EXPR]) = 1;
  ansi_opname[(int) TRUTH_ORIF_EXPR] = get_identifier ("__oo");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TRUTH_ORIF_EXPR]) = 1;
  ansi_opname[(int) BIT_NOT_EXPR] = get_identifier ("__co");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) BIT_NOT_EXPR]) = 1;
  ansi_opname[(int) PREDECREMENT_EXPR] = get_identifier ("__mm");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) PREDECREMENT_EXPR]) = 1;
  ansi_opname[(int) POSTDECREMENT_EXPR] = ansi_opname[(int) PREDECREMENT_EXPR];
  ansi_opname[(int) COMPONENT_REF] = get_identifier ("__rf");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) COMPONENT_REF]) = 1;
  ansi_opname[(int) MEMBER_REF] = get_identifier ("__rm");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MEMBER_REF]) = 1;
  ansi_opname[(int) CALL_EXPR] = get_identifier ("__cl");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) CALL_EXPR]) = 1;
  ansi_opname[(int) ARRAY_REF] = get_identifier ("__vc");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) ARRAY_REF]) = 1;
  ansi_opname[(int) NEW_EXPR] = get_identifier ("__nw");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) NEW_EXPR]) = 1;
  ansi_opname[(int) DELETE_EXPR] = get_identifier ("__dl");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) DELETE_EXPR]) = 1;
  ansi_opname[(int) VEC_NEW_EXPR] = get_identifier ("__vn");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) VEC_NEW_EXPR]) = 1;
  ansi_opname[(int) VEC_DELETE_EXPR] = get_identifier ("__vd");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) VEC_DELETE_EXPR]) = 1;
  ansi_opname[(int) TYPE_EXPR] = get_identifier (OPERATOR_TYPENAME_FORMAT);
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TYPE_EXPR]) = 1;

  /* This is not true: these operators are not defined in ANSI,
     but we need them anyway.  */
  ansi_opname[(int) MIN_EXPR] = get_identifier ("__mn");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MIN_EXPR]) = 1;
  ansi_opname[(int) MAX_EXPR] = get_identifier ("__mx");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MAX_EXPR]) = 1;
  ansi_opname[(int) COND_EXPR] = get_identifier ("__cn");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) COND_EXPR]) = 1;
  ansi_opname[(int) SIZEOF_EXPR] = get_identifier ("__sz");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) SIZEOF_EXPR]) = 1;

  init_method ();
  init_error ();
  gcc_obstack_init (&inline_text_obstack);
  inline_text_firstobj = (char *) obstack_alloc (&inline_text_obstack, 0);

  internal_filename = ggc_alloc_string (INTERNAL_FILENAME, 
					sizeof (INTERNAL_FILENAME));

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;
  input_filename = internal_filename;
  current_function_decl = NULL;

  maxtoken = 40;
  token_buffer = (char *) xmalloc (maxtoken + 2);

  ridpointers[(int) RID_INT] = get_identifier ("int");
  ridpointers[(int) RID_BOOL] = get_identifier ("bool");
  ridpointers[(int) RID_CHAR] = get_identifier ("char");
  ridpointers[(int) RID_VOID] = get_identifier ("void");
  ridpointers[(int) RID_FLOAT] = get_identifier ("float");
  ridpointers[(int) RID_DOUBLE] = get_identifier ("double");
  ridpointers[(int) RID_SHORT] = get_identifier ("short");
  ridpointers[(int) RID_LONG] = get_identifier ("long");
  ridpointers[(int) RID_UNSIGNED] = get_identifier ("unsigned");
  ridpointers[(int) RID_SIGNED] = get_identifier ("signed");
  ridpointers[(int) RID_INLINE] = get_identifier ("inline");
  ridpointers[(int) RID_CONST] = get_identifier ("const");
  ridpointers[(int) RID_RESTRICT] = get_identifier ("__restrict");
  ridpointers[(int) RID_VOLATILE] = get_identifier ("volatile");
  ridpointers[(int) RID_AUTO] = get_identifier ("auto");
  ridpointers[(int) RID_STATIC] = get_identifier ("static");
  ridpointers[(int) RID_EXTERN] = get_identifier ("extern");
  ridpointers[(int) RID_TYPEDEF] = get_identifier ("typedef");
  ridpointers[(int) RID_REGISTER] = get_identifier ("register");
  ridpointers[(int) RID_COMPLEX] = get_identifier ("__complex");

  /* C++ extensions. These are probably not correctly named.  */
  ridpointers[(int) RID_WCHAR] = get_identifier ("__wchar_t");
  class_type_node = build_int_2 (class_type, 0);
  TREE_TYPE (class_type_node) = class_type_node;
  ridpointers[(int) RID_CLASS] = class_type_node;

  record_type_node = build_int_2 (record_type, 0);
  TREE_TYPE (record_type_node) = record_type_node;
  ridpointers[(int) RID_RECORD] = record_type_node;

  union_type_node = build_int_2 (union_type, 0);
  TREE_TYPE (union_type_node) = union_type_node;
  ridpointers[(int) RID_UNION] = union_type_node;

  enum_type_node = build_int_2 (enum_type, 0);
  TREE_TYPE (enum_type_node) = enum_type_node;
  ridpointers[(int) RID_ENUM] = enum_type_node;

  ridpointers[(int) RID_VIRTUAL] = get_identifier ("virtual");
  ridpointers[(int) RID_EXPLICIT] = get_identifier ("explicit");
  ridpointers[(int) RID_EXPORT] = get_identifier ("export");
  ridpointers[(int) RID_FRIEND] = get_identifier ("friend");

  ridpointers[(int) RID_PUBLIC] = get_identifier ("public");
  ridpointers[(int) RID_PRIVATE] = get_identifier ("private");
  ridpointers[(int) RID_PROTECTED] = get_identifier ("protected");
  ridpointers[(int) RID_TEMPLATE] = get_identifier ("template");
  /* This is for ANSI C++.  */
  ridpointers[(int) RID_MUTABLE] = get_identifier ("mutable");

  /* Create the built-in __null node.  Note that we can't yet call for
     type_for_size here because integer_type_node and so forth are not
     set up.  Therefore, we don't set the type of these nodes until
     init_decl_processing.  */
  null_node = build_int_2 (0, 0);
  ridpointers[RID_NULL] = null_node;

  opname_tab[(int) COMPONENT_REF] = "->";
  opname_tab[(int) MEMBER_REF] = "->*";
  opname_tab[(int) INDIRECT_REF] = "*";
  opname_tab[(int) ARRAY_REF] = "[]";
  opname_tab[(int) MODIFY_EXPR] = "=";
  opname_tab[(int) INIT_EXPR] = "=";
  opname_tab[(int) NEW_EXPR] = "new";
  opname_tab[(int) DELETE_EXPR] = "delete";
  opname_tab[(int) VEC_NEW_EXPR] = "new []";
  opname_tab[(int) VEC_DELETE_EXPR] = "delete []";
  opname_tab[(int) COND_EXPR] = "?:";
  opname_tab[(int) CALL_EXPR] = "()";
  opname_tab[(int) PLUS_EXPR] = "+";
  opname_tab[(int) MINUS_EXPR] = "-";
  opname_tab[(int) MULT_EXPR] = "*";
  opname_tab[(int) TRUNC_DIV_EXPR] = "/";
  opname_tab[(int) CEIL_DIV_EXPR] = "(ceiling /)";
  opname_tab[(int) FLOOR_DIV_EXPR] = "(floor /)";
  opname_tab[(int) ROUND_DIV_EXPR] = "(round /)";
  opname_tab[(int) TRUNC_MOD_EXPR] = "%";
  opname_tab[(int) CEIL_MOD_EXPR] = "(ceiling %)";
  opname_tab[(int) FLOOR_MOD_EXPR] = "(floor %)";
  opname_tab[(int) ROUND_MOD_EXPR] = "(round %)";
  opname_tab[(int) EXACT_DIV_EXPR] = "/";
  opname_tab[(int) NEGATE_EXPR] = "-";
  opname_tab[(int) MIN_EXPR] = "<?";
  opname_tab[(int) MAX_EXPR] = ">?";
  opname_tab[(int) ABS_EXPR] = "abs";
  opname_tab[(int) FFS_EXPR] = "ffs";
  opname_tab[(int) LSHIFT_EXPR] = "<<";
  opname_tab[(int) RSHIFT_EXPR] = ">>";
  opname_tab[(int) BIT_IOR_EXPR] = "|";
  opname_tab[(int) BIT_XOR_EXPR] = "^";
  opname_tab[(int) BIT_AND_EXPR] = "&";
  opname_tab[(int) BIT_ANDTC_EXPR] = "&~";
  opname_tab[(int) BIT_NOT_EXPR] = "~";
  opname_tab[(int) TRUTH_ANDIF_EXPR] = "&&";
  opname_tab[(int) TRUTH_ORIF_EXPR] = "||";
  opname_tab[(int) TRUTH_AND_EXPR] = "strict &&";
  opname_tab[(int) TRUTH_OR_EXPR] = "strict ||";
  opname_tab[(int) TRUTH_NOT_EXPR] = "!";
  opname_tab[(int) LT_EXPR] = "<";
  opname_tab[(int) LE_EXPR] = "<=";
  opname_tab[(int) GT_EXPR] = ">";
  opname_tab[(int) GE_EXPR] = ">=";
  opname_tab[(int) EQ_EXPR] = "==";
  opname_tab[(int) NE_EXPR] = "!=";
  opname_tab[(int) IN_EXPR] = "in";
  opname_tab[(int) RANGE_EXPR] = "...";
  opname_tab[(int) CONVERT_EXPR] = "+";
  opname_tab[(int) ADDR_EXPR] = "&";
  opname_tab[(int) PREDECREMENT_EXPR] = "--";
  opname_tab[(int) PREINCREMENT_EXPR] = "++";
  opname_tab[(int) POSTDECREMENT_EXPR] = "--";
  opname_tab[(int) POSTINCREMENT_EXPR] = "++";
  opname_tab[(int) COMPOUND_EXPR] = ",";

  assignop_tab[(int) NOP_EXPR] = "=";
  assignop_tab[(int) PLUS_EXPR] =  "+=";
  assignop_tab[(int) CONVERT_EXPR] =  "+=";
  assignop_tab[(int) MINUS_EXPR] = "-=";
  assignop_tab[(int) NEGATE_EXPR] = "-=";
  assignop_tab[(int) MULT_EXPR] = "*=";
  assignop_tab[(int) INDIRECT_REF] = "*=";
  assignop_tab[(int) TRUNC_DIV_EXPR] = "/=";
  assignop_tab[(int) EXACT_DIV_EXPR] = "(exact /=)";
  assignop_tab[(int) CEIL_DIV_EXPR] = "(ceiling /=)";
  assignop_tab[(int) FLOOR_DIV_EXPR] = "(floor /=)";
  assignop_tab[(int) ROUND_DIV_EXPR] = "(round /=)";
  assignop_tab[(int) TRUNC_MOD_EXPR] = "%=";
  assignop_tab[(int) CEIL_MOD_EXPR] = "(ceiling %=)";
  assignop_tab[(int) FLOOR_MOD_EXPR] = "(floor %=)";
  assignop_tab[(int) ROUND_MOD_EXPR] = "(round %=)";
  assignop_tab[(int) MIN_EXPR] = "<?=";
  assignop_tab[(int) MAX_EXPR] = ">?=";
  assignop_tab[(int) LSHIFT_EXPR] = "<<=";
  assignop_tab[(int) RSHIFT_EXPR] = ">>=";
  assignop_tab[(int) BIT_IOR_EXPR] = "|=";
  assignop_tab[(int) BIT_XOR_EXPR] = "^=";
  assignop_tab[(int) BIT_AND_EXPR] = "&=";
  assignop_tab[(int) ADDR_EXPR] = "&=";

  init_filename_times ();

  /* Some options inhibit certain reserved words.
     Clear those words out of the hash table so they won't be recognized.  */
#define UNSET_RESERVED_WORD(STRING) \
  do { struct resword *s = is_reserved_word (STRING, sizeof (STRING) - 1); \
       if (s) s->name = ""; } while (0)

#if 0
  /* let's parse things, and if they use it, then give them an error.  */
  if (!flag_exceptions)
    {
      UNSET_RESERVED_WORD ("throw");
      UNSET_RESERVED_WORD ("try");
      UNSET_RESERVED_WORD ("catch");
    }
#endif

  if (flag_no_asm || flag_no_gnu_keywords)
    UNSET_RESERVED_WORD ("typeof");
  if (! flag_operator_names)
    {
      /* These are new ANSI keywords that may break code.  */
      UNSET_RESERVED_WORD ("and");
      UNSET_RESERVED_WORD ("and_eq");
      UNSET_RESERVED_WORD ("bitand");
      UNSET_RESERVED_WORD ("bitor");
      UNSET_RESERVED_WORD ("compl");
      UNSET_RESERVED_WORD ("not");
      UNSET_RESERVED_WORD ("not_eq");
      UNSET_RESERVED_WORD ("or");
      UNSET_RESERVED_WORD ("or_eq");
      UNSET_RESERVED_WORD ("xor");
      UNSET_RESERVED_WORD ("xor_eq");
    }

  token_count = init_cpp_parse ();
  interface_unknown = 1;

  ggc_add_tree_root (ansi_opname, LAST_CPLUS_TREE_CODE);
  ggc_add_tree_root (ansi_assopname, LAST_CPLUS_TREE_CODE);
  ggc_add_string_root (&internal_filename, 1);
  ggc_add_tree_root (ridpointers, RID_MAX);
  ggc_add_tree_root (&defarg_fns, 1);
  ggc_add_tree_root (&defarg_parm, 1);
  ggc_add_tree_root (&this_filename_time, 1);
  ggc_add_tree_root (&filename_times, 1);
  ggc_add_root (&impl_file_chain, 1, sizeof (impl_file_chain),
		mark_impl_file_chain);
  return filename;
}

void
finish_parse ()
{
#if USE_CPPLIB
  cpp_finish (&parse_in);
  errorcount += parse_in.errors;
#else
  fclose (finput);
#endif
}

void
reinit_parse_for_function ()
{
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;
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
    case TYPENAME:
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
	my_friendly_abort (80);
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
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  extern int yydebug;
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}


/* Mark ARG (which is really a struct impl_files **) for GC.  */

static void
mark_impl_file_chain (arg)
     void *arg;
{
  struct impl_files *ifs;

  ifs = *(struct impl_files **) arg;
  while (ifs)
    {
      ggc_mark_string (ifs->filename);
      ifs = ifs->next;
    }
}

/* Helper function to load global variables with interface
   information.  */

void
extract_interface_info ()
{
  tree fileinfo = 0;

  if (flag_alt_external_templates)
    {
      struct tinst_level *til = tinst_for_decl ();
  
      if (til)
	fileinfo = get_time_identifier (til->file);
    }
  if (!fileinfo)
    fileinfo = get_time_identifier (input_filename);
  fileinfo = TIME_IDENTIFIER_FILEINFO (fileinfo);
  interface_only = TREE_INT_CST_LOW (fileinfo);
  interface_unknown = TREE_INT_CST_HIGH (fileinfo);
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
      if (index (s1, '.') || index (t1, '.'))
	continue;

      if (*s1 == '\0' || s1[-1] != '.' || t1[-1] != '.')
	continue;

      /* A match.  */
      return 0;
    }

  /* No matches.  */
  return 1;
}

static void
cp_pragma_interface (main_filename)
     char *main_filename;
{
  tree fileinfo 
    = TIME_IDENTIFIER_FILEINFO (get_time_identifier (input_filename));

  if (impl_file_chain == 0)
    {
      /* If this is zero at this point, then we are
	 auto-implementing.  */
      if (main_input_filename == 0)
	main_input_filename = input_filename;

#ifdef AUTO_IMPLEMENT
      filename = file_name_nondirectory (main_input_filename);
      fi = get_time_identifier (filename);
      fi = TIME_IDENTIFIER_FILEINFO (fi);
      TREE_INT_CST_LOW (fi) = 0;
      TREE_INT_CST_HIGH (fi) = 1;
      /* Get default.  */
      impl_file_chain 
	= (struct impl_files *) xmalloc (sizeof (struct impl_files));
      impl_file_chain->filename = ggc_alloc_string (filename, -1);
      impl_file_chain->next = 0;
#endif
    }

  interface_only = interface_strcmp (main_filename);
#ifdef MULTIPLE_SYMBOL_SPACES
  if (! interface_only)
    interface_unknown = 0;
#else /* MULTIPLE_SYMBOL_SPACES */
  interface_unknown = 0;
#endif /* MULTIPLE_SYMBOL_SPACES */
  TREE_INT_CST_LOW (fileinfo) = interface_only;
  TREE_INT_CST_HIGH (fileinfo) = interface_unknown;
}

/* Note that we have seen a #pragma implementation for the key MAIN_FILENAME.
   We used to only allow this at toplevel, but that restriction was buggy
   in older compilers and it seems reasonable to allow it in the headers
   themselves, too.  It only needs to precede the matching #p interface.

   We don't touch interface_only or interface_unknown; the user must specify
   a matching #p interface for this to have any effect.  */

static void
cp_pragma_implementation (main_filename)
     char *main_filename;
{
  struct impl_files *ifiles = impl_file_chain;
  for (; ifiles; ifiles = ifiles->next)
    {
      if (! strcmp (ifiles->filename, main_filename))
	break;
    }
  if (ifiles == 0)
    {
      ifiles = (struct impl_files*) xmalloc (sizeof (struct impl_files));
      ifiles->filename = ggc_alloc_string (main_filename, -1);
      ifiles->next = impl_file_chain;
      impl_file_chain = ifiles;
    }
}

/* Set up the state required to correctly handle the definition of the
   inline function whose preparsed state has been saved in PI.  */

static void
begin_definition_of_inclass_inline (pi)
     struct pending_inline* pi;
{
  tree context;

  if (!pi->fndecl)
    return;

  /* If this is an inline function in a local class, we must make sure
     that we save all pertinent information about the function
     surrounding the local class.  */
  context = decl_function_context (pi->fndecl);
  if (context)
    push_function_context_to (context);

  feed_input (pi->buf, pi->len, pi->filename, pi->lineno);
  yychar = PRE_PARSED_FUNCTION_DECL;
  yylval.pi = pi;
  /* Pass back a handle to the rest of the inline functions, so that they
     can be processed later.  */
  DECL_PENDING_INLINE_INFO (pi->fndecl) = 0;
  DECL_PENDING_INLINE_P (pi->fndecl) = 0;
  interface_unknown = pi->interface == 1;
  interface_only  = pi->interface == 0;
}

/* Called from the top level: if there are any pending inlines to
   do, set up to process them now.  This function sets up the first function
   to be parsed; after it has been, the rule for fndef in parse.y will
   call process_next_inline to start working on the next one.  */

void
do_pending_inlines ()
{
  struct pending_inline *t;

  /* Oops, we're still dealing with the last batch.  */
  if (yychar == PRE_PARSED_FUNCTION_DECL)
    return;

  /* Reverse the pending inline functions, since
     they were cons'd instead of appended.  */
  {
    struct pending_inline *prev = 0, *tail;
    t = pending_inlines;
    pending_inlines = 0;

    for (; t; t = tail)
      {
	tail = t->next;
	t->next = prev;
	t->deja_vu = 1;
	prev = t;
      }
    t = prev;
  }

  if (t == 0)
    return;
	    
  /* Now start processing the first inline function.  */
  begin_definition_of_inclass_inline (t);
}

/* Called from the fndecl rule in the parser when the function just parsed
   was declared using a PRE_PARSED_FUNCTION_DECL (i.e. came from
   do_pending_inlines).  */

void
process_next_inline (i)
     struct pending_inline *i;
{
  tree context;
  context = decl_function_context (i->fndecl);  
  if (context)
    pop_function_context_from (context);
  i = i->next;
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    {
      error ("parse error at end of saved function text");

      /* restore_pending_input will abort unless yychar is either
         END_OF_SAVED_INPUT or YYEMPTY; since we already know we're
         hosed, feed back YYEMPTY.  */
    }
  yychar = YYEMPTY;
  end_input ();
  if (i)
    begin_definition_of_inclass_inline (i);
  else
    extract_interface_info ();
}

/* Since inline methods can refer to text which has not yet been seen,
   we store the text of the method in a structure which is placed in the
   DECL_PENDING_INLINE_INFO field of the FUNCTION_DECL.
   After parsing the body of the class definition, the FUNCTION_DECL's are
   scanned to see which ones have this field set.  Those are then digested
   one at a time.

   This function's FUNCTION_DECL will have a bit set in its common so
   that we know to watch out for it.  */

static void
consume_string (this_obstack, matching_char)
     register struct obstack *this_obstack;
     int matching_char;
{
  register int c;
  int starting_lineno;

#if USE_CPPLIB
  if (cpp_token == CPP_STRING)
    {
      /* The C preprocessor will warn about newlines in strings.  */
      obstack_grow (this_obstack, yy_cur, (yy_lim - yy_cur));
      yy_cur = yy_lim;
      lineno = parse_in.lineno;
      return;
    }
#endif

  starting_lineno = lineno;
  do
    {
      c = getch ();
      if (c == EOF)
	{
	  int save_lineno = lineno;
	  lineno = starting_lineno;
	  if (matching_char == '"')
	    error ("end of file encountered inside string constant");
	  else
	    error ("end of file encountered inside character constant");
	  lineno = save_lineno;
	  return;
	}
      if (c == '\\')
	{
	  obstack_1grow (this_obstack, c);
	  c = getch ();
	  obstack_1grow (this_obstack, c);

	  /* Make sure we continue the loop */
	  c = 0;
	  continue;
	}
      if (c == '\n')
	{
	  if (pedantic)
	    pedwarn ("ISO C++ forbids newline in string constant");
	  lineno++;
	}
      obstack_1grow (this_obstack, c);
    }
  while (c != matching_char);
}

struct pending_input {
  int yychar, eof;
  YYSTYPE yylval;
  struct obstack token_obstack;
  int first_token;
};

struct pending_input *
save_pending_input ()
{
  struct pending_input *p;
  p = (struct pending_input *) xmalloc (sizeof (struct pending_input));
  p->yychar = yychar;
  p->yylval = yylval;
  p->eof = end_of_file;
  yychar = YYEMPTY;
  p->first_token = first_token;
  p->token_obstack = token_obstack;

  first_token = 0;
  gcc_obstack_init (&token_obstack);
  end_of_file = 0;
  return p;
}

void
restore_pending_input (p)
     struct pending_input *p;
{
  my_friendly_assert (yychar == YYEMPTY || yychar == END_OF_SAVED_INPUT, 230);
  yychar = p->yychar;
  yylval = p->yylval;
  first_token = p->first_token;
  obstack_free (&token_obstack, (char *) 0);
  token_obstack = p->token_obstack;
  end_of_file = p->eof;
  free (p);
}

/* Unget character CH from the input stream.
   If RESCAN is non-zero, then we want to `see' this
   character as the next input token.  */

void
yyungetc (ch, rescan)
     int ch;
     int rescan;
{
  /* Unget a character from the input stream.  */
  if (yychar == YYEMPTY || rescan == 0)
    {
      /* If we're putting back a brace, undo the change in indent_level
	 from the first time we saw it.  */
      if (ch == '{')
	indent_level--;
      else if (ch == '}')
	indent_level++;

      put_back (ch);
    }
  else
    {
      yychar = ch;
    }
}

void
clear_inline_text_obstack ()
{
  obstack_free (&inline_text_obstack, inline_text_firstobj);
}

/* This function stores away the text for an inline function that should
   be processed later.  It decides how much later, and may need to move
   the info between obstacks; therefore, the caller should not refer to
   the T parameter after calling this function.  */

static void
store_pending_inline (decl, t)
     tree decl;
     struct pending_inline *t;
{
  t->fndecl = decl;
  DECL_PENDING_INLINE_INFO (decl) = t;
  DECL_PENDING_INLINE_P (decl) = 1;

  /* Because we use obstacks, we must process these in precise order.  */
  t->next = pending_inlines;
  pending_inlines = t;
}

void
reinit_parse_for_method (yychar, decl)
     int yychar;
     tree decl;
{
  int len;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;

  reinit_parse_for_block (yychar, &inline_text_obstack);

  len = obstack_object_size (&inline_text_obstack);
  if (decl == void_type_node
      || (current_class_type && TYPE_REDEFINED (current_class_type)))
    {
      /* Happens when we get two declarations of the same
	 function in the same scope.  */
      char *buf = obstack_finish (&inline_text_obstack);
      obstack_free (&inline_text_obstack, buf);
      return;
    }
  else
    {
      struct pending_inline *t;
      char *buf = obstack_finish (&inline_text_obstack);

      t = (struct pending_inline *) obstack_alloc (&inline_text_obstack,
						   sizeof (struct pending_inline));
      t->lineno = starting_lineno;
      t->filename = starting_filename;
      t->token = YYEMPTY;
      t->token_value = 0;
      t->buf = buf;
      t->len = len;
      t->deja_vu = 0;
#if 0
      if (interface_unknown && processing_template_defn && flag_external_templates && ! DECL_IN_SYSTEM_HEADER (decl))
	warn_if_unknown_interface (decl);
#endif
      t->interface = (interface_unknown ? 1 : (interface_only ? 0 : 2));
      store_pending_inline (decl, t);
    }
}

/* Consume a block -- actually, a method beginning
   with `:' or `{' -- and save it away on the specified obstack.  */

void
reinit_parse_for_block (pyychar, obstackp)
     int pyychar;
     struct obstack *obstackp;
{
  register int c;
  int blev = 1;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;
  int len;
  int look_for_semicolon = 0;
  int look_for_lbrac = 0;

  if (pyychar == '{')
    {
      obstack_1grow (obstackp, '{');
      /* We incremented indent_level in yylex; undo that.  */
      indent_level--;
    }
  else if (pyychar == '=')
    look_for_semicolon = 1;
  else if (pyychar == ':')
    {
      obstack_1grow (obstackp, pyychar);
      /* Add a space so we don't get confused by ': ::A(20)'.  */
      obstack_1grow (obstackp, ' ');
      look_for_lbrac = 1;
      blev = 0;
    }
  else if (pyychar == RETURN_KEYWORD)
    {
      obstack_grow (obstackp, "return", 6);
      look_for_lbrac = 1;
      blev = 0;
    }
  else if (pyychar == TRY)
    {
      obstack_grow (obstackp, "try", 3);
      look_for_lbrac = 1;
      blev = 0;
    }
  else
    {
      yyerror ("parse error in method specification");
      obstack_1grow (obstackp, '{');
    }

  c = getch ();

  while (c != EOF)
    {
      int this_lineno = lineno;

      saving_parse_to_obstack = 1;
      c = skip_white_space (c);
      saving_parse_to_obstack = 0;

      /* Don't lose our cool if there are lots of comments.  */
      if (lineno == this_lineno + 1)
	obstack_1grow (obstackp, '\n');
      else if (lineno == this_lineno)
	;
      else if (lineno - this_lineno < 10)
	{
	  int i;
	  for (i = lineno - this_lineno; i > 0; i--)
	    obstack_1grow (obstackp, '\n');
	}
      else
	{
	  char buf[16];
	  sprintf (buf, "\n# %d \"", lineno);
	  len = strlen (buf);
	  obstack_grow (obstackp, buf, len);

	  len = strlen (input_filename);
	  obstack_grow (obstackp, input_filename, len);
	  obstack_1grow (obstackp, '\"');
	  obstack_1grow (obstackp, '\n');
	}

      while (c > ' ')		/* ASCII dependent...  */
	{
	  obstack_1grow (obstackp, c);
	  if (c == '{')
	    {
	      look_for_lbrac = 0;
	      blev++;
	    }
	  else if (c == '}')
	    {
	      blev--;
	      if (blev == 0 && !look_for_semicolon)
		{
		  if (pyychar == TRY)
		    {
		      if (peekyylex () == CATCH)
			{
			  yylex ();
			  obstack_grow (obstackp, " catch ", 7);
			  look_for_lbrac = 1;
			}
		      else
			{
			  yychar = '{';
			  goto done;
			}
		    }
		  else
		    {
		      goto done;
		    }
		}
	    }
	  else if (c == '\\')
	    {
	      /* Don't act on the next character...e.g, doing an escaped
		 double-quote.  */
	      c = getch ();
	      if (c == EOF)
		{
		  error_with_file_and_line (starting_filename,
					    starting_lineno,
					    "end of file read inside definition");
		  goto done;
		}
	      obstack_1grow (obstackp, c);
	    }
	  else if (c == '\"')
	    consume_string (obstackp, c);
	  else if (c == '\'')
	    consume_string (obstackp, c);
	  else if (c == ';')
	    {
	      if (look_for_lbrac)
		{
		  error ("function body for constructor missing");
		  obstack_1grow (obstackp, '{');
		  obstack_1grow (obstackp, '}');
		  len += 2;
		  goto done;
		}
	      else if (look_for_semicolon && blev == 0)
		goto done;
	    }
	  c = getch ();
	}

      if (c == EOF)
	{
	  error_with_file_and_line (starting_filename,
				    starting_lineno,
				    "end of file read inside definition");
	  goto done;
	}
      else if (c != '\n')
	{
	  obstack_1grow (obstackp, c);
	  c = getch ();
	}
    }
 done:
  obstack_1grow (obstackp, '\0');
}

/* Consume a no-commas expression -- actually, a default argument -- and
   save it away on the specified obstack.  */

static void
reinit_parse_for_expr (obstackp)
     struct obstack *obstackp;
{
  register int c;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;
  int len;
  int plev = 0;

  c = getch ();

  while (c != EOF)
    {
      int this_lineno = lineno;

      saving_parse_to_obstack = 1;
      c = skip_white_space (c);
      saving_parse_to_obstack = 0;

      /* Don't lose our cool if there are lots of comments.  */
      if (lineno == this_lineno + 1)
	obstack_1grow (obstackp, '\n');
      else if (lineno == this_lineno)
	;
      else if (lineno - this_lineno < 10)
	{
	  int i;
	  for (i = lineno - this_lineno; i > 0; --i)
	    obstack_1grow (obstackp, '\n');
	}
      else
	{
	  char buf[16];
	  sprintf (buf, "\n# %d \"", lineno);
	  len = strlen (buf);
	  obstack_grow (obstackp, buf, len);

	  len = strlen (input_filename);
	  obstack_grow (obstackp, input_filename, len);
	  obstack_1grow (obstackp, '\"');
	  obstack_1grow (obstackp, '\n');
	}

      while (c > ' ')		/* ASCII dependent...  */
	{
	  if (plev <= 0 && (c == ')' || c == ','))
	    {
	      put_back (c);
	      goto done;
	    }
	  obstack_1grow (obstackp, c);
	  if (c == '(' || c == '[')
	    ++plev;
	  else if (c == ']' || c == ')')
	    --plev;
	  else if (c == '\\')
	    {
	      /* Don't act on the next character...e.g, doing an escaped
		 double-quote.  */
	      c = getch ();
	      if (c == EOF)
		{
		  error_with_file_and_line (starting_filename,
					    starting_lineno,
					    "end of file read inside definition");
		  goto done;
		}
	      obstack_1grow (obstackp, c);
	    }
	  else if (c == '\"')
	    consume_string (obstackp, c);
	  else if (c == '\'')
	    consume_string (obstackp, c);
	  c = getch ();
	}

      if (c == EOF)
	{
	  error_with_file_and_line (starting_filename,
				    starting_lineno,
				    "end of file read inside definition");
	  goto done;
	}
      else if (c != '\n')
	{
	  obstack_1grow (obstackp, c);
	  c = getch ();
	}
    }
 done:
  obstack_1grow (obstackp, '\0');
}

int do_snarf_defarg;

/* Decide whether the default argument we are about to see should be
   gobbled up as text for later parsing.  */

void
maybe_snarf_defarg ()
{
  if (current_class_type && TYPE_BEING_DEFINED (current_class_type))
    do_snarf_defarg = 1;
}

tree
snarf_defarg ()
{
  int len;
  char *buf;
  tree arg;

  reinit_parse_for_expr (&inline_text_obstack);
  len = obstack_object_size (&inline_text_obstack);
  buf = obstack_finish (&inline_text_obstack);

  arg = make_node (DEFAULT_ARG);
  DEFARG_LENGTH (arg) = len - 1;
  DEFARG_POINTER (arg) = buf;

  return arg;
}

/* Called from grokfndecl to note a function decl with unparsed default
   arguments for later processing.  Also called from grokdeclarator
   for function types with unparsed defargs; the call from grokfndecl
   will always come second, so we can overwrite the entry from the type.  */

void
add_defarg_fn (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    TREE_VALUE (defarg_fns) = decl;
  else
    defarg_fns = tree_cons (current_class_type, decl, defarg_fns);  
}

/* Helper for do_pending_defargs.  Starts the parsing of a default arg.  */

static void
feed_defarg (f, p)
     tree f, p;
{
  tree d = TREE_PURPOSE (p);
  char *file;
  int line;
  if (TREE_CODE (f) == FUNCTION_DECL)
    {
      line = DECL_SOURCE_LINE (f);
      file = DECL_SOURCE_FILE (f);
    }
  else
    {
      line = lineno;
      file = input_filename;
    }

  feed_input (DEFARG_POINTER (d), DEFARG_LENGTH (d), file, line);
  yychar = DEFARG_MARKER;
  yylval.ttype = p;
}

/* Helper for do_pending_defargs.  Ends the parsing of a default arg.  */

static void
finish_defarg ()
{
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    {
      error ("parse error at end of saved function text");

      /* restore_pending_input will abort unless yychar is either
         END_OF_SAVED_INPUT or YYEMPTY; since we already know we're
         hosed, feed back YYEMPTY.  */
    }
  yychar = YYEMPTY;
  end_input ();
}  

/* Main function for deferred parsing of default arguments.  Called from
   the parser.  */

void
do_pending_defargs ()
{
  if (defarg_parm)
    finish_defarg ();

  for (; defarg_fns; defarg_fns = TREE_CHAIN (defarg_fns))
    {
      tree defarg_fn = TREE_VALUE (defarg_fns);
      if (defarg_parm == NULL_TREE)
	{
	  push_nested_class (TREE_PURPOSE (defarg_fns), 1);
	  pushlevel (0);
	  if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	    maybe_begin_member_template_processing (defarg_fn);

	  if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	    {
#if 0
	      tree p;
	      for (p = DECL_ARGUMENTS (defarg_fn); p; p = TREE_CHAIN (p))
		pushdecl (copy_node (p));
#endif
	      defarg_parm = TYPE_ARG_TYPES (TREE_TYPE (defarg_fn));
	    }
	  else
	    defarg_parm = TYPE_ARG_TYPES (defarg_fn);
	}
      else
	defarg_parm = TREE_CHAIN (defarg_parm);

      for (; defarg_parm; defarg_parm = TREE_CHAIN (defarg_parm))
	if (TREE_PURPOSE (defarg_parm)
	    && TREE_CODE (TREE_PURPOSE (defarg_parm)) == DEFAULT_ARG)
	  {
	    feed_defarg (defarg_fn, defarg_parm);

	    /* Return to the parser, which will process this defarg
	       and call us again.  */
	    return;
	  }

      if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	{
	  maybe_end_member_template_processing ();
	  check_default_args (defarg_fn);
	}

      poplevel (0, 0, 0);
      pop_nested_class ();
    }
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
       && yychar != TYPENAME
       && yychar != CV_QUALIFIER
       && yychar != SELFNAME)
      || end_of_file)
    {
      if (ANON_AGGRNAME_P (TYPE_IDENTIFIER (type)))
	error ("semicolon missing after %s declaration",
	       TREE_CODE (type) == ENUMERAL_TYPE ? "enum" : "struct");
      else
	cp_error ("semicolon missing after declaration of `%T'", type);
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
    my_friendly_abort (60);
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
      if (TYPE_P (type))
	note_got_semicolon (type);
    }
  clear_anon_tags ();
}

/* Iff C is a carriage return, warn about it - if appropriate -
   and return nonzero.  */
static int
whitespace_cr (c)
     int c;
{
  static int newline_warning = 0;

  if (c == '\r')
    {
      /* ANSI C says the effects of a carriage return in a source file
	 are undefined.  */
      if (pedantic && !newline_warning)
	{
	  warning ("carriage return in source file (we only warn about the first carriage return)");
	  newline_warning = 1;
	}
      return 1;
    }
  return 0;
}

/* If C is not whitespace, return C.
   Otherwise skip whitespace and return first nonwhite char read.  */

static int
skip_white_space (c)
     register int c;
{
  for (;;)
    {
      switch (c)
	{
	  /* We don't recognize comments here, because
	     cpp output can include / and * consecutively as operators.
	     Also, there's no need, since cpp removes all comments.  */

	case '\n':
	  if (linemode)
	    {
	      put_back (c);
	      return EOF;
	    }
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\v':
	case '\b':
#if USE_CPPLIB
	  /* While processing a # directive we don't get CPP_HSPACE
	     tokens, so we also need to handle whitespace the normal way.  */
	  if (cpp_token == CPP_HSPACE)
	    c = yy_get_token ();
	  else
#endif
	    c = getch ();
	  break;

	case '\r':
	  whitespace_cr (c);
	  c = getch ();
	  break;

	case '\\':
	  c = getch ();
	  if (c == '\n')
	    {
	      lineno++;
	      c = getch ();
	    }
	  else if (c == 'u')
	    c = read_ucs (4);
	  else if (c == 'U')
	    c = read_ucs (8);
	  else
	    error ("stray '\\' in program");
	  break;

	default:
	  return (c);
	}
    }
}

/* Make the token buffer longer, preserving the data in it.
   P should point to just beyond the last valid character in the old buffer.
   The value we return is a pointer to the new buffer
   at a place corresponding to P.  */

static void
extend_token_buffer_to (size)
     int size;
{
  do
    maxtoken = maxtoken * 2 + 10;
  while (maxtoken < size);
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);
}

static char *
extend_token_buffer (p)
     const char *p;
{
  int offset = p - token_buffer;
  extend_token_buffer_to (offset);
  return token_buffer + offset;
}

#if defined HANDLE_PRAGMA
/* Local versions of these macros, that can be passed as function pointers.  */
static int
pragma_getc ()
{
  return getch ();
}

static void
pragma_ungetc (arg)
     int arg;
{
  put_back (arg);
}
#endif

static int
read_line_number (num)
     int *num;
{
  register int token = real_yylex ();

  if (token == CONSTANT
      && TREE_CODE (yylval.ttype) == INTEGER_CST)
    {
      *num = TREE_INT_CST_LOW (yylval.ttype);
      return 1;
    }
  else
    {
      if (token != END_OF_LINE)
	error ("invalid #-line");
      return 0;
    }
}

/* At the beginning of a line, increment the line number
   and process any #-directive on this line.
   If the line is a #-directive, read the entire line and return a newline.
   Otherwise, return the line's first non-whitespace character.

   Note that in the case of USE_CPPLIB, we get the whole line as one
   CPP_DIRECTIVE token.  */

static int
check_newline ()
{
  register int c;
  register int token;
  int saw_line;
  enum { act_none, act_push, act_pop } action;
  int action_number, l;
  int entering_c_header;
  char *new_file;

 restart:
  /* Read first nonwhite char on the line.  Do this before incrementing the
     line number, in case we're at the end of saved text.  */

#ifdef USE_CPPLIB
  c = getch ();
  /* In some cases where we're leaving an include file, we can get multiple
     CPP_HSPACE tokens in a row, so we need to loop.  */
  while (cpp_token == CPP_HSPACE)
    c = yy_get_token ();
#else
  do
    c = getch ();
  while (c == ' ' || c == '\t');
#endif

  lineno++;

  if (c != '#')
    {
      /* Sequences of multiple newlines are very common; optimize them.  */
      if (c == '\n')
	goto restart;

      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Don't read beyond this line.  */
  saw_line = 0;
  linemode = 1;
  
#if USE_CPPLIB
  if (cpp_token == CPP_VSPACE)
    {
      /* Format is "<space> <line number> <filename> <newline>".
	 Only the line number is interesting, and even that
	 we can get more efficiently than scanning the line.  */
      yy_cur = yy_lim - 1;
      lineno = parse_in.lineno - 1;
      goto skipline;
    }
#endif

  token = real_yylex ();

  if (token == IDENTIFIER)
    {
      /* If a letter follows, then if the word here is `line', skip
	 it and ignore it; otherwise, ignore the line, with an error
	 if the word isn't `pragma'.  */

      const char *name = IDENTIFIER_POINTER (yylval.ttype);

      if (!strcmp (name, "pragma"))
	{
	  token = real_yylex ();
	  if (token != IDENTIFIER
	      || TREE_CODE (yylval.ttype) != IDENTIFIER_NODE)
	    goto skipline;
	  
	  /* If this is 1, we handled it; if it's -1, it was one we
	     wanted but had something wrong with it.  Only if it's
	     0 was it not handled.  */
	  if (handle_cp_pragma (IDENTIFIER_POINTER (yylval.ttype)))
	    goto skipline;

#ifdef HANDLE_PRAGMA
	  /* We invoke HANDLE_PRAGMA before HANDLE_GENERIC_PRAGMAS
	     (if both are defined), in order to give the back
	     end a chance to override the interpretation of
	     SYSV style pragmas.  */
	  if (HANDLE_PRAGMA (pragma_getc, pragma_ungetc,
			     IDENTIFIER_POINTER (yylval.ttype)))
	    goto skipline;
#endif /* HANDLE_PRAGMA */
	      
#ifdef HANDLE_GENERIC_PRAGMAS
	  if (handle_generic_pragma (token))
	    goto skipline;
#endif /* HANDLE_GENERIC_PRAGMAS */

	  /* Issue a warning message if we have been asked to do so.
	     Ignoring unknown pragmas in system header file unless
	     an explcit -Wunknown-pragmas has been given. */
	  if (warn_unknown_pragmas > 1
	      || (warn_unknown_pragmas && ! in_system_header))
	    warning ("ignoring pragma: %s", token_buffer);

	  goto skipline;
	}
      else if (!strcmp (name, "define"))
	{
	  debug_define (lineno, GET_DIRECTIVE_LINE ());
	  goto skipline;
	}
      else if (!strcmp (name, "undef"))
	{
	  debug_undef (lineno, GET_DIRECTIVE_LINE ());
	  goto skipline;
	}
      else if (!strcmp (name, "line"))
	{
	  saw_line = 1;
	  token = real_yylex ();
	  goto linenum;
	}
      else if (!strcmp (name, "ident"))
	{
	  /* #ident.  The pedantic warning is now in cpp.  */

	  /* Here we have just seen `#ident '.
	     A string constant should follow.  */

	  token = real_yylex ();
	  if (token == END_OF_LINE)
	    goto skipline;
	  if (token != STRING
	      || TREE_CODE (yylval.ttype) != STRING_CST)
	    {
	      error ("invalid #ident");
	      goto skipline;
	    }

	  if (! flag_no_ident)
	    {
#ifdef ASM_OUTPUT_IDENT
	      ASM_OUTPUT_IDENT (asm_out_file,
				TREE_STRING_POINTER (yylval.ttype));
#endif
	    }

	  /* Skip the rest of this line.  */
	  goto skipline;
	}

      error ("undefined or invalid # directive `%s'", name);
      goto skipline;
    }

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (token == END_OF_LINE)
    goto skipline;

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  if (token != CONSTANT
      || TREE_CODE (yylval.ttype) != INTEGER_CST)
    {
      error ("invalid #-line");
      goto skipline;
    }

  /* subtract one, because it is the following line that
     gets the specified number */

  l = TREE_INT_CST_LOW (yylval.ttype) - 1;

  /* More follows: it must be a string constant (filename).
     It would be neat to use cpplib to quickly process the string, but
     (1) we don't have a handy tokenization of the string, and
     (2) I don't know how well that would work in the presense
     of filenames that contain wide characters.  */

  if (saw_line || saving_parse_to_obstack)
    {
      /* Don't treat \ as special if we are processing #line 1 "...".
	 If you want it to be treated specially, use # 1 "...". Also
	 ignore these if saving to an obstack for later parsing. */
      ignore_escape_flag = 1;
    }

  /* Read the string constant.  */
  token = real_yylex ();

  ignore_escape_flag = 0;

  if (token == END_OF_LINE)
    {
      /* No more: store the line number and check following line.  */
      lineno = l;
      goto skipline;
    }

  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
    {
      error ("invalid #line");
      goto skipline;
    }

  /* Changing files again.  This means currently collected time
     is charged against header time, and body time starts back at 0.  */
  if (flag_detailed_statistics)
    {
      int this_time = get_run_time ();
      tree time_identifier = get_time_identifier (TREE_STRING_POINTER (yylval.ttype));
      header_time += this_time - body_time;
      TREE_INT_CST_LOW (TIME_IDENTIFIER_TIME (this_filename_time))
	+= this_time - body_time;
      this_filename_time = time_identifier;
      body_time = this_time;
    }

  new_file = TREE_STRING_POINTER (yylval.ttype);

  GNU_xref_file (new_file);
      
  if (main_input_filename == 0)
    {
      struct impl_files *ifiles = impl_file_chain;

      if (ifiles)
	{
	  while (ifiles->next)
	    ifiles = ifiles->next;
	  ifiles->filename = file_name_nondirectory (new_file);
	}

      main_input_filename = new_file;
    }

  action = act_none;
  action_number = 0;

  /* Each change of file name
     reinitializes whether we are now in a system header.  */
  in_system_header = 0;
  entering_c_header = 0;

  if (!read_line_number (&action_number) && input_file_stack)
    {
      input_file_stack->name = input_filename = new_file;
      input_file_stack->line = lineno = l;
    }

  /* `1' after file name means entering new file.
     `2' after file name means just left a file.  */

  if (action_number == 1)
    {
      action = act_push;
      read_line_number (&action_number);
    }
  else if (action_number == 2)
    {
      action = act_pop;
      read_line_number (&action_number);
    }
  if (action_number == 3)
    {
      /* `3' after file name means this is a system header file.  */
      in_system_header = 1;
      read_line_number (&action_number);
    }
  if (action_number == 4)
    {
      /* `4' after file name means this is a C header file.  */
      entering_c_header = 1;
      read_line_number (&action_number);
    }

  /* Do the actions implied by the preceding numbers.  */

  if (action == act_push)
    {
      /* Pushing to a new file.  */
      push_srcloc (new_file, l);
      input_file_stack->indent_level = indent_level;
      debug_start_source_file (input_filename);
      if (c_header_level)
	++c_header_level;
      else if (entering_c_header)
	{
	  c_header_level = 1;
	  ++pending_lang_change;
	}
    }
  else if (action == act_pop)
    {
      /* Popping out of a file.  */
      if (input_file_stack->next)
	{
	  if (c_header_level && --c_header_level == 0)
	    {
	      if (entering_c_header)
		warning ("badly nested C headers from preprocessor");
	      --pending_lang_change;
	    }

	  if (indent_level != input_file_stack->indent_level)
	    {
	      warning_with_file_and_line
		(input_filename, lineno,
		 "This file contains more `%c's than `%c's.",
		 indent_level > input_file_stack->indent_level ? '{' : '}',
		 indent_level > input_file_stack->indent_level ? '}' : '{');
	    }

	  pop_srcloc ();
	  input_file_stack->name = new_file;
	  debug_end_source_file (input_file_stack->line);
	}
      else
	error ("#-lines for entering and leaving files don't match");
    }

  input_filename = new_file;
  lineno = l;

  extract_interface_info ();

  /* skip the rest of this line.  */
 skipline:
  linemode = 0;
  end_of_file = 0;

  do
    c = getch ();
  while (c != '\n' && c != EOF);
  return c;
}

#ifdef HANDLE_GENERIC_PRAGMAS

/* Handle a #pragma directive.
   TOKEN is the token we read after `#pragma'.  Processes the entire input
   line and return non-zero iff the pragma has been successfully parsed.  */

/* This function has to be in this file, in order to get at
   the token types.  */

static int
handle_generic_pragma (token)
     register int token;
{
  for (;;)
    {
      switch (token)
	{
	case IDENTIFIER:
	case TYPENAME:
        case STRING:
        case CONSTANT:
	  handle_pragma_token (token_buffer, yylval.ttype);
	  break;

	case LEFT_RIGHT:
	  handle_pragma_token ("(", NULL_TREE);
	  handle_pragma_token (")", NULL_TREE);
	  break;

	case END_OF_LINE:
	  return handle_pragma_token (NULL_PTR, NULL_TREE);

	default:
	  handle_pragma_token (token_buffer, NULL_TREE);
	}
      
      token = real_yylex ();
    }
}
#endif /* HANDLE_GENERIC_PRAGMAS */

static int
handle_cp_pragma (pname)
     const char *pname;
{
  register int token;

  if (! strcmp (pname, "vtable"))
    {
      /* More follows: it must be a string constant (class name).  */
      token = real_yylex ();
      if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #pragma vtable");
	  return -1;
	}

      pending_vtables
	= tree_cons (NULL_TREE,
		     get_identifier (TREE_STRING_POINTER (yylval.ttype)),
		     pending_vtables);
      token = real_yylex ();
      if (token != END_OF_LINE)
	warning ("trailing characters ignored");
      return 1;
    }
  else if (! strcmp (pname, "unit"))
    {
      /* More follows: it must be a string constant (unit name).  */
      token = real_yylex ();
      if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #pragma unit");
	  return -1;
	}
      token = real_yylex ();
      if (token != END_OF_LINE)
	warning ("trailing characters ignored");
      return 1;
    }
  else if (! strcmp (pname, "interface"))
    {
      char *main_filename = input_filename;

      main_filename = file_name_nondirectory (main_filename);

      token = real_yylex ();
      
      if (token != END_OF_LINE)
	{
	  if (token != STRING
	      || TREE_CODE (yylval.ttype) != STRING_CST)
	    {
	      error ("invalid `#pragma interface'");
	      return -1;
	    }
	  main_filename = TREE_STRING_POINTER (yylval.ttype);
	  token = real_yylex ();
	}

      if (token != END_OF_LINE)
	warning ("garbage after `#pragma interface' ignored");

      cp_pragma_interface (main_filename);

      return 1;
    }
  else if (! strcmp (pname, "implementation"))
    {
      char *main_filename = main_input_filename ? main_input_filename : input_filename;

      main_filename = file_name_nondirectory (main_filename);

      token = real_yylex ();

      if (token != END_OF_LINE)
	{
	  if (token != STRING
	      || TREE_CODE (yylval.ttype) != STRING_CST)
	    {
	      error ("invalid `#pragma implementation'");
	      return -1;
	    }
	  main_filename = TREE_STRING_POINTER (yylval.ttype);
	  token = real_yylex ();
	}

      if (token != END_OF_LINE)
	warning ("garbage after `#pragma implementation' ignored");

      cp_pragma_implementation (main_filename);

      return 1;
    }

  return 0;
}

void
do_pending_lang_change ()
{
  for (; pending_lang_change > 0; --pending_lang_change)
    push_lang_context (lang_name_c);
  for (; pending_lang_change < 0; ++pending_lang_change)
    pop_lang_context ();
}

/* Parse a '\uNNNN' or '\UNNNNNNNN' sequence.

   [lex.charset]: The character designated by the universal-character-name 
   \UNNNNNNNN is that character whose character short name in ISO/IEC 10646
   is NNNNNNNN; the character designated by the universal-character-name
   \uNNNN is that character whose character short name in ISO/IEC 10646 is
   0000NNNN. If the hexadecimal value for a universal character name is
   less than 0x20 or in the range 0x7F-0x9F (inclusive), or if the
   universal character name designates a character in the basic source
   character set, then the program is ill-formed.

   We assume that wchar_t is Unicode, so we don't need to do any
   mapping.  Is this ever wrong?  */

static int
read_ucs (length)
     int length;
{
  unsigned int code = 0;
  int c;

  for (; length; --length)
    {
      c = getch ();
      if (! ISXDIGIT (c))
	{
	  error ("non hex digit '%c' in universal-character-name", c);
	  put_back (c);
	  break;
	}
      code <<= 4;
      if (c >= 'a' && c <= 'f')
	code += c - 'a' + 10;
      if (c >= 'A' && c <= 'F')
	code += c - 'A' + 10;
      if (c >= '0' && c <= '9')
	code += c - '0';
    }

#ifdef TARGET_EBCDIC
  sorry ("universal-character-name on EBCDIC target");
  return 0x3F;
#endif

  if (code > 0x9f && !(code & 0x80000000))
    /* True extended character, OK.  */;
  else if (code >= 0x20 && code < 0x7f)
    {
      /* ASCII printable character.  The C character set consists of all of
	 these except $, @ and `.  We use hex escapes so that this also
	 works with EBCDIC hosts.  */
      if (code != 0x24 && code != 0x40 && code != 0x60)
	error ("universal-character-name designates `%c', part of the basic source character set", code);
    }
  else
    error ("invalid universal-character-name");
  return code;
}

/* Returns nonzero if C is a universal-character-name.  Give an error if it
   is not one which may appear in an identifier, as per [extendid].  */

static inline int
is_extended_char (c)
     int c;
{
#ifdef TARGET_EBCDIC
  return 0;
#else
  /* ASCII.  */
  if (c < 0x7f)
    return 0;
  
  return is_extended_char_1 (c);
#endif
}

static int
is_extended_char_1 (c)
     int c;
{
  /* None of the valid chars are outside the Basic Multilingual Plane (the
     low 16 bits).  */
  if (c > 0xffff)
    {
      error ("universal-character-name `\\U%08x' not valid in identifier", c);
      return 1;
    }
  
  /* Latin */
  if ((c >= 0x00c0 && c <= 0x00d6)
      || (c >= 0x00d8 && c <= 0x00f6)
      || (c >= 0x00f8 && c <= 0x01f5)
      || (c >= 0x01fa && c <= 0x0217)
      || (c >= 0x0250 && c <= 0x02a8)
      || (c >= 0x1e00 && c <= 0x1e9a)
      || (c >= 0x1ea0 && c <= 0x1ef9))
    return 1;

  /* Greek */
  if ((c == 0x0384)
      || (c >= 0x0388 && c <= 0x038a)
      || (c == 0x038c)
      || (c >= 0x038e && c <= 0x03a1)
      || (c >= 0x03a3 && c <= 0x03ce)
      || (c >= 0x03d0 && c <= 0x03d6)
      || (c == 0x03da)
      || (c == 0x03dc)
      || (c == 0x03de)
      || (c == 0x03e0)
      || (c >= 0x03e2 && c <= 0x03f3)
      || (c >= 0x1f00 && c <= 0x1f15)
      || (c >= 0x1f18 && c <= 0x1f1d)
      || (c >= 0x1f20 && c <= 0x1f45)
      || (c >= 0x1f48 && c <= 0x1f4d)
      || (c >= 0x1f50 && c <= 0x1f57)
      || (c == 0x1f59)
      || (c == 0x1f5b)
      || (c == 0x1f5d)
      || (c >= 0x1f5f && c <= 0x1f7d)
      || (c >= 0x1f80 && c <= 0x1fb4)
      || (c >= 0x1fb6 && c <= 0x1fbc)
      || (c >= 0x1fc2 && c <= 0x1fc4)
      || (c >= 0x1fc6 && c <= 0x1fcc)
      || (c >= 0x1fd0 && c <= 0x1fd3)
      || (c >= 0x1fd6 && c <= 0x1fdb)
      || (c >= 0x1fe0 && c <= 0x1fec)
      || (c >= 0x1ff2 && c <= 0x1ff4)
      || (c >= 0x1ff6 && c <= 0x1ffc))
    return 1;

  /* Cyrillic */
  if ((c >= 0x0401 && c <= 0x040d)
      || (c >= 0x040f && c <= 0x044f)
      || (c >= 0x0451 && c <= 0x045c)
      || (c >= 0x045e && c <= 0x0481)
      || (c >= 0x0490 && c <= 0x04c4)
      || (c >= 0x04c7 && c <= 0x04c8)
      || (c >= 0x04cb && c <= 0x04cc)
      || (c >= 0x04d0 && c <= 0x04eb)
      || (c >= 0x04ee && c <= 0x04f5)
      || (c >= 0x04f8 && c <= 0x04f9))
    return 1;

  /* Armenian */
  if ((c >= 0x0531 && c <= 0x0556)
      || (c >= 0x0561 && c <= 0x0587))
    return 1;

  /* Hebrew */
  if ((c >= 0x05d0 && c <= 0x05ea)
      || (c >= 0x05f0 && c <= 0x05f4))
    return 1;

  /* Arabic */
  if ((c >= 0x0621 && c <= 0x063a)
      || (c >= 0x0640 && c <= 0x0652)
      || (c >= 0x0670 && c <= 0x06b7)
      || (c >= 0x06ba && c <= 0x06be)
      || (c >= 0x06c0 && c <= 0x06ce)
      || (c >= 0x06e5 && c <= 0x06e7))
    return 1;

  /* Devanagari */
  if ((c >= 0x0905 && c <= 0x0939)
      || (c >= 0x0958 && c <= 0x0962))
    return 1;

  /* Bengali */
  if ((c >= 0x0985 && c <= 0x098c)
      || (c >= 0x098f && c <= 0x0990)
      || (c >= 0x0993 && c <= 0x09a8)
      || (c >= 0x09aa && c <= 0x09b0)
      || (c == 0x09b2)
      || (c >= 0x09b6 && c <= 0x09b9)
      || (c >= 0x09dc && c <= 0x09dd)
      || (c >= 0x09df && c <= 0x09e1)
      || (c >= 0x09f0 && c <= 0x09f1))
    return 1;

  /* Gurmukhi */
  if ((c >= 0x0a05 && c <= 0x0a0a)
      || (c >= 0x0a0f && c <= 0x0a10)
      || (c >= 0x0a13 && c <= 0x0a28)
      || (c >= 0x0a2a && c <= 0x0a30)
      || (c >= 0x0a32 && c <= 0x0a33)
      || (c >= 0x0a35 && c <= 0x0a36)
      || (c >= 0x0a38 && c <= 0x0a39)
      || (c >= 0x0a59 && c <= 0x0a5c)
      || (c == 0x0a5e))
    return 1;

  /* Gujarati */
  if ((c >= 0x0a85 && c <= 0x0a8b)
      || (c == 0x0a8d)
      || (c >= 0x0a8f && c <= 0x0a91)
      || (c >= 0x0a93 && c <= 0x0aa8)
      || (c >= 0x0aaa && c <= 0x0ab0)
      || (c >= 0x0ab2 && c <= 0x0ab3)
      || (c >= 0x0ab5 && c <= 0x0ab9)
      || (c == 0x0ae0))
    return 1;

  /* Oriya */
  if ((c >= 0x0b05 && c <= 0x0b0c)
      || (c >= 0x0b0f && c <= 0x0b10)
      || (c >= 0x0b13 && c <= 0x0b28)
      || (c >= 0x0b2a && c <= 0x0b30)
      || (c >= 0x0b32 && c <= 0x0b33)
      || (c >= 0x0b36 && c <= 0x0b39)
      || (c >= 0x0b5c && c <= 0x0b5d)
      || (c >= 0x0b5f && c <= 0x0b61))
    return 1;

  /* Tamil */
  if ((c >= 0x0b85 && c <= 0x0b8a)
      || (c >= 0x0b8e && c <= 0x0b90)
      || (c >= 0x0b92 && c <= 0x0b95)
      || (c >= 0x0b99 && c <= 0x0b9a)
      || (c == 0x0b9c)
      || (c >= 0x0b9e && c <= 0x0b9f)
      || (c >= 0x0ba3 && c <= 0x0ba4)
      || (c >= 0x0ba8 && c <= 0x0baa)
      || (c >= 0x0bae && c <= 0x0bb5)
      || (c >= 0x0bb7 && c <= 0x0bb9))
    return 1;

  /* Telugu */
  if ((c >= 0x0c05 && c <= 0x0c0c)
      || (c >= 0x0c0e && c <= 0x0c10)
      || (c >= 0x0c12 && c <= 0x0c28)
      || (c >= 0x0c2a && c <= 0x0c33)
      || (c >= 0x0c35 && c <= 0x0c39)
      || (c >= 0x0c60 && c <= 0x0c61))
    return 1;

  /* Kannada */
  if ((c >= 0x0c85 && c <= 0x0c8c)
      || (c >= 0x0c8e && c <= 0x0c90)
      || (c >= 0x0c92 && c <= 0x0ca8)
      || (c >= 0x0caa && c <= 0x0cb3)
      || (c >= 0x0cb5 && c <= 0x0cb9)
      || (c >= 0x0ce0 && c <= 0x0ce1))
    return 1;

  /* Malayalam */
  if ((c >= 0x0d05 && c <= 0x0d0c)
      || (c >= 0x0d0e && c <= 0x0d10)
      || (c >= 0x0d12 && c <= 0x0d28)
      || (c >= 0x0d2a && c <= 0x0d39)
      || (c >= 0x0d60 && c <= 0x0d61))
    return 1;

  /* Thai */
  if ((c >= 0x0e01 && c <= 0x0e30)
      || (c >= 0x0e32 && c <= 0x0e33)
      || (c >= 0x0e40 && c <= 0x0e46)
      || (c >= 0x0e4f && c <= 0x0e5b))
    return 1;

  /* Lao */
  if ((c >= 0x0e81 && c <= 0x0e82)
      || (c == 0x0e84)
      || (c == 0x0e87)
      || (c == 0x0e88)
      || (c == 0x0e8a)
      || (c == 0x0e0d)
      || (c >= 0x0e94 && c <= 0x0e97)
      || (c >= 0x0e99 && c <= 0x0e9f)
      || (c >= 0x0ea1 && c <= 0x0ea3)
      || (c == 0x0ea5)
      || (c == 0x0ea7)
      || (c == 0x0eaa)
      || (c == 0x0eab)
      || (c >= 0x0ead && c <= 0x0eb0)
      || (c == 0x0eb2)
      || (c == 0x0eb3)
      || (c == 0x0ebd)
      || (c >= 0x0ec0 && c <= 0x0ec4)
      || (c == 0x0ec6))
    return 1;

  /* Georgian */
  if ((c >= 0x10a0 && c <= 0x10c5)
      || (c >= 0x10d0 && c <= 0x10f6))
    return 1;

  /* Hiragana */
  if ((c >= 0x3041 && c <= 0x3094)
      || (c >= 0x309b && c <= 0x309e))
    return 1;

  /* Katakana */
  if ((c >= 0x30a1 && c <= 0x30fe))
    return 1;

  /* Bopmofo */
  if ((c >= 0x3105 && c <= 0x312c))
    return 1;

  /* Hangul */
  if ((c >= 0x1100 && c <= 0x1159)
      || (c >= 0x1161 && c <= 0x11a2)
      || (c >= 0x11a8 && c <= 0x11f9))
    return 1;

  /* CJK Unified Ideographs */
  if ((c >= 0xf900 && c <= 0xfa2d)
      || (c >= 0xfb1f && c <= 0xfb36)
      || (c >= 0xfb38 && c <= 0xfb3c)
      || (c == 0xfb3e)
      || (c >= 0xfb40 && c <= 0xfb41)
      || (c >= 0xfb42 && c <= 0xfb44)
      || (c >= 0xfb46 && c <= 0xfbb1)
      || (c >= 0xfbd3 && c <= 0xfd3f)
      || (c >= 0xfd50 && c <= 0xfd8f)
      || (c >= 0xfd92 && c <= 0xfdc7)
      || (c >= 0xfdf0 && c <= 0xfdfb)
      || (c >= 0xfe70 && c <= 0xfe72)
      || (c == 0xfe74)
      || (c >= 0xfe76 && c <= 0xfefc)
      || (c >= 0xff21 && c <= 0xff3a)
      || (c >= 0xff41 && c <= 0xff5a)
      || (c >= 0xff66 && c <= 0xffbe)
      || (c >= 0xffc2 && c <= 0xffc7)
      || (c >= 0xffca && c <= 0xffcf)
      || (c >= 0xffd2 && c <= 0xffd7)
      || (c >= 0xffda && c <= 0xffdc)
      || (c >= 0x4e00 && c <= 0x9fa5))
    return 1;

  error ("universal-character-name `\\u%04x' not valid in identifier", c);
  return 1;
}

#if 0
/* Add the UTF-8 representation of C to the token_buffer.  */

static void
utf8_extend_token (c)
     int c;
{
  int shift, mask;

  if      (c <= 0x0000007f)
    {
      extend_token (c);
      return;
    }
  else if (c <= 0x000007ff)
    shift = 6, mask = 0xc0;
  else if (c <= 0x0000ffff)
    shift = 12, mask = 0xe0;
  else if (c <= 0x001fffff)
    shift = 18, mask = 0xf0;
  else if (c <= 0x03ffffff)
    shift = 24, mask = 0xf8;
  else
    shift = 30, mask = 0xfc;

  extend_token (mask | (c >> shift));
  do
    {
      shift -= 6;
      extend_token ((unsigned char) (0x80 | (c >> shift)));
    }
  while (shift);
}
#endif

#define ENDFILE -1  /* token that represents end-of-file */

/* Read an escape sequence, returning its equivalent as a character,
   or store 1 in *ignore_ptr if it is backslash-newline.  */

static int
readescape (ignore_ptr)
     int *ignore_ptr;
{
  register int c = getch ();
  register int code;
  register unsigned count;
  unsigned firstdig = 0;
  int nonnull;

  switch (c)
    {
    case 'x':
      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = getch ();
	  if (! ISXDIGIT (c))
	    {
	      put_back (c);
	      break;
	    }
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || (count > 1
		   && (((unsigned)1
			<< (TYPE_PRECISION (integer_type_node)
			    - (count - 1) * 4))
		       <= firstdig)))
	pedwarn ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = getch ();
	}
      put_back (c);
      return code;

    case 'U':
      return read_ucs (8);
    case 'u':
      return read_ucs (4);

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      *ignore_ptr = 1;
      return 0;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      return TARGET_BELL;

    case 'v':
      return TARGET_VT;

    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ISO-standard escape sequence, `\\%c'", c);
      return 033;

    case '?':
      return c;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      /* `\%' is used to prevent SCCS from getting confused.  */
    case '%':
      if (pedantic)
	pedwarn ("unknown escape sequence `\\%c'", c);
      return c;
    }
  if (ISGRAPH (c))
    pedwarn ("unknown escape sequence `\\%c'", c);
  else
    pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

void
yyerror (string)
     const char *string;
{
  extern int end_of_file;

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  if (end_of_file)
  {
    if (input_redirected ())
      error ("%s at end of saved text", string);
    else
      error ("%s at end of input", string);
  }
  else if (token_buffer[0] == 0)
    error ("%s at null character", string);
  else if (token_buffer[0] == '"')
    error ("%s before string constant", string);
  else if (token_buffer[0] == '\'')
    error ("%s before character constant", string);
  else if (!ISGRAPH ((unsigned char)token_buffer[0]))
    error ("%s before character 0%o", string, (unsigned char) token_buffer[0]);
  else
    error ("%s before `%s'", string, token_buffer);
}

/* Value is 1 (or 2) if we should try to make the next identifier look like
   a typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion.  */
int looking_for_typename;

inline int
identifier_type (decl)
     tree decl;
{
  tree t;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      if (TREE_CODE (DECL_TEMPLATE_RESULT (decl)) == TYPE_DECL)
	return PTYPENAME;
      else if (looking_for_template) 
	return PFUNCNAME;
    }
  if (looking_for_template && really_overloaded_fn (decl))
    {
      /* See through a baselink.  */
      if (TREE_CODE (decl) == TREE_LIST)
	decl = TREE_VALUE (decl);

      for (t = decl; t != NULL_TREE; t = OVL_CHAIN (t))
	if (DECL_FUNCTION_TEMPLATE_P (OVL_FUNCTION (t))) 
	  return PFUNCNAME;
    }
  if (TREE_CODE (decl) == NAMESPACE_DECL)
    return NSNAME;
  if (TREE_CODE (decl) != TYPE_DECL)
    return IDENTIFIER;
  if (DECL_ARTIFICIAL (decl) && TREE_TYPE (decl) == current_class_type)
    return SELFNAME;

  /* A constructor declarator for a template type will get here as an
     implicit typename, a TYPENAME_TYPE with a type.  */
  t = got_scope;
  if (t && TREE_CODE (t) == TYPENAME_TYPE)
    t = TREE_TYPE (t);
  decl = TREE_TYPE (decl);
  if (TREE_CODE (decl) == TYPENAME_TYPE)
    decl = TREE_TYPE (decl);
  if (t && t == decl)
    return SELFNAME;

  return TYPENAME;
}

void
see_typename ()
{
  /* Only types expected, not even namespaces. */
  looking_for_typename = 2;
  if (yychar < 0)
    if ((yychar = yylex ()) < 0) yychar = 0;
  looking_for_typename = 0;
  if (yychar == IDENTIFIER)
    {
      lastiddecl = lookup_name (yylval.ttype, -2);
      if (lastiddecl == 0)
	{
	  if (flag_labels_ok)
	    lastiddecl = IDENTIFIER_LABEL_VALUE (yylval.ttype);
	}
      else
	yychar = identifier_type (lastiddecl);
    }
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
  int lexing = (parsing == 1);
  int in_call = (parsing == 2);

  if (! lexing || IDENTIFIER_OPNAME_P (token))
    id = lookup_name (token, 0);
  else
    id = lastiddecl;

  /* Do Koenig lookup if appropriate (inside templates we build lookup
     expressions instead).

     [basic.lookup.koenig]: If the ordinary unqualified lookup of the name
     finds the declaration of a class member function, the associated
     namespaces and classes are not considered.  */

  if (args && !current_template_parms && (!id || is_global (id)))
    id = lookup_arg_dependent (token, id, args);

  /* Remember that this name has been used in the class definition, as per
     [class.scope0] */
  if (id && parsing)
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
      
  if (!id)
    {
      if (current_template_parms)
	return build_min_nt (LOOKUP_EXPR, token);
      else if (IDENTIFIER_OPNAME_P (token))
	{
	  if (token != ansi_opname[ERROR_MARK])
	    cp_error ("`%D' not defined", token);
	  id = error_mark_node;
	}
      else if (in_call && ! flag_strict_prototype)
	{
	  id = implicitly_declare (token);
	}
      else if (current_function_decl == 0)
	{
	  cp_error ("`%D' was not declared in this scope", token);
	  id = error_mark_node;
	}
      else
	{
	  if (IDENTIFIER_NAMESPACE_VALUE (token) != error_mark_node
	      || IDENTIFIER_ERROR_LOCUS (token) != current_function_decl)
	    {
	      static int undeclared_variable_notice;

	      cp_error ("`%D' undeclared (first use this function)", token);

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
	  /* Some local VAR_DECLs (such as those for local variables
	     in member functions of local classes) are built on the
	     permanent obstack.  */
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
  if (! id)
    {
      if (processing_template_decl)
	{
	  id = build_min_nt (LOOKUP_EXPR, token);
	  LOOKUP_EXPR_GLOBAL (id) = 1;
	  return id;
	}
      if (parsing && (yychar == '(' || yychar == LEFT_RIGHT)
	  && ! flag_strict_prototype)
	id = implicitly_declare (token);
      else
	{
	  if (IDENTIFIER_NAMESPACE_VALUE (token) != error_mark_node)
	    cp_error ("`::%D' undeclared (first use here)", token);
	  id = error_mark_node;
	  /* Prevent repeated error messages.  */
	  SET_IDENTIFIER_NAMESPACE_VALUE (token, error_mark_node);
	}
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

struct pf_args
{
  /* Input */
  int base;
  char * p;
  /* I/O */
  int c;
  /* Output */
  int imag;
  tree type;
  int conversion_errno;
  REAL_VALUE_TYPE value;
};

static void
parse_float (data)
     PTR data;
{
  struct pf_args * args = (struct pf_args *) data;
  int fflag = 0, lflag = 0;
  /* Copy token_buffer now, while it has just the number
     and not the suffixes; once we add `f' or `i',
     REAL_VALUE_ATOF may not work any more.  */
  char *copy = (char *) alloca (args->p - token_buffer + 1);
  bcopy (token_buffer, copy, args->p - token_buffer + 1);
  args->imag = 0;
  args->conversion_errno = 0;
  args->type = double_type_node;

  while (1)
    {
      int lose = 0;

      /* Read the suffixes to choose a data type.  */
      switch (args->c)
	{
	case 'f': case 'F':
	  if (fflag)
	    error ("more than one `f' in numeric constant");
	  fflag = 1;
	  break;

	case 'l': case 'L':
	  if (lflag)
	    error ("more than one `l' in numeric constant");
	  lflag = 1;
	  break;

	case 'i': case 'I':
	  if (args->imag)
	    error ("more than one `i' or `j' in numeric constant");
	  else if (pedantic)
	    pedwarn ("ISO C++ forbids imaginary numeric constants");
	  args->imag = 1;
	  break;

	default:
	  lose = 1;
	}

      if (lose)
	break;

      if (args->p >= token_buffer + maxtoken - 3)
	args->p = extend_token_buffer (args->p);
      *(args->p++) = args->c;
      *(args->p) = 0;
      args->c = getch ();
    }

  /* The second argument, machine_mode, of REAL_VALUE_ATOF
     tells the desired precision of the binary result
     of decimal-to-binary conversion.  */

  if (fflag)
    {
      if (lflag)
	error ("both `f' and `l' in floating constant");

      args->type = float_type_node;
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      /* A diagnostic is required here by some ANSI C testsuites.
	 This is not pedwarn, because some people don't want
	 an error for this.  */
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `float'");
    }
  else if (lflag)
    {
      args->type = long_double_type_node;
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `long double'");
    }
  else
    {
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `double'");
    }
}

/* Get the next character, staying within the current token if possible.
   If we're lexing a token, we don't want to look beyond the end of the
   token cpplib has prepared for us; otherwise, we end up reading in the
   next token, which screws up feed_input.  So just return a null
   character.  */

static int
token_getch ()
{
#if USE_CPPLIB
  if (yy_cur == yy_lim)
    return '\0';
#endif
  return getch ();
}

static void
token_put_back (ch)
     int ch;
{
#if USE_CPPLIB
  if (ch == '\0')
    return;
#endif
  put_back (ch);
}

/* Read a single token from the input stream, and assign it lexical
   semantics.

   Note: We used to do token pasting here, to produce compound tokens like
   LEFT_RIGHT and EXTERN_LANG_STRING.  That's now handled in spew.c, along
   with symbol table interaction and other context-sensitivity.  */

int
real_yylex ()
{
  register int c;
  register char *p;
  register int value;
  int wide_flag = 0;

  c = getch ();

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      case '\b':
#if USE_CPPLIB
	if (cpp_token == CPP_HSPACE)
	  c = yy_get_token ();
	else
#endif
	  c = getch ();
	break;

      case '\r':
	/* Call skip_white_space so we can warn if appropriate.  */

      case '\n':
      case '/':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_buffer[0] = c;
  token_buffer[1] = 0;

/*  yylloc.first_line = lineno; */

  switch (c)
    {
    case EOF:
      end_of_file = 1;
      token_buffer[0] = 0;
      if (linemode)
	value = END_OF_LINE;
      else if (input_redirected ())
	value = END_OF_SAVED_INPUT;
      else
	value = ENDFILE;
      break;

    case 'L':
#if USE_CPPLIB
      if (cpp_token == CPP_NAME)
	goto letter;
#endif
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	register int c = token_getch ();
	if (c == '\'')
	  {
	    wide_flag = 1;
	    goto char_constant;
	  }
	if (c == '"')
	  {
	    wide_flag = 1;
	    goto string_constant;
	  }
	token_put_back (c);
      }
      
    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':		  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    case '$':
    letter:
#if USE_CPPLIB
      if (cpp_token == CPP_NAME)
	{
	  /* Note that one character has already been read from
	     yy_cur into token_buffer.  Also, cpplib complains about
	     $ in identifiers, so we don't have to.  */

	  int len = yy_lim - yy_cur + 1;
	  if (len >= maxtoken)
	    extend_token_buffer_to (len + 1);
	  memcpy (token_buffer + 1, yy_cur, len);
	  p = token_buffer + len;
	  yy_cur = yy_lim;
	}
      else
#endif
	{
	  p = token_buffer;
	  while (1)
	    {
	      /* Make sure this char really belongs in an identifier.  */
	      if (ISALNUM (c) || c == '_')
		/* OK */;
	      else if (c == '$')
		{
		  if (! dollars_in_ident)
		    error ("`$' in identifier");
		  else if (pedantic)
		    pedwarn ("`$' in identifier");
		}
	      /* FIXME we should use some sort of multibyte character
		 encoding.  Locale-dependent?  Always UTF-8?  */
	      else if (is_extended_char (c))
		{
		  sorry ("universal characters in identifiers");
		  c = '_';
		}
	      else
		break;

	      if (p >= token_buffer + maxtoken)
		p = extend_token_buffer (p);

	      *p++ = c;

	    idtryagain:
	      c = token_getch ();
	      
	      if (c == '\\')
		{
		  int ignore = 0;
		  c = readescape (&ignore);
		  if (ignore)
		    goto idtryagain;
		}
	    }

	  *p = 0;
	  token_put_back (c);
	}

      value = IDENTIFIER;
      yylval.itype = 0;

      /* Try to recognize a keyword.  Uses minimum-perfect hash function */

      {
	register struct resword *ptr;

	if ((ptr = is_reserved_word (token_buffer, p - token_buffer)))
	  {
	    if (ptr->rid)
	      {
		if (ptr->token == VISSPEC)
		  {
		    switch (ptr->rid)
		      {
		      case RID_PUBLIC:
			yylval.ttype = access_public_node;
			break;
		      case RID_PRIVATE:
			yylval.ttype = access_private_node;
			break;
		      case RID_PROTECTED:
			yylval.ttype = access_protected_node;
			break;
		      default:
			my_friendly_abort (63);
		      }
		  }
		else
		  yylval.ttype = ridpointers[(int) ptr->rid];
	      }
	    else switch (ptr->token)
	      {
	      case EQCOMPARE:
		yylval.code = NE_EXPR;
		token_buffer[0] = '!';
		token_buffer[1] = '=';
		token_buffer[2] = 0;
		break;

	      case ASSIGN:
		if (strcmp ("and_eq", token_buffer) == 0)
		  {
		    yylval.code = BIT_AND_EXPR;
		    token_buffer[0] = '&';
		  }
		else if (strcmp ("or_eq", token_buffer) == 0)
		  {
		    yylval.code = BIT_IOR_EXPR;
		    token_buffer[0] = '|';
		  }
		else if (strcmp ("xor_eq", token_buffer) == 0)
		  {
		    yylval.code = BIT_XOR_EXPR;
		    token_buffer[0] = '^';
		  }
		token_buffer[1] = '=';
		token_buffer[2] = 0;
		break;

	      case '&':
		yylval.code = BIT_AND_EXPR;
		token_buffer[0] = '&';
		token_buffer[1] = 0;
		break;

	      case '|':
		yylval.code = BIT_IOR_EXPR;
		token_buffer[0] = '|';
		token_buffer[1] = 0;
		break;

	      case '^':
		yylval.code = BIT_XOR_EXPR;
		token_buffer[0] = '^';
		token_buffer[1] = 0;
		break;
	      }

	    value = (int) ptr->token;
	  }
      }

      /* If we did not find a keyword, look for an identifier
	 (or a typename).  */

      if (value == IDENTIFIER || value == TYPESPEC)
	GNU_xref_ref (current_function_decl, token_buffer);

      if (value == IDENTIFIER)
	{
	  register tree tmp = get_identifier (token_buffer);

#if !defined(VMS) && defined(JOINER)
	  /* Make sure that user does not collide with our internal
	     naming scheme.  */
	  if (JOINER == '$'
	      && (THIS_NAME_P (tmp)
		  || VPTR_NAME_P (tmp)
		  || DESTRUCTOR_NAME_P (tmp)
		  || VTABLE_NAME_P (tmp)
		  || TEMP_NAME_P (tmp)
		  || ANON_AGGRNAME_P (tmp)
		  || ANON_PARMNAME_P (tmp)))
	    warning ("identifier name `%s' conflicts with GNU C++ internal naming strategy",
		     token_buffer);
#endif

	  yylval.ttype = tmp;
	}
      if (value == NEW && ! global_bindings_p ())
	{
	  value = NEW;
	  goto done;
	}
      break;

    case '.':
#if USE_CPPLIB
      if (yy_cur < yy_lim)
#endif
	{
	  /* It's hard to preserve tokenization on '.' because
	     it could be a symbol by itself, or it could be the
	     start of a floating point number and cpp won't tell us.  */
	  register int c1 = token_getch ();
	  token_buffer[1] = c1;
	  if (c1 == '*')
	    {
	      value = DOT_STAR;
	      token_buffer[2] = 0;
	      goto done;
	    }
	  if (c1 == '.')
	    {
	      c1 = token_getch ();
	      if (c1 == '.')
		{
		  token_buffer[2] = c1;
		  token_buffer[3] = 0;
		  value = ELLIPSIS;
		  goto done;
		}
	      error ("parse error at `..'");
	    }
	  if (ISDIGIT (c1))
	    {
	      token_put_back (c1);
	      goto number;
	    }
	  token_put_back (c1);
	}
      value = '.';
      token_buffer[1] = 0;
      break;

    case '0':  case '1':
      /* Optimize for most frequent case.  */
      {
	register int cond;

#if USE_CPPLIB
	cond = (yy_cur == yy_lim);
#else
	register int c1 = token_getch ();
	token_put_back (c1);
	cond = (! ISALNUM (c1) && c1 != '.');
#endif
	if (cond)
	  {
	    yylval.ttype = (c == '0') ? integer_zero_node : integer_one_node;
	    value = CONSTANT;
	    break;
	  }
	/*FALLTHRU*/
      }
    case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    number:
      {
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	int overflow = 0;

	/* We actually store only HOST_BITS_PER_CHAR bits in each part.
	   The code below which fills the parts array assumes that a host
	   int is at least twice as wide as a host char, and that 
	   HOST_BITS_PER_WIDE_INT is an even multiple of HOST_BITS_PER_CHAR.
	   Two HOST_WIDE_INTs is the largest int literal we can store.
	   In order to detect overflow below, the number of parts (TOTAL_PARTS)
	   must be exactly the number of parts needed to hold the bits
	   of two HOST_WIDE_INTs. */
#define TOTAL_PARTS ((HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR) * 2)
	unsigned int parts[TOTAL_PARTS];

	enum anon1 { NOT_FLOAT, AFTER_POINT, TOO_MANY_POINTS, AFTER_EXPON }
	  floatflag = NOT_FLOAT;

	for (count = 0; count < TOTAL_PARTS; count++)
	  parts[count] = 0;

	p = token_buffer;
	*p++ = c;

	if (c == '0')
	  {
	    *p++ = (c = token_getch ());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = token_getch ());
	      }
	    /* Leading 0 forces octal unless the 0 is the only digit.  */
	    else if (c >= '0' && c <= '9')
	      {
		base = 8;
		numdigits++;
	      }
	    else
	      numdigits++;
	  }

	/* Read all the digits-and-decimal-points.  */

	while (c == '.'
	       || (ISALNUM (c) && c != 'l' && c != 'L'
		   && c != 'u' && c != 'U'
		   && c != 'i' && c != 'I' && c != 'j' && c != 'J'
		   && (floatflag == NOT_FLOAT
		       || ((base != 16) && (c != 'f') && (c != 'F'))
		       || base == 16)))   
	  {
	    if (c == '.')
	      {
		if (base == 16 && pedantic)
		  pedwarn ("floating constant may not be in radix 16");
		if (floatflag == TOO_MANY_POINTS)
		  /* We have already emitted an error.  Don't need another.  */
		  ;
		else if (floatflag == AFTER_POINT || floatflag == AFTER_EXPON)
		  {
		    error ("malformed floating constant");
		    floatflag = TOO_MANY_POINTS;
		    /* Avoid another error from atof by forcing all characters
		       from here on to be ignored.  */
		    p[-1] = '\0';
		  }
		else
		  floatflag = AFTER_POINT;

		if (base == 8)
		  base = 10;
		*p++ = c = token_getch ();
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.  */
		if (p == token_buffer + 2 && !ISDIGIT (c))
		  my_friendly_abort (990710);
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (ISDIGIT (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if (c == 'e' || c == 'E')
		      {
			base = 10;
			floatflag = AFTER_EXPON;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (base == 16 && (c == 'p' || c == 'P'))
		  {
		    floatflag = AFTER_EXPON;
		    break;   /* start of exponent */
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		for (count = 0; count < TOTAL_PARTS; count++)
		  {
		    parts[count] *= base;
		    if (count)
		      {
			parts[count]
			  += (parts[count-1] >> HOST_BITS_PER_CHAR);
			parts[count-1]
			  &= (1 << HOST_BITS_PER_CHAR) - 1;
		      }
		    else
		      parts[0] += c;
		  }

		/* If the highest-order part overflows (gets larger than
		   a host char will hold) then the whole number has 
		   overflowed.  Record this and truncate the highest-order
		   part. */
		if (parts[TOTAL_PARTS - 1] >> HOST_BITS_PER_CHAR)
		  {
		    overflow = 1;
		    parts[TOTAL_PARTS - 1] &= (1 << HOST_BITS_PER_CHAR) - 1;
		  }

		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = (c = token_getch ());
	      }
	  }

	/* This can happen on input like `int i = 0x;' */
	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the
           string.  */
	*--p = 0;

	if (floatflag != NOT_FLOAT)
	  {
	    tree type;
	    int imag, conversion_errno;
	    REAL_VALUE_TYPE value;
	    struct pf_args args;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((base == 10 && ((c == 'e') || (c == 'E')))
		|| (base == 16 && (c == 'p' || c == 'P')))
	      {
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = token_getch ();
		if ((c == '+') || (c == '-'))
		  {
		    *p++ = c;
		    c = token_getch ();
		  }
		/* Exponent is decimal, even if string is a hex float.  */
		if (! ISDIGIT (c))
		  error ("floating constant exponent has no digits");
		while (ISDIGIT (c))
		  {
		    if (p >= token_buffer + maxtoken - 3)
		      p = extend_token_buffer (p);
		    *p++ = c;
		    c = token_getch ();
		  }
	      }
	    if (base == 16 && floatflag != AFTER_EXPON)
	      error ("hexadecimal floating constant has no exponent");

	    *p = 0;

	    /* Setup input for parse_float() */
	    args.base = base;
	    args.p = p;
	    args.c = c;

	    /* Convert string to a double, checking for overflow.  */
	    if (do_float_handler (parse_float, (PTR) &args))
	      {
		/* Receive output from parse_float() */
		value = args.value;
	      }
	    else
	      {
		/* We got an exception from parse_float() */
		error ("floating constant out of range");
		value = dconst0;
	      }

	    /* Receive output from parse_float() */
	    c = args.c;
	    imag = args.imag;
	    type = args.type;
	    conversion_errno = args.conversion_errno;
	    
#ifdef ERANGE
	    /* ERANGE is also reported for underflow,
	       so test the value to distinguish overflow from that.  */
	    if (conversion_errno == ERANGE && pedantic
		&& (REAL_VALUES_LESS (dconst1, value)
		    || REAL_VALUES_LESS (value, dconstm1)))
	      warning ("floating point number exceeds range of `double'");
#endif

	    /* If the result is not a number, assume it must have been
	       due to some error message above, so silently convert
	       it to a zero.  */
	    if (REAL_VALUE_ISNAN (value))
	      value = dconst0;

	    /* Create a node with determined type and value.  */
	    if (imag)
	      yylval.ttype = build_complex (NULL_TREE,
					    convert (type, integer_zero_node),
					    build_real (type, value));
	    else
	      yylval.ttype = build_real (type, value);
	  }
	else
	  {
	    tree type;
	    HOST_WIDE_INT high, low;
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int spec_imag = 0;
	    int warn = 0;
	    int i;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      {
			if (spec_long_long)
			  error ("three `l's in integer constant");
			else if (pedantic && ! in_system_header && warn_long_long)
			  pedwarn ("ISO C++ forbids long long integer constants");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else if (c == 'i' || c == 'j' || c == 'I' || c == 'J')
		  {
		    if (spec_imag)
		      error ("more than one `i' or `j' in numeric constant");
		    else if (pedantic)
		      pedwarn ("ISO C++ forbids imaginary numeric constants");
		    spec_imag = 1;
		  }
		else
		  break;
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = token_getch ();
	      }

	    /* If the literal overflowed, pedwarn about it now. */
	    if (overflow)
	      {
		warn = 1;
		pedwarn ("integer constant is too large for this configuration of the compiler - truncated to %d bits", HOST_BITS_PER_WIDE_INT * 2);
	      }

	    /* This is simplified by the fact that our constant
	       is always positive.  */

	    high = low = 0;

	    for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR; i++)
	      {
		high |= ((HOST_WIDE_INT) parts[i + (HOST_BITS_PER_WIDE_INT
						    / HOST_BITS_PER_CHAR)]
			 << (i * HOST_BITS_PER_CHAR));
		low |= (HOST_WIDE_INT) parts[i] << (i * HOST_BITS_PER_CHAR);
	      }

	    yylval.ttype = build_int_2 (low, high);
	    TREE_TYPE (yylval.ttype) = long_long_unsigned_type_node;

	    /* Calculate the ANSI type.  */
	    if (! spec_long && ! spec_unsigned
		&& int_fits_type_p (yylval.ttype, integer_type_node))
	      type = integer_type_node;
	    else if (! spec_long && (base != 10 || spec_unsigned)
		     && int_fits_type_p (yylval.ttype, unsigned_type_node))
	      type = unsigned_type_node;
	    else if (! spec_unsigned && !spec_long_long
		     && int_fits_type_p (yylval.ttype, long_integer_type_node))
	      type = long_integer_type_node;
	    else if (! spec_long_long
		     && int_fits_type_p (yylval.ttype,
					 long_unsigned_type_node))
	      type = long_unsigned_type_node;
	    else if (! spec_unsigned
		     && int_fits_type_p (yylval.ttype,
					 long_long_integer_type_node))
	      type = long_long_integer_type_node;
	    else if (int_fits_type_p (yylval.ttype,
				      long_long_unsigned_type_node))
	      type = long_long_unsigned_type_node;
	    else if (! spec_unsigned
		     && int_fits_type_p (yylval.ttype,
					 widest_integer_literal_type_node))
	      type = widest_integer_literal_type_node;
	    else
	      type = widest_unsigned_literal_type_node;

	    if (pedantic && !spec_long_long && !warn
		&& (TYPE_PRECISION (long_integer_type_node)
		    < TYPE_PRECISION (type)))
	      {
		warn = 1;
		pedwarn ("integer constant larger than the maximum value of an unsigned long int");
	      }

	    if (base == 10 && ! spec_unsigned && TREE_UNSIGNED (type))
	      warning ("decimal constant is so large that it is unsigned");

	    if (spec_imag)
	      {
		if (TYPE_PRECISION (type)
		    <= TYPE_PRECISION (integer_type_node))
		  yylval.ttype
		    = build_complex (NULL_TREE, integer_zero_node,
				     convert (integer_type_node,
					      yylval.ttype));
		else
		  error ("complex integer constant is too wide for `__complex int'");
	      }
	    else
	      TREE_TYPE (yylval.ttype) = type;


	    /* If it's still an integer (not a complex), and it doesn't
	       fit in the type we choose for it, then pedwarn. */

	    if (! warn
		&& TREE_CODE (TREE_TYPE (yylval.ttype)) == INTEGER_TYPE
		&& ! int_fits_type_p (yylval.ttype, TREE_TYPE (yylval.ttype)))
	      pedwarn ("integer constant is larger than the maximum value for its type");
	  }

	token_put_back (c);
	*p = 0;

	if (ISALNUM (c) || c == '.' || c == '_' || c == '$'
	    || ((c == '-' || c == '+')
		&& (p[-1] == 'e' || p[-1] == 'E')))
	  error ("missing white space after number `%s'", token_buffer);

	value = CONSTANT; break;
      }

    case '\'':
    char_constant:
      {
	register int result = 0;
	register int num_chars = 0;
	int chars_seen = 0;
	unsigned width = TYPE_PRECISION (char_type_node);
	int max_chars;
#ifdef MULTIBYTE_CHARS
	int longest_char = local_mb_cur_max ();
	local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

	max_chars = TYPE_PRECISION (integer_type_node) / width;
	if (wide_flag)
	  width = WCHAR_TYPE_SIZE;

	while (1)
	  {
	  tryagain:
	    c = token_getch ();

	    if (c == '\'' || c == EOF)
	      break;

	    ++chars_seen;
	    if (c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto tryagain;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= ((unsigned)1 << width))
		  pedwarn ("escape sequence out of range for character");
#ifdef MAP_CHARACTER
		if (ISPRINT (c))
		  c = MAP_CHARACTER (c);
#endif
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ISO C++ forbids newline in character constant");
		lineno++;
	      }
	    else
	      {
#ifdef MULTIBYTE_CHARS
		wchar_t wc;
		int i;
		int char_len = -1;
		for (i = 1; i <= longest_char; ++i)
		  {
		    if (i > maxtoken - 4)
		      extend_token_buffer (token_buffer);

		    token_buffer[i] = c;
		    char_len = local_mbtowc (& wc,
					     token_buffer + 1,
					     i);
		    if (char_len != -1)
		      break;
		    c = token_getch ();
		  }
		if (char_len > 1)
		  {
		    /* mbtowc sometimes needs an extra char before accepting */
		    if (char_len < i)
		      token_put_back (c);
		    if (! wide_flag)
		      {
			/* Merge character into result; ignore excess chars.  */
			for (i = 1; i <= char_len; ++i)
			  {
			    if (i > max_chars)
			      break;
			    if (width < HOST_BITS_PER_INT)
			      result = (result << width)
				| (token_buffer[i]
				   & ((1 << width) - 1));
			    else
			      result = token_buffer[i];
			  }
			num_chars += char_len;
			goto tryagain;
		      }
		    c = wc;
		  }
		else
		  {
		    if (char_len == -1)
		      {
			warning ("Ignoring invalid multibyte character");
			/* Replace all but the first byte.  */
			for (--i; i > 1; --i)
			  token_put_back (token_buffer[i]);
			wc = token_buffer[1];
		      }
#ifdef MAP_CHARACTER
		      c = MAP_CHARACTER (wc);
#else
		      c = wc;
#endif
		  }
#else /* ! MULTIBYTE_CHARS */
#ifdef MAP_CHARACTER
		c = MAP_CHARACTER (c);
#endif
#endif /* ! MULTIBYTE_CHARS */
	      }

	    if (wide_flag)
	      {
		if (chars_seen == 1) /* only keep the first one */
		  result = c;
		goto tryagain;
	      }

	    /* Merge character into result; ignore excess chars.  */
	    num_chars += (width / TYPE_PRECISION (char_type_node));
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	if (c != '\'')
	  error ("malformatted character constant");
	else if (chars_seen == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }
	else if (chars_seen != 1 && warn_multichar)
	  warning ("multi-character character constant");

	/* If char type is signed, sign-extend the constant.  */
	if (! wide_flag)
	  {
	    int num_bits = num_chars * width;
	    if (num_bits == 0)
	      /* We already got an error; avoid invalid shift.  */
	      yylval.ttype = build_int_2 (0, 0);
	    else if (TREE_UNSIGNED (char_type_node)
		     || ((result >> (num_bits - 1)) & 1) == 0)
	      yylval.ttype
		= build_int_2 (result & (~(unsigned HOST_WIDE_INT) 0
					 >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       0);
	    else
	      yylval.ttype
		= build_int_2 (result | ~(~(unsigned HOST_WIDE_INT) 0
					  >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       -1);
	    /* In C, a character constant has type 'int'; in C++, 'char'.  */
	    if (chars_seen <= 1)
	      TREE_TYPE (yylval.ttype) = char_type_node;
	    else
	      TREE_TYPE (yylval.ttype) = integer_type_node;
	  }
	else
	  {
	    yylval.ttype = build_int_2 (result, 0);
	    TREE_TYPE (yylval.ttype) = wchar_type_node;
	  }

	value = CONSTANT;
	break;
      }

    case '"':
    string_constant:
      {
	unsigned width = wide_flag ? WCHAR_TYPE_SIZE
	                           : TYPE_PRECISION (char_type_node);
#ifdef MULTIBYTE_CHARS
	int longest_char = local_mb_cur_max ();
	local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

	c = token_getch ();
	p = token_buffer + 1;

	while (c != '"' && c != EOF)
	  {
	    /* ignore_escape_flag is set for reading the filename in #line.  */
	    if (!ignore_escape_flag && c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto skipnewline;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= ((unsigned)1 << width))
		  pedwarn ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ISO C++ forbids newline in string constant");
		lineno++;
	      }
	    else
	      {
#ifdef MULTIBYTE_CHARS
		wchar_t wc;
		int i;
		int char_len = -1;
		for (i = 0; i < longest_char; ++i)
		  {
		    if (p + i >= token_buffer + maxtoken)
		      p = extend_token_buffer (p);
		    p[i] = c;

		    char_len = local_mbtowc (& wc, p, i + 1);
		    if (char_len != -1)
		      break;
		    c = token_getch ();
		  }
		if (char_len == -1)
		  {
		    warning ("Ignoring invalid multibyte character");
		    /* Replace all except the first byte.  */
		    token_put_back (c);
		    for (--i; i > 0; --i)
		      token_put_back (p[i]);
		    char_len = 1;
		  }
		/* mbtowc sometimes needs an extra char before accepting */
		if (char_len <= i)
		  token_put_back (c);
		if (! wide_flag)
		  {
		    p += (i + 1);
		    c = token_getch ();
		    continue;
		  }
		c = wc;
#endif /* MULTIBYTE_CHARS */
	      }

	    /* Add this single character into the buffer either as a wchar_t
	       or as a single byte.  */
	    if (wide_flag)
	      {
		unsigned width = TYPE_PRECISION (char_type_node);
		unsigned bytemask = (1 << width) - 1;
		int byte;

		if (p + WCHAR_BYTES > token_buffer + maxtoken)
		  p = extend_token_buffer (p);

		for (byte = 0; byte < WCHAR_BYTES; ++byte)
		  {
		    int value;
		    if (byte >= (int) sizeof (c))
		      value = 0;
		    else
		      value = (c >> (byte * width)) & bytemask;
		    if (BYTES_BIG_ENDIAN)
		      p[WCHAR_BYTES - byte - 1] = value;
		    else
		      p[byte] = value;
		  }
		p += WCHAR_BYTES;
	      }
	    else
	      {
		if (p >= token_buffer + maxtoken)
		  p = extend_token_buffer (p);
		*p++ = c;
	      }

	  skipnewline:
	    c = token_getch ();
	  }

	/* Terminate the string value, either with a single byte zero
	   or with a wide zero.  */
	if (wide_flag)
	  {
	    if (p + WCHAR_BYTES > token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    bzero (p, WCHAR_BYTES);
	    p += WCHAR_BYTES;
	  }
	else
	  {
	    if (p >= token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    *p++ = 0;
	  }

	if (c == EOF)
	  error ("Unterminated string constant");

	/* We have read the entire constant.
	   Construct a STRING_CST for the result.  */

	yylval.ttype = build_string (p - (token_buffer + 1), token_buffer + 1);

	if (wide_flag)
	  TREE_TYPE (yylval.ttype) = wchar_array_type_node;
	else
	  TREE_TYPE (yylval.ttype) = char_array_type_node;

	value = STRING; break;
      }

    case '+':
    case '-':
    case '&':
    case '|':
    case ':':
    case '<':
    case '>':
    case '*':
    case '/':
    case '%':
    case '^':
    case '!':
    case '=':
      {
	register int c1;

      combine:

	switch (c)
	  {
	  case '+':
	    yylval.code = PLUS_EXPR; break;
	  case '-':
	    yylval.code = MINUS_EXPR; break;
	  case '&':
	    yylval.code = BIT_AND_EXPR; break;
	  case '|':
	    yylval.code = BIT_IOR_EXPR; break;
	  case '*':
	    yylval.code = MULT_EXPR; break;
	  case '/':
	    yylval.code = TRUNC_DIV_EXPR; break;
	  case '%':
	    yylval.code = TRUNC_MOD_EXPR; break;
	  case '^':
	    yylval.code = BIT_XOR_EXPR; break;
	  case LSHIFT:
	    yylval.code = LSHIFT_EXPR; break;
	  case RSHIFT:
	    yylval.code = RSHIFT_EXPR; break;
	  case '<':
	    yylval.code = LT_EXPR; break;
	  case '>':
	    yylval.code = GT_EXPR; break;
	  }

	token_buffer[1] = c1 = token_getch ();
	token_buffer[2] = 0;

	if (c1 == '=')
	  {
	    switch (c)
	      {
	      case '<':
		value = ARITHCOMPARE; yylval.code = LE_EXPR; goto done;
	      case '>':
		value = ARITHCOMPARE; yylval.code = GE_EXPR; goto done;
	      case '!':
		value = EQCOMPARE; yylval.code = NE_EXPR; goto done;
	      case '=':
		value = EQCOMPARE; yylval.code = EQ_EXPR; goto done;
	      }
	    value = ASSIGN; goto done;
	  }
	else if (c == c1)
	  switch (c)
	    {
	    case '+':
	      value = PLUSPLUS; goto done;
	    case '-':
	      value = MINUSMINUS; goto done;
	    case '&':
	      value = ANDAND; goto done;
	    case '|':
	      value = OROR; goto done;
	    case '<':
	      c = LSHIFT;
	      goto combine;
	    case '>':
	      c = RSHIFT;
	      goto combine;
	    case ':':
	      value = SCOPE;
	      yylval.itype = 1;
	      goto done;
	    }
	else if (c1 == '?' && (c == '<' || c == '>'))
	  {
	    token_buffer[3] = 0;

	    c1 = token_getch ();
	    yylval.code = (c == '<' ? MIN_EXPR : MAX_EXPR);
	    if (c1 == '=')
	      {
		/* <?= or >?= expression.  */
		token_buffer[2] = c1;
		value = ASSIGN;
	      }
	    else
	      {
		value = MIN_MAX;
		token_put_back (c1);
	      }
	    if (pedantic)
	      pedwarn ("use of `operator %s' is not standard C++",
		       token_buffer);
	    goto done;
	  }
	else
	  switch (c)
	    {
	    case '-':
	      if (c1 == '>')
		{
		  c1 = token_getch ();
		  if (c1 == '*')
		    value = POINTSAT_STAR;
		  else
		    {
		      token_put_back (c1);
		      value = POINTSAT;
		    }
		  goto done;
		}
	      break;

	      /* digraphs */
	    case ':':
	      if (c1 == '>')
		{ value = ']'; goto done; }
	      break;
	    case '<':
	      if (c1 == '%')
		{ value = '{'; indent_level++; goto done; }
	      if (c1 == ':')
		{ value = '['; goto done; }
	      break;
	    case '%':
	      if (c1 == '>')
		{ value = '}'; indent_level--; goto done; }
	      break;
	    }

	token_put_back (c1);
	token_buffer[1] = 0;

	/* Here the C frontend changes < and > to ARITHCOMPARE.  We don't
	   do that because of templates.  */

	value = c;
	break;
      }

    case 0:
      /* Don't make yyparse think this is eof.  */
      value = 1;
      break;

    case '{':
      indent_level++;
      value = c;
      break;

    case '}':
      indent_level--;
      value = c;
      break;

    default:
      if (is_extended_char (c))
	goto letter;
      value = c;
    }

done:
/*  yylloc.last_line = lineno; */
#ifdef GATHER_STATISTICS
#ifdef REDUCE_LENGTH
  token_count[value] += 1;
#endif
#endif

  return value;
}

int
is_rid (t)
     tree t;
{
  return !!is_reserved_word (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));
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

  ld = (struct lang_decl *) ggc_alloc_obj (size, 1);

  DECL_LANG_SPECIFIC (t) = ld;
  if (current_lang_name == lang_name_cplusplus)
    DECL_LANGUAGE (t) = lang_cplusplus;
  else if (current_lang_name == lang_name_c)
    DECL_LANGUAGE (t) = lang_c;
  else if (current_lang_name == lang_name_java)
    DECL_LANGUAGE (t) = lang_java;
  else my_friendly_abort (64);

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
  bcopy ((char *)DECL_LANG_SPECIFIC (node), (char *)ld, size);
  DECL_LANG_SPECIFIC (node) = ld;
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

tree
cp_make_lang_type (code)
     enum tree_code code;
{
  register tree t = make_node (code);

  /* Set up some flags that give proper default behavior.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      struct lang_type *pi;

      pi = (struct lang_type *) ggc_alloc (sizeof (struct lang_type));
      bzero ((char *) pi, (int) sizeof (struct lang_type));

      TYPE_LANG_SPECIFIC (t) = pi;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;

      /* Make sure this is laid out, for ease of use later.  In the
	 presence of parse errors, the normal was of assuring this
	 might not ever get executed, so we lay it out *immediately*.  */
      build_pointer_type (t);

#ifdef GATHER_STATISTICS
      tree_node_counts[(int)lang_type] += 1;
      tree_node_sizes[(int)lang_type] += sizeof (struct lang_type);
#endif
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
  if (IS_AGGR_TYPE_CODE (code) || code == TEMPLATE_TYPE_PARM)
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
dump_time_statistics ()
{
  register tree prev = 0, decl, next;
  int this_time = get_run_time ();
  TREE_INT_CST_LOW (TIME_IDENTIFIER_TIME (this_filename_time))
    += this_time - body_time;

  fprintf (stderr, "\n******\n");
  print_time ("header files (total)", header_time);
  print_time ("main file (total)", this_time - body_time);
  fprintf (stderr, "ratio = %g : 1\n",
	   (double)header_time / (double)(this_time - body_time));
  fprintf (stderr, "\n******\n");

  for (decl = filename_times; decl; decl = next)
    {
      next = IDENTIFIER_GLOBAL_VALUE (decl);
      SET_IDENTIFIER_GLOBAL_VALUE (decl, prev);
      prev = decl;
    }

  for (decl = prev; decl; decl = IDENTIFIER_GLOBAL_VALUE (decl))
    print_time (IDENTIFIER_POINTER (decl),
		TREE_INT_CST_LOW (TIME_IDENTIFIER_TIME (decl)));
}

void
compiler_error VPARAMS ((const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msg;
#endif
  char buf[1024];
  va_list ap;
  
  VA_START (ap, msg);
  
#ifndef ANSI_PROTOTYPES
  msg = va_arg (ap, const char *);
#endif

  vsprintf (buf, msg, ap);
  va_end (ap);
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

  my_friendly_abort (0);
  return TYPE_UNQUALIFIED;
}
