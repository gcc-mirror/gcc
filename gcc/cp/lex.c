/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
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

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include "config.h"
#include "input.h"
#include "tree.h"
#include "lex.h"
#include "parse.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"
#include "c-pragma.h"

#ifdef MULTIBYTE_CHARS
#include <stdlib.h>
#include <locale.h>
#endif

#ifndef errno
extern int errno;		/* needed for VAX.  */
#endif
extern jmp_buf toplevel;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern struct obstack *expression_obstack, permanent_obstack;
extern struct obstack *current_obstack, *saveable_obstack;

extern double atof ();

extern char *get_directive_line ();	/* In c-common.c */

/* Given a file name X, return the nondirectory portion.
   Keep in mind that X can be computed more than once.  */
#ifndef FILE_NAME_NONDIRECTORY
#define FILE_NAME_NONDIRECTORY(X)		\
 (rindex (X, '/') != 0 ? rindex (X, '/') + 1 : X)
#endif

extern char *index ();
extern char *rindex ();

void extract_interface_info ();
void yyerror ();

/* This obstack is needed to hold text.  It is not safe to use
   TOKEN_BUFFER because `check_newline' calls `yylex'.  */
struct obstack inline_text_obstack;
static char *inline_text_firstobj;

/* This obstack is used to hold information about methods to be
   synthesized.  It should go away when synthesized methods are handled
   properly (i.e. only when needed).  */
struct obstack synth_obstack;
static char *synth_firstobj;

int end_of_file;

/* Pending language change.
   Positive is push count, negative is pop count.  */
int pending_lang_change = 0;

/* Wrap the current header file in extern "C".  */
static int c_header_level = 0;

extern int first_token;
extern struct obstack token_obstack;

/* ??? Don't really know where this goes yet.  */
#if 1
#include "input.c"
#else
extern void put_back (/* int */);
extern int input_redirected ();
extern void feed_input (/* char *, int, struct obstack * */);
#endif

/* Holds translations from TREE_CODEs to operator name strings,
   i.e., opname_tab[PLUS_EXPR] == "+".  */
char **opname_tab;
char **assignop_tab;

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
static tree get_time_identifier ();
static tree filename_times;
static tree this_filename_time;

/* For implementing #pragma unit.  */
tree current_unit_name;
tree current_unit_language;

/* Array for holding counts of the numbers of tokens seen.  */
extern int *token_count;

/* Textual definition used for default functions.  */
static void default_copy_constructor_body ();
static void default_assign_ref_body ();

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers.

   We return an INDIRECT_REF whose "contents" are TARGET
   and whose type is the modifier list.  */

tree
make_pointer_declarator (type_quals, target)
     tree type_quals, target;
{
  if (target && TREE_CODE (target) == IDENTIFIER_NODE
      && ANON_AGGRNAME_P (target))
    error ("type name expected before `*'");
  target = build_parse_node (INDIRECT_REF, target);
  TREE_TYPE (target) = type_quals;
  return target;
}

/* Return something to represent absolute declarators containing a &.
   TARGET is the absolute declarator that the & contains.
   TYPE_QUALS is a list of modifiers such as const or volatile
   to apply to the reference type, represented as identifiers.

   We return an ADDR_EXPR whose "contents" are TARGET
   and whose type is the modifier list.  */
   
tree
make_reference_declarator (type_quals, target)
     tree type_quals, target;
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
  TREE_TYPE (target) = type_quals;
  return target;
}

/* Build names and nodes for overloaded operators.  */

tree ansi_opname[LAST_CPLUS_TREE_CODE];
tree ansi_assopname[LAST_CPLUS_TREE_CODE];

char *
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

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

#ifndef WCHAR_TYPE_SIZE
#ifdef INT_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#else
#define WCHAR_TYPE_SIZE	BITS_PER_WORD
#endif
#endif

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

static int maxtoken;		/* Current nominal length of token buffer.  */
char *token_buffer;		/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2.  */

#include "hash.h"

int check_newline ();

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag = 0;

static int skip_white_space ();

static tree
get_time_identifier (name)
     char *name;
{
  tree time_identifier;
  int len = strlen (name);
  char *buf = (char *) alloca (len + 6);
  strcpy (buf, "file ");
  bcopy (name, buf+5, len);
  buf[len+5] = '\0';
  time_identifier = get_identifier (buf);
  if (IDENTIFIER_LOCAL_VALUE (time_identifier) == NULL_TREE)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      IDENTIFIER_LOCAL_VALUE (time_identifier) = build_int_2 (0, 0);
      IDENTIFIER_CLASS_VALUE (time_identifier) = build_int_2 (0, 1);
      IDENTIFIER_GLOBAL_VALUE (time_identifier) = filename_times;
      filename_times = time_identifier;
      pop_obstacks ();
    }
  return time_identifier;
}

#ifdef __GNUC__
__inline
#endif
static int
my_get_run_time ()
{
  int old_quiet_flag = quiet_flag;
  int this_time;
  quiet_flag = 0;
  this_time = get_run_time ();
  quiet_flag = old_quiet_flag;
  return this_time;
}

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp/tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *cplus_tree_code_type[] = {
  "x",
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int cplus_tree_code_length[] = {
  0,
#include "tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *cplus_tree_code_name[] = {
  "@@dummy",
#include "tree.def"
};
#undef DEFTREECODE

/* toplev.c needs to call these.  */

void
lang_init ()
{
  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  put_back (check_newline ());

  if (flag_cadillac)
    cadillac_start ();
  if (flag_gnu_xref) GNU_xref_begin (input_filename);
  init_repo (input_filename);
}

void
lang_finish ()
{
  extern int errorcount, sorrycount;
  if (flag_gnu_xref) GNU_xref_end (errorcount+sorrycount);
}

char *
lang_identify ()
{
  return "cplusplus";
}

void
init_filename_times ()
{
  this_filename_time = get_time_identifier ("<top level>");
  if (flag_detailed_statistics)
    {
      header_time = 0;
      body_time = my_get_run_time ();
      TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time)) = body_time;
    }
}

/* Change by Bryan Boreham, Kewill, Thu Jul 27 09:46:05 1989.
   Stuck this hack in to get the files open correctly; this is called
   in place of init_lex if we are an unexec'd binary.    */
void
reinit_lang_specific ()
{
  init_filename_times ();
  reinit_search_statistics ();
}

void
init_lex ()
{
  extern char *(*decl_printable_name) ();
  extern int flag_no_gnu_keywords;
  extern int flag_operator_names;

  int i;

  /* Initialize the lookahead machinery.  */
  init_spew ();

  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));
  decl_printable_name = lang_printable_name;

  init_cplus_expand ();

  tree_code_type
    = (char **) realloc (tree_code_type,
			 sizeof (char *) * LAST_CPLUS_TREE_CODE);
  tree_code_length
    = (int *) realloc (tree_code_length,
		       sizeof (int) * LAST_CPLUS_TREE_CODE);
  tree_code_name
    = (char **) realloc (tree_code_name,
			 sizeof (char *) * LAST_CPLUS_TREE_CODE);
  bcopy ((char *)cplus_tree_code_type,
	 (char *)(tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE),
	 (LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));
  bcopy ((char *)cplus_tree_code_length,
	 (char *)(tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE),
	 (LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (int));
  bcopy ((char *)cplus_tree_code_name,
	 (char *)(tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE),
	 (LAST_CPLUS_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));

  opname_tab = (char **)oballoc ((int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  bzero ((char *)opname_tab, (int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  assignop_tab = (char **)oballoc ((int)LAST_CPLUS_TREE_CODE * sizeof (char *));
  bzero ((char *)assignop_tab, (int)LAST_CPLUS_TREE_CODE * sizeof (char *));

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
  ansi_opname[(int) TYPE_EXPR] = get_identifier ("__op");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) TYPE_EXPR]) = 1;

  /* This is not true: these operators are not defined in ANSI,
     but we need them anyway.  */
  ansi_opname[(int) MIN_EXPR] = get_identifier ("__mn");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MIN_EXPR]) = 1;
  ansi_opname[(int) MAX_EXPR] = get_identifier ("__mx");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) MAX_EXPR]) = 1;
  ansi_opname[(int) COND_EXPR] = get_identifier ("__cn");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) COND_EXPR]) = 1;
  ansi_opname[(int) METHOD_CALL_EXPR] = get_identifier ("__wr");
  IDENTIFIER_OPNAME_P (ansi_opname[(int) METHOD_CALL_EXPR]) = 1;

  init_method ();
  init_error ();
  gcc_obstack_init (&inline_text_obstack);
  inline_text_firstobj = (char *) obstack_alloc (&inline_text_obstack, 0);
  gcc_obstack_init (&synth_obstack);
  synth_firstobj = (char *) obstack_alloc (&synth_obstack, 0);

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;
  input_filename = "<internal>";
  current_function_decl = NULL;

  maxtoken = 40;
  token_buffer = (char *) xmalloc (maxtoken + 2);

  ridpointers[(int) RID_INT] = get_identifier ("int");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_INT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]));
  ridpointers[(int) RID_BOOL] = get_identifier ("bool");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_BOOL],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_BOOL]));
  ridpointers[(int) RID_CHAR] = get_identifier ("char");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_CHAR],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]));
  ridpointers[(int) RID_VOID] = get_identifier ("void");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VOID],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VOID]));
  ridpointers[(int) RID_FLOAT] = get_identifier ("float");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_FLOAT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_FLOAT]));
  ridpointers[(int) RID_DOUBLE] = get_identifier ("double");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_DOUBLE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_DOUBLE]));
  ridpointers[(int) RID_SHORT] = get_identifier ("short");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_SHORT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_SHORT]));
  ridpointers[(int) RID_LONG] = get_identifier ("long");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_LONG],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]));
  ridpointers[(int) RID_UNSIGNED] = get_identifier ("unsigned");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_UNSIGNED],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_UNSIGNED]));
  ridpointers[(int) RID_SIGNED] = get_identifier ("signed");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_SIGNED],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_SIGNED]));
  ridpointers[(int) RID_INLINE] = get_identifier ("inline");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_INLINE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_INLINE]));
  ridpointers[(int) RID_CONST] = get_identifier ("const");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_CONST],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_CONST]));
  ridpointers[(int) RID_VOLATILE] = get_identifier ("volatile");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VOLATILE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VOLATILE]));
  ridpointers[(int) RID_AUTO] = get_identifier ("auto");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_AUTO],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_AUTO]));
  ridpointers[(int) RID_STATIC] = get_identifier ("static");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_STATIC],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]));
  ridpointers[(int) RID_EXTERN] = get_identifier ("extern");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_EXTERN],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_EXTERN]));
  ridpointers[(int) RID_TYPEDEF] = get_identifier ("typedef");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_TYPEDEF],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_TYPEDEF]));
  ridpointers[(int) RID_REGISTER] = get_identifier ("register");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_REGISTER],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_REGISTER]));

  /* C++ extensions. These are probably not correctly named. */
  ridpointers[(int) RID_WCHAR] = get_identifier ("__wchar_t");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_WCHAR],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_WCHAR]));
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
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VIRTUAL],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VIRTUAL]));
  ridpointers[(int) RID_EXPLICIT] = get_identifier ("explicit");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_EXPLICIT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_EXPLICIT]));
  ridpointers[(int) RID_FRIEND] = get_identifier ("friend");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_FRIEND],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_FRIEND]));

  ridpointers[(int) RID_PUBLIC] = get_identifier ("public");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_PUBLIC],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_PUBLIC]));
  ridpointers[(int) RID_PRIVATE] = get_identifier ("private");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_PRIVATE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_PRIVATE]));
  ridpointers[(int) RID_PROTECTED] = get_identifier ("protected");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_PROTECTED],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_PROTECTED]));
  ridpointers[(int) RID_TEMPLATE] = get_identifier ("template");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_TEMPLATE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_TEMPLATE]));
  /* This is for ANSI C++. */
  ridpointers[(int) RID_MUTABLE] = get_identifier ("mutable");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_MUTABLE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_MUTABLE]));

  /* Signature handling extensions.  */
  signature_type_node = build_int_2 (signature_type, 0);
  TREE_TYPE (signature_type_node) = signature_type_node;
  ridpointers[(int) RID_SIGNATURE] = signature_type_node;

  opname_tab[(int) COMPONENT_REF] = "->";
  opname_tab[(int) MEMBER_REF] = "->*";
  opname_tab[(int) METHOD_CALL_EXPR] = "->()";
  opname_tab[(int) INDIRECT_REF] = "(unary *)";
  opname_tab[(int) ARRAY_REF] = "[]";
  opname_tab[(int) MODIFY_EXPR] = "=";
  opname_tab[(int) NEW_EXPR] = "new";
  opname_tab[(int) DELETE_EXPR] = "delete";
  opname_tab[(int) VEC_NEW_EXPR] = "new []";
  opname_tab[(int) VEC_DELETE_EXPR] = "delete []";
  opname_tab[(int) COND_EXPR] = "... ? ... : ...";
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
  opname_tab[(int) RANGE_EXPR] = "..";
  opname_tab[(int) CONVERT_EXPR] = "(unary +)";
  opname_tab[(int) ADDR_EXPR] = "(unary &)";
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
  if (!flag_handle_exceptions)
    {
      UNSET_RESERVED_WORD ("throw");
      UNSET_RESERVED_WORD ("try");
      UNSET_RESERVED_WORD ("catch");
    }
#endif

  if (! (flag_gc || flag_rtti) || flag_no_gnu_keywords)
    {
      UNSET_RESERVED_WORD ("classof");
      UNSET_RESERVED_WORD ("headof");
    }
  if (! flag_handle_signatures || flag_no_gnu_keywords)
    {
      /* Easiest way to not recognize signature
	 handling extensions...  */
      UNSET_RESERVED_WORD ("signature");
      UNSET_RESERVED_WORD ("sigof");
    }
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
  if (! flag_traditional)
    UNSET_RESERVED_WORD ("overload");

  token_count = init_parse ();
  interface_unknown = 1;
}

void
reinit_parse_for_function ()
{
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;
}

#ifdef __GNUC__
__inline
#endif
void
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
    case IDENTIFIER_DEFN:
    case TYPENAME_DEFN:
    case PTYPENAME_DEFN:
    case TYPENAME_ELLIPSIS:
    case SCSPEC:
    case PRE_PARSED_CLASS_DECL:
      t = yylval.ttype;
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
      else if (yylval.ttype == signature_type_node)
	fprintf (file, " `signature'");
      else
	my_friendly_abort (80);
      break;
    }
}

static int *reduce_count;
int *token_count;

#define REDUCE_LENGTH (sizeof (yyr2) / sizeof (yyr2[0]))
#define TOKEN_LENGTH (256 + sizeof (yytname) / sizeof (yytname[0]))

int *
init_parse ()
{
#ifdef GATHER_STATISTICS
  reduce_count = (int *)malloc (sizeof (int) * (REDUCE_LENGTH + 1));
  bzero (reduce_count, sizeof (int) * (REDUCE_LENGTH + 1));
  reduce_count += 1;
  token_count = (int *)malloc (sizeof (int) * (TOKEN_LENGTH + 1));
  bzero (token_count, sizeof (int) * (TOKEN_LENGTH + 1));
  token_count += 1;
#endif
  return token_count;
}

#ifdef GATHER_STATISTICS
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

void
print_parse_statistics ()
{
#ifdef GATHER_STATISTICS
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
      int index = sorted[i];
      if (token_count[index] == 0)
	break;
      if (token_count[index] < token_count[-1])
	break;
      fprintf (stderr, "token %d, `%s', count = %d\n",
	       index, yytname[YYTRANSLATE (index)], token_count[index]);
    }
  fprintf (stderr, "\n");
  for (i = 0; i < REDUCE_LENGTH; i++)
    sorted[i] = i;
  qsort (sorted, REDUCE_LENGTH, sizeof (int), reduce_cmp);
  for (i = 0; i < REDUCE_LENGTH; i++)
    {
      int index = sorted[i];
      if (reduce_count[index] == 0)
	break;
      if (reduce_count[index] < reduce_count[-1])
	break;
      fprintf (stderr, "rule %d, line %d, count = %d\n",
	       index, yyrline[index], reduce_count[index]);
    }
  fprintf (stderr, "\n");
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
  fileinfo = IDENTIFIER_CLASS_VALUE (fileinfo);
  interface_only = TREE_INT_CST_LOW (fileinfo);
  if (!processing_template_defn || flag_external_templates)
    interface_unknown = TREE_INT_CST_HIGH (fileinfo);
}

/* Return nonzero if S is not considered part of an
   INTERFACE/IMPLEMENTATION pair.  Otherwise, return 0.  */
static int
interface_strcmp (s)
     char *s;
{
  /* Set the interface/implementation bits for this scope.  */
  struct impl_files *ifiles;
  char *s1;

  for (ifiles = impl_file_chain; ifiles; ifiles = ifiles->next)
    {
      char *t1 = ifiles->filename;
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

void
set_typedecl_interface_info (prev, vars)
     tree prev, vars;
{
  tree id = get_time_identifier (DECL_SOURCE_FILE (vars));
  tree fileinfo = IDENTIFIER_CLASS_VALUE (id);
  tree type = TREE_TYPE (vars);

  CLASSTYPE_INTERFACE_ONLY (type) = TREE_INT_CST_LOW (fileinfo)
    = interface_strcmp (FILE_NAME_NONDIRECTORY (DECL_SOURCE_FILE (vars)));
}

void
set_vardecl_interface_info (prev, vars)
     tree prev, vars;
{
  tree type = DECL_CONTEXT (vars);

  if (CLASSTYPE_INTERFACE_KNOWN (type))
    {
      if (CLASSTYPE_INTERFACE_ONLY (type))
	set_typedecl_interface_info (prev, TYPE_NAME (type));
      else
	CLASSTYPE_VTABLE_NEEDS_WRITING (type) = 1;
      DECL_EXTERNAL (vars) = CLASSTYPE_INTERFACE_ONLY (type);
      TREE_PUBLIC (vars) = 1;
    }
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
    struct pending_inline *prev = 0, *tail, *bottom = 0;
    t = pending_inlines;
    pending_inlines = 0;

    for (; t; t = tail)
      {
	tail = t->next;
	t->next = prev;
	t->deja_vu = 1;
	prev = t;
      }

    /* This kludge should go away when synthesized methods are handled
       properly, i.e. only when needed.  */
    for (t = prev; t; t = t->next)
      {
	if (t->lineno <= 0)
	  {
	    tree f = t->fndecl;
	    DECL_PENDING_INLINE_INFO (f) = 0;
	    interface_unknown = t->interface == 1;
	    interface_only = t->interface == 0;
	    synthesize_method (f);
	    if (tail)
	      tail->next = t->next;
	    else
	      prev = t->next;
	    if (! bottom)
	      bottom = t;
	  }
	else
	  tail = t;
      }
    if (bottom)
      {
	obstack_free (&synth_obstack, bottom);
	extract_interface_info ();
      }
    t = prev;
  }

  if (t == 0)
    return;
	    
  /* Now start processing the first inline function.  */
  my_friendly_assert ((t->parm_vec == NULL_TREE) == (t->bindings == NULL_TREE),
		      226);
  if (t->parm_vec)
    push_template_decls (t->parm_vec, t->bindings, 0);
  if (t->len > 0)
    {
      feed_input (t->buf, t->len, t->can_free ? &inline_text_obstack : 0);
      lineno = t->lineno;
#if 0
      if (input_filename != t->filename)
	{
	  input_filename = t->filename;
	  /* Get interface/implementation back in sync.  */
	  extract_interface_info ();
	}
#else
      input_filename = t->filename;
      interface_unknown = t->interface == 1;
      interface_only = t->interface == 0;
#endif
      yychar = PRE_PARSED_FUNCTION_DECL;
    }
  /* Pass back a handle on the rest of the inline functions, so that they
     can be processed later.  */
  yylval.ttype = build_tree_list ((tree) t, t->fndecl);
#if 0
  if (flag_default_inline && t->fndecl
      /* If we're working from a template, don't change
	 the `inline' state.  */
      && t->parm_vec == NULL_TREE)
    DECL_INLINE (t->fndecl) = 1;
#endif
  DECL_PENDING_INLINE_INFO (t->fndecl) = 0;
}

extern struct pending_input *to_be_restored;
static int nextchar = -1;

/* Called from the fndecl rule in the parser when the function just parsed
   was declared using a PRE_PARSED_FUNCTION_DECL (i.e. came from
   do_pending_inlines).  */
void
process_next_inline (t)
     tree t;
{
  struct pending_inline *i = (struct pending_inline *) TREE_PURPOSE (t);
  my_friendly_assert ((i->parm_vec == NULL_TREE) == (i->bindings == NULL_TREE),
		      227);
  if (i->parm_vec)
    pop_template_decls (i->parm_vec, i->bindings, 0);
  i = i->next;
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    {
      error ("parse error at end of saved function text");
      /* restore_pending_input will abort unless yychar is either
       * END_OF_SAVED_INPUT or YYEMPTY; since we already know we're
       * hosed, feed back YYEMPTY.
       *  We also need to discard nextchar, since that may have gotten
       * set as well.
       */
      nextchar = -1;
    }
  yychar = YYEMPTY;
  if (to_be_restored == 0)
    my_friendly_abort (123);
  restore_pending_input (to_be_restored);
  to_be_restored = 0;
  if (i && i->fndecl != NULL_TREE)
    {
      my_friendly_assert ((i->parm_vec == NULL_TREE) == (i->bindings == NULL_TREE),
			  228);
      if (i->parm_vec)
	push_template_decls (i->parm_vec, i->bindings, 0);
      feed_input (i->buf, i->len, i->can_free ? &inline_text_obstack : 0);
      lineno = i->lineno;
      input_filename = i->filename;
      yychar = PRE_PARSED_FUNCTION_DECL;
      yylval.ttype = build_tree_list ((tree) i, i->fndecl);
#if 0
      if (flag_default_inline
	  /* If we're working from a template, don't change
	     the `inline' state.  */
	  && i->parm_vec == NULL_TREE)
	DECL_INLINE (i->fndecl) = 1;
#endif
      DECL_PENDING_INLINE_INFO (i->fndecl) = 0;
    }
  if (i)
    {
      interface_unknown = i->interface == 1;
      interface_only = i->interface == 0;
    }
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
  int starting_lineno = lineno;
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
	    pedwarn ("ANSI C++ forbids newline in string constant");
	  lineno++;
	}
      obstack_1grow (this_obstack, c);
    }
  while (c != matching_char);
}

static int nextyychar = YYEMPTY;
static YYSTYPE nextyylval;

struct pending_input {
  int nextchar, yychar, nextyychar, eof;
  YYSTYPE yylval, nextyylval;
  struct obstack token_obstack;
  int first_token;
};

struct pending_input *
save_pending_input ()
{
  struct pending_input *p;
  p = (struct pending_input *) xmalloc (sizeof (struct pending_input));
  p->nextchar = nextchar;
  p->yychar = yychar;
  p->nextyychar = nextyychar;
  p->yylval = yylval;
  p->nextyylval = nextyylval;
  p->eof = end_of_file;
  yychar = nextyychar = YYEMPTY;
  nextchar = -1;
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
  my_friendly_assert (nextchar == -1, 229);
  nextchar = p->nextchar;
  my_friendly_assert (yychar == YYEMPTY || yychar == END_OF_SAVED_INPUT, 230);
  yychar = p->yychar;
  my_friendly_assert (nextyychar == YYEMPTY, 231);
  nextyychar = p->nextyychar;
  yylval = p->yylval;
  nextyylval = p->nextyylval;
  first_token = p->first_token;
  obstack_free (&token_obstack, (char *) 0);
  token_obstack = p->token_obstack;
  end_of_file = p->eof;
  free (p);
}

/* Return next non-whitespace input character, which may come
   from `finput', or from `nextchar'.  */
static int
yynextch ()
{
  int c;

  if (nextchar >= 0)
    {
      c = nextchar;
      nextchar = -1;
    }
  else c = getch ();
  return skip_white_space (c);
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
      if (nextchar >= 0)
	put_back (nextchar);
      nextchar = ch;
    }
  else
    {
      my_friendly_assert (nextyychar == YYEMPTY, 232);
      nextyychar = yychar;
      nextyylval = yylval;
      yychar = ch;
    }
}

/* This function stores away the text for an inline function that should
   be processed later.  It decides how much later, and may need to move
   the info between obstacks; therefore, the caller should not refer to
   the T parameter after calling this function.

   This function also stores the list of template-parameter bindings that
   will be needed for expanding the template, if any.  */

static void
store_pending_inline (decl, t)
     tree decl;
     struct pending_inline *t;
{
  extern int processing_template_defn;
  int delay_to_eof = 0;
  struct pending_inline **inlines;

  t->fndecl = decl;
  /* Default: compile right away, and no extra bindings are needed.  */
  t->parm_vec = t->bindings = 0;
  if (processing_template_defn)
    {
      tree type = current_class_type;
      /* Assumption: In this (possibly) nested class sequence, only
	 one name will have template parms.  */
      while (type && TREE_CODE_CLASS (TREE_CODE (type)) == 't')
	{
	  tree decl = TYPE_NAME (type);
	  tree tmpl = IDENTIFIER_TEMPLATE (DECL_NAME (decl));
	  if (tmpl)
	    {
	      t->parm_vec = DECL_TEMPLATE_INFO (TREE_PURPOSE (tmpl))->parm_vec;
	      t->bindings = TREE_VALUE (tmpl);
	    }
	  type = DECL_CONTEXT (decl);
	}
      if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE
	  || TREE_CODE (TREE_TYPE (decl)) == FUNCTION_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	    my_friendly_assert (TYPE_MAX_VALUE (TREE_TYPE (decl)) == current_class_type,
				233);

	  /* Inline functions can be compiled immediately.  Other functions
	     will be output separately, so if we're in interface-only mode,
	     punt them now, or output them now if we're doing implementations
	     and we know no overrides will exist.  Otherwise, we delay until
	     end-of-file, to see if the definition is really required.  */
	  if (DECL_THIS_INLINE (decl))
	    /* delay_to_eof == 0 */;
	  else if (current_class_type && !interface_unknown)
	    {
	      if (interface_only)
		{
#if 0
		  print_node_brief (stderr, "\ndiscarding text for ", decl, 0);
#endif
		  if (t->can_free)
		    obstack_free (&inline_text_obstack, t->buf);
		  DECL_PENDING_INLINE_INFO (decl) = 0;
		  return;
		}
	    }
	  /* Don't delay the processing of virtual functions.  */
	  else if (DECL_VINDEX (decl) == NULL_TREE)
	    delay_to_eof = 1;
	}
      else
	my_friendly_abort (58);
    }

  if (delay_to_eof)
    {
      extern struct pending_inline *pending_template_expansions;

      if (t->can_free)
	{
	  char *free_to = t->buf;
	  t->buf = (char *) obstack_copy (&permanent_obstack, t->buf,
					  t->len + 1);
	  t = (struct pending_inline *) obstack_copy (&permanent_obstack, 
						      (char *)t, sizeof (*t));
	  obstack_free (&inline_text_obstack, free_to);
	}
      inlines = &pending_template_expansions;
      t->can_free = 0;
    }
  else
    {
      inlines = &pending_inlines;
      DECL_PENDING_INLINE_INFO (decl) = t;
    }

  /* Because we use obstacks, we must process these in precise order.  */
  t->next = *inlines;
  *inlines = t;
}

void reinit_parse_for_block ();

void
reinit_parse_for_method (yychar, decl)
     int yychar;
     tree decl;
{
  int len;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;

  reinit_parse_for_block (yychar, &inline_text_obstack, 0);

  len = obstack_object_size (&inline_text_obstack);
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;
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
      t->can_free = 1;
      t->deja_vu = 0;
      if (interface_unknown && processing_template_defn && flag_external_templates && ! DECL_IN_SYSTEM_HEADER (decl))
	warn_if_unknown_interface (decl);
      t->interface = (interface_unknown ? 1 : (interface_only ? 0 : 2));
      store_pending_inline (decl, t);
    }
}

/* Consume a block -- actually, a method or template definition beginning
   with `:' or `{' -- and save it away on the specified obstack.

   Argument IS_TEMPLATE indicates which set of error messages should be
   output if something goes wrong.  This should really be cleaned up somehow,
   without loss of clarity.  */
void
reinit_parse_for_block (pyychar, obstackp, is_template)
     int pyychar;
     struct obstack *obstackp;
     int is_template;
{
  register int c = 0;
  int blev = 1;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;
  int len;
  int look_for_semicolon = 0;
  int look_for_lbrac = 0;

  if (pyychar == '{')
    obstack_1grow (obstackp, '{');
  else if (pyychar == '=')
    look_for_semicolon = 1;
  else if (pyychar == ':')
    {
      obstack_1grow (obstackp, pyychar);
      look_for_lbrac = 1;
      blev = 0;
    }
  else if (pyychar == RETURN && !is_template)
    {
      obstack_grow (obstackp, "return", 6);
      look_for_lbrac = 1;
      blev = 0;
    }
  else if (pyychar == TRY && !is_template)
    {
      obstack_grow (obstackp, "try", 3);
      look_for_lbrac = 1;
      blev = 0;
    }
  else
    {
      yyerror (is_template
	       ? "parse error in template specification"
	       : "parse error in method specification");
      obstack_1grow (obstackp, '{');
    }

  if (nextchar != EOF)
    {
      c = nextchar;
      nextchar = EOF;
    }
  else
    c = getch ();
  
  while (c != EOF)
    {
      int this_lineno = lineno;

      c = skip_white_space (c);

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
		  error (is_template
			 ? "template body missing"
			 : "function body for constructor missing");
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

/* Build a default function named NAME for type TYPE.
   KIND says what to build.

   When KIND == 0, build default destructor.
   When KIND == 1, build virtual destructor.
   When KIND == 2, build default constructor.
   When KIND == 3, build default X(const X&) constructor.
   When KIND == 4, build default X(X&) constructor.
   When KIND == 5, build default operator = (const X&).
   When KIND == 6, build default operator = (X&).  */

tree
cons_up_default_function (type, full_name, kind)
     tree type, full_name;
     int kind;
{
  extern tree void_list_node;
  char *func_buf = NULL;
  int func_len = 0;
  tree declspecs = NULL_TREE;
  tree fn, args;
  tree argtype;
  int retref = 0;
  int complex = 0;
  tree name = constructor_name (full_name);

  switch (kind)
    {
      /* Destructors.  */
    case 1:
      declspecs = build_decl_list (NULL_TREE, ridpointers [(int) RID_VIRTUAL]);
      /* Fall through...  */
    case 0:
      name = build_parse_node (BIT_NOT_EXPR, name);
      args = void_list_node;
      break;

    case 2:
      /* Default constructor.  */
      args = void_list_node;
      complex = TYPE_NEEDS_CONSTRUCTING (type);
      break;

    case 3:
      type = build_type_variant (type, 1, 0);
      /* Fall through...  */
    case 4:
      /* According to ARM $12.8, the default copy ctor will be declared, but
	 not defined, unless it's needed.  */
      argtype = build_reference_type (type);
      args = tree_cons (NULL_TREE,
			build_tree_list (hash_tree_chain (argtype, NULL_TREE),
					 get_identifier ("_ctor_arg")),
			void_list_node);
      complex = TYPE_HAS_COMPLEX_INIT_REF (type);
      break;

    case 5:
      type = build_type_variant (type, 1, 0);
      /* Fall through...  */
    case 6:
      retref = 1;
      declspecs = build_decl_list (NULL_TREE, full_name);

      name = ansi_opname [(int) MODIFY_EXPR];

      argtype = build_reference_type (type);
      args = tree_cons (NULL_TREE,
			build_tree_list (hash_tree_chain (argtype, NULL_TREE),
					 get_identifier ("_ctor_arg")),
			void_list_node);
      complex = TYPE_HAS_COMPLEX_ASSIGN_REF (type);
      break;

    default:
      my_friendly_abort (59);
    }

  declspecs = decl_tree_cons (NULL_TREE, ridpointers [(int) RID_INLINE],
			      declspecs);

  TREE_PARMLIST (args) = 1;

  {
    tree declarator = build_parse_node (CALL_EXPR, name, args, NULL_TREE);
    if (retref)
      declarator = build_parse_node (ADDR_EXPR, declarator);
    
    fn = grokfield (declarator, declspecs, NULL_TREE, NULL_TREE,
		    NULL_TREE, NULL_TREE);
  }
  
  if (fn == void_type_node)
    return fn;

  if (processing_template_defn)
    {
      SET_DECL_IMPLICIT_INSTANTIATION (fn);
      repo_template_used (fn);
    }

  if (CLASSTYPE_INTERFACE_KNOWN (type))
    {
      DECL_INTERFACE_KNOWN (fn) = 1;
      DECL_NOT_REALLY_EXTERN (fn) = (!CLASSTYPE_INTERFACE_ONLY (type)
				     && flag_implement_inlines);
    }
  else
    DECL_NOT_REALLY_EXTERN (fn) = 1;

#if 0
  /* When on-the-fly synthesis works properly, remove the second and third
     conditions here.  */
  if (flag_keep_inline_functions
#if 0
      || ! flag_no_inline
      || complex
#endif
      || ! DECL_EXTERNAL (fn))
    {
      struct pending_inline *t;
      t = (struct pending_inline *)
	obstack_alloc (&synth_obstack, sizeof (struct pending_inline));
      t->lineno = -kind;
      t->can_free = 0;
      t->interface = (interface_unknown ? 1 : (interface_only ? 0 : 2));
      store_pending_inline (fn, t);
    }
  else
#endif
    mark_inline_for_output (fn);

#ifdef DEBUG_DEFAULT_FUNCTIONS
  { char *fn_type = NULL;
    tree t = name;
    switch (kind)
      {
      case 0: fn_type = "default destructor"; break;
      case 1: fn_type = "virtual destructor"; break;
      case 2: fn_type = "default constructor"; break;
      case 3: fn_type = "default X(const X&)"; break;
      case 4: fn_type = "default X(X&)"; break;
      }
    if (fn_type)
      {
	if (TREE_CODE (name) == BIT_NOT_EXPR)
	  t = TREE_OPERAND (name, 0);
	fprintf (stderr, "[[[[ %s for %s:\n%s]]]]\n", fn_type,
		 IDENTIFIER_POINTER (t), func_buf);
      }
  }
#endif /* DEBUG_DEFAULT_FUNCTIONS */

  /* Show that this function was generated by the compiler.  */
  SET_DECL_ARTIFICIAL (fn);
  
  return fn;
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
       && yychar != TYPENAME)
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
  if (TREE_CODE_CLASS (TREE_CODE (type)) != 't')
    my_friendly_abort (60);
  if (IS_AGGR_TYPE (type))
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
      if (TREE_CODE_CLASS (TREE_CODE (type)) == 't')
	note_got_semicolon (type);
    }
  clear_anon_tags ();
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
	case '\n':
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\r':
	case '\v':
	case '\b':
	  do
	    c = getch ();
	  while (c == ' ' || c == '\t');
	  break;

	case '\\':
	  c = getch ();
	  if (c == '\n')
	    lineno++;
	  else
	    error ("stray '\\' in program");
	  c = getch ();
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

static char *
extend_token_buffer (p)
     char *p;
{
  int offset = p - token_buffer;

  maxtoken = maxtoken * 2 + 10;
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);

  return token_buffer + offset;
}

static int
get_last_nonwhite_on_line ()
{
  register int c;

  /* Is this the last nonwhite stuff on the line?  */
  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
    c = getch ();

  while (c == ' ' || c == '\t')
    c = getch ();
  return c;
}

/* At the beginning of a line, increment the line number
   and process any #-directive on this line.
   If the line is a #-directive, read the entire line and return a newline.
   Otherwise, return the line's first non-whitespace character.  */

int linemode;

int
check_newline ()
{
  register int c;
  register int token;

  /* Read first nonwhite char on the line.  Do this before incrementing the
     line number, in case we're at the end of saved text.  */

  do
    c = getch ();
  while (c == ' ' || c == '\t');

  lineno++;

  if (c != '#')
    {
      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Don't read beyond this line.  */
  linemode = 1;
  
  /* Read first nonwhite char after the `#'.  */

  do
    c = getch ();
  while (c == ' ' || c == '\t');

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma'.  */

  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    {
      if (c == 'p')
	{
	  if (getch () == 'r'
	      && getch () == 'a'
	      && getch () == 'g'
	      && getch () == 'm'
	      && getch () == 'a')
	    {
	      /* Read first nonwhite char after the `#pragma'.  */

	      do
		c = getch ();
	      while (c == ' ' || c == '\t');

	      if (c == 'v'
		  && getch () == 't'
		  && getch () == 'a'
		  && getch () == 'b'
		  && getch () == 'l'
		  && getch () == 'e'
		  && ((c = getch ()) == ' ' || c == '\t'))
		{
		  extern tree pending_vtables;

		  /* More follows: it must be a string constant (class name).  */
		  token = real_yylex ();
		  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
		    {
		      error ("invalid #pragma vtable");
		      goto skipline;
		    }
		  if (write_virtuals != 2)
		    {
		      warning ("use `+e2' option to enable #pragma vtable");
		      goto skipline;
		    }
		  pending_vtables = perm_tree_cons (NULL_TREE, get_identifier (TREE_STRING_POINTER (yylval.ttype)), pending_vtables);
		  if (nextchar < 0)
		    nextchar = getch ();
		  c = nextchar;
		  if (c != EOF)
		    warning ("trailing characters ignored");
		}
	      else if (c == 'u'
		       && getch () == 'n'
		       && getch () == 'i'
		       && getch () == 't'
		       && ((c = getch ()) == ' ' || c == '\t'))
		{
		  /* More follows: it must be a string constant (unit name).  */
		  token = real_yylex ();
		  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
		    {
		      error ("invalid #pragma unit");
		      goto skipline;
		    }
		  current_unit_name = get_identifier (TREE_STRING_POINTER (yylval.ttype));
		  current_unit_language = current_lang_name;
		  if (nextchar < 0)
		    nextchar = getch ();
		  c = nextchar;
		  if (c != EOF)
		    warning ("trailing characters ignored");
		}
	      else if (c == 'i')
		{
		  tree fileinfo = IDENTIFIER_CLASS_VALUE (get_time_identifier (input_filename));
		  c = getch ();

		  if (c == 'n'
		      && getch () == 't'
		      && getch () == 'e'
		      && getch () == 'r'
		      && getch () == 'f'
		      && getch () == 'a'
		      && getch () == 'c'
		      && getch () == 'e'
		      && ((c = getch ()) == ' ' || c == '\t' || c == EOF))
		    {
		      int warned_already = 0;
		      char *main_filename = input_filename;

		      main_filename = FILE_NAME_NONDIRECTORY (main_filename);
		      while (c == ' ' || c == '\t')
			c = getch ();
		      if (c != EOF)
			{
			  put_back (c);
			  token = real_yylex ();
			  if (token != STRING
			      || TREE_CODE (yylval.ttype) != STRING_CST)
			    {
			      error ("invalid `#pragma interface'");
			      goto skipline;
			    }
			  main_filename = TREE_STRING_POINTER (yylval.ttype);
			  c = getch();
			  put_back (c);
			}

		      while (c == ' ' || c == '\t')
			c = getch ();

		      while (c != EOF)
			{
			  if (!warned_already && extra_warnings
			      && c != ' ' && c != '\t')
			    {
			      warning ("garbage after `#pragma interface' ignored");
			      warned_already = 1;
			    }
			  c = getch ();
			}

		      write_virtuals = 3;

		      if (impl_file_chain == 0)
			{
			  /* If this is zero at this point, then we are
			     auto-implementing.  */
			  if (main_input_filename == 0)
			    main_input_filename = input_filename;

#ifdef AUTO_IMPLEMENT
			  filename = FILE_NAME_NONDIRECTORY (main_input_filename);
			  fi = get_time_identifier (filename);
			  fi = IDENTIFIER_CLASS_VALUE (fi);
			  TREE_INT_CST_LOW (fi) = 0;
			  TREE_INT_CST_HIGH (fi) = 1;
			  /* Get default.  */
			  impl_file_chain = (struct impl_files *)permalloc (sizeof (struct impl_files));
			  impl_file_chain->filename = filename;
			  impl_file_chain->next = 0;
#endif
			}

		      interface_only = interface_strcmp (main_filename);
		      interface_unknown = 0;
		      TREE_INT_CST_LOW (fileinfo) = interface_only;
		      TREE_INT_CST_HIGH (fileinfo) = interface_unknown;
		    }
		  else if (c == 'm'
			   && getch () == 'p'
			   && getch () == 'l'
			   && getch () == 'e'
			   && getch () == 'm'
			   && getch () == 'e'
			   && getch () == 'n'
			   && getch () == 't'
			   && getch () == 'a'
			   && getch () == 't'
			   && getch () == 'i'
			   && getch () == 'o'
			   && getch () == 'n'
			   && ((c = getch ()) == ' ' || c == '\t' || c == EOF))
		    {
		      int warned_already = 0;
		      char *main_filename = main_input_filename ? main_input_filename : input_filename;

		      main_filename = FILE_NAME_NONDIRECTORY (main_filename);
		      while (c == ' ' || c == '\t')
			c = getch ();
		      if (c != EOF)
			{
			  put_back (c);
			  token = real_yylex ();
			  if (token != STRING
			      || TREE_CODE (yylval.ttype) != STRING_CST)
			    {
			      error ("invalid `#pragma implementation'");
			      goto skipline;
			    }
			  main_filename = TREE_STRING_POINTER (yylval.ttype);
			  c = getch();
			  put_back (c);
			}

		      while (c == ' ' || c == '\t')
			c = getch ();

		      while (c != EOF)
			{
			  if (!warned_already && extra_warnings
			      && c != ' ' && c != '\t')
			    {
			      warning ("garbage after `#pragma implementation' ignored");
			      warned_already = 1;
			    }
			  c = getch ();
			}

		      if (write_virtuals == 3)
			{
			  struct impl_files *ifiles = impl_file_chain;
			  while (ifiles)
			    {
			      if (! strcmp (ifiles->filename, main_filename))
				break;
			      ifiles = ifiles->next;
			    }
			  if (ifiles == 0)
			    {
			      ifiles = (struct impl_files*) permalloc (sizeof (struct impl_files));
			      ifiles->filename = main_filename;
			      ifiles->next = impl_file_chain;
			      impl_file_chain = ifiles;
			    }
			}
		      else if ((main_input_filename != 0
				&& ! strcmp (main_input_filename, input_filename))
			       || ! strcmp (input_filename, main_filename))
			{
			  write_virtuals = 3;
			  if (impl_file_chain == 0)
			    {
			      impl_file_chain = (struct impl_files*) permalloc (sizeof (struct impl_files));
			      impl_file_chain->filename = main_filename;
			      impl_file_chain->next = 0;
			    }
			}
		      else
			error ("`#pragma implementation' can only appear at top-level");
		      interface_only = 0;
#if 1
		      /* We make this non-zero so that we infer decl linkage
			 in the impl file only for variables first declared
			 in the interface file.  */
		      interface_unknown = 1;
#else
		      /* We make this zero so that templates in the impl
                         file will be emitted properly. */
		      interface_unknown = 0;
#endif
		      TREE_INT_CST_LOW (fileinfo) = interface_only;
		      TREE_INT_CST_HIGH (fileinfo) = interface_unknown;
		    }
		}
#ifdef HANDLE_SYSV_PRAGMA
	      else
		{
		  put_back (c);
		  handle_sysv_pragma ();
		}
#else
#ifdef HANDLE_PRAGMA
	      /* FIXME: This will break if we're doing any of the C++ input
                 tricks.  */
	      else
		{
		  ungetc (c, finput);
		  HANDLE_PRAGMA (finput);
		}
#endif
#endif
	      goto skipline;
	    }
	}
      else if (c == 'd')
	{
	  if (getch () == 'e'
	      && getch () == 'f'
	      && getch () == 'i'
	      && getch () == 'n'
	      && getch () == 'e'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
#ifdef DWARF_DEBUGGING_INFO
	      if ((debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_define (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'u')
	{
	  if (getch () == 'n'
	      && getch () == 'd'
	      && getch () == 'e'
	      && getch () == 'f'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
#ifdef DWARF_DEBUGGING_INFO
	      if ((debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_undef (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'l')
	{
	  if (getch () == 'i'
	      && getch () == 'n'
	      && getch () == 'e'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    goto linenum;
	}
      else if (c == 'i')
	{
	  if (getch () == 'd'
	      && getch () == 'e'
	      && getch () == 'n'
	      && getch () == 't'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
#ifdef ASM_OUTPUT_IDENT
              extern FILE *asm_out_file;
#endif
	      /* #ident.  The pedantic warning is now in cccp.c.  */

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = getch ();

	      /* If no argument, ignore the line.  */
	      if (c == EOF)
		goto skipline;

	      put_back (c);
	      token = real_yylex ();
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
	}
      else if (c == 'n')
	{
	  if (getch () == 'e'
	      && getch () == 'w'
	      && getch () == 'w'
	      && getch () == 'o'
	      && getch () == 'r'
	      && getch () == 'l'
	      && getch () == 'd'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
	      /* Used to test incremental compilation.  */
	      sorry ("#pragma newworld");
	      goto skipline;
	    }
	}
      error ("undefined or invalid # directive");
      goto skipline;
    }

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = getch ();

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == EOF)
    goto skipline;

  /* Something follows the #; read a token.  */

  put_back (c);
  token = real_yylex ();

  if (token == CONSTANT
      && TREE_CODE (yylval.ttype) == INTEGER_CST)
    {
      int old_lineno = lineno;
      enum { act_none, act_push, act_pop } action = act_none;
      int entering_system_header = 0;
      int entering_c_header = 0;

      /* subtract one, because it is the following line that
	 gets the specified number */

      int l = TREE_INT_CST_LOW (yylval.ttype) - 1;
      c = get_last_nonwhite_on_line ();
      if (c == EOF)
	{
	  /* No more: store the line number and check following line.  */
	  lineno = l;
	  goto skipline;
	}
      put_back (c);

      /* More follows: it must be a string constant (filename).  */

      /* Read the string constant, but don't treat \ as special.  */
      ignore_escape_flag = 1;
      token = real_yylex ();
      ignore_escape_flag = 0;

      if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #line");
	  goto skipline;
	}

      /* Changing files again.  This means currently collected time
	 is charged against header time, and body time starts back
	 at 0.  */
      if (flag_detailed_statistics)
	{
	  int this_time = my_get_run_time ();
	  tree time_identifier = get_time_identifier (TREE_STRING_POINTER (yylval.ttype));
	  header_time += this_time - body_time;
	  TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time))
	    += this_time - body_time;
	  this_filename_time = time_identifier;
	  body_time = this_time;
	}

      if (flag_cadillac)
	cadillac_note_source ();

      input_filename
	= (char *) permalloc (TREE_STRING_LENGTH (yylval.ttype) + 1);
      strcpy (input_filename, TREE_STRING_POINTER (yylval.ttype));
      lineno = l;
      GNU_xref_file (input_filename);
      
      if (main_input_filename == 0)
	{
	  struct impl_files *ifiles = impl_file_chain;

	  if (ifiles)
	    {
	      while (ifiles->next)
		ifiles = ifiles->next;
	      ifiles->filename = FILE_NAME_NONDIRECTORY (input_filename);
	    }

	  main_input_filename = input_filename;
	  if (write_virtuals == 3)
	    walk_vtables (set_typedecl_interface_info, set_vardecl_interface_info);
	}

      extract_interface_info ();

      c = get_last_nonwhite_on_line ();
      if (c == EOF)
	{
	  /* Update the name in the top element of input_file_stack.  */
	  if (input_file_stack)
	    input_file_stack->name = input_filename;
	}
      else
	{
	  put_back (c);

	  token = real_yylex ();

	  /* `1' after file name means entering new file.
	     `2' after file name means just left a file.  */

	  if (token == CONSTANT
	      && TREE_CODE (yylval.ttype) == INTEGER_CST)
	    {
	      if (TREE_INT_CST_LOW (yylval.ttype) == 1)
		action = act_push;
	      else if (TREE_INT_CST_LOW (yylval.ttype) == 2)
		action = act_pop;

	      if (action)
		{
		  c = get_last_nonwhite_on_line ();
		  if (c != EOF)
		    {
		      put_back (c);
		      token = real_yylex ();
		    }
		}
	    }

	  /* `3' after file name means this is a system header file.  */

	  if (token == CONSTANT
	      && TREE_CODE (yylval.ttype) == INTEGER_CST
	      && TREE_INT_CST_LOW (yylval.ttype) == 3)
	    {
	      entering_system_header = 1;

	      c = get_last_nonwhite_on_line ();
	      if (c != EOF)
		{
		  put_back (c);
		  token = real_yylex ();
		}
	    }

	  /* `4' after file name means this is a C header file.  */

	  if (token == CONSTANT
	      && TREE_CODE (yylval.ttype) == INTEGER_CST
	      && TREE_INT_CST_LOW (yylval.ttype) == 4)
	    {
	      entering_c_header = 1;

	      c = get_last_nonwhite_on_line ();
	      if (c != EOF)
		{
		  put_back (c);
		  token = real_yylex ();
		}
	    }

	  /* Do the actions implied by the preceding numbers.  */

	  if (action == act_push)
	    {
	      /* Pushing to a new file.  */
	      struct file_stack *p;

	      p = (struct file_stack *) xmalloc (sizeof (struct file_stack));
	      input_file_stack->line = old_lineno;
	      p->next = input_file_stack;
	      p->name = input_filename;
	      input_file_stack = p;
	      input_file_stack_tick++;
#ifdef DWARF_DEBUGGING_INFO
	      if (debug_info_level == DINFO_LEVEL_VERBOSE
		  && write_symbols == DWARF_DEBUG)
		dwarfout_start_new_source_file (input_filename);
#endif /* DWARF_DEBUGGING_INFO */
	      if (flag_cadillac)
		cadillac_push_source ();
	      in_system_header = entering_system_header;
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
		  struct file_stack *p;

		  if (c_header_level && --c_header_level == 0)
		    {
		      if (entering_c_header)
			warning ("badly nested C headers from preprocessor");
		      --pending_lang_change;
		    }
		  if (flag_cadillac)
		    cadillac_pop_source ();
		  in_system_header = entering_system_header;

		  p = input_file_stack;
		  input_file_stack = p->next;
		  free (p);
		  input_file_stack_tick++;
#ifdef DWARF_DEBUGGING_INFO
		  if (debug_info_level == DINFO_LEVEL_VERBOSE
		      && write_symbols == DWARF_DEBUG)
		    dwarfout_resume_previous_source_file (input_file_stack->line);
#endif /* DWARF_DEBUGGING_INFO */
		}
	      else
		error ("#-lines for entering and leaving files don't match");
	    }
	  else
	    {
	      in_system_header = entering_system_header;
	      if (flag_cadillac)
		cadillac_switch_source (-1);
	    }
	}

      /* If NEXTCHAR is not end of line, we don't care what it is.  */
      if (nextchar == EOF)
	c = EOF;
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  linemode = 0;
  end_of_file = 0;
  while ((c = getch ()) != EOF && c != '\n');
  return c;
}

void
do_pending_lang_change ()
{
  for (; pending_lang_change > 0; --pending_lang_change)
    push_lang_context (lang_name_c);
  for (; pending_lang_change < 0; ++pending_lang_change)
    pop_lang_context ();
}

#if 0
#define isalnum(char) (char >= 'a' ? char <= 'z' : char >= '0' ? char <= '9' || (char >= 'A' && char <= 'Z') : 0)
#define isdigit(char) (char >= '0' && char <= '9')
#else
#include <ctype.h>
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
  unsigned firstdig;
  int nonnull;

  switch (c)
    {
    case 'x':
      if (warn_traditional)
	warning ("the meaning of `\\x' varies with -traditional");

      if (flag_traditional)
	return c;

      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = getch ();
	  if (! isxdigit (c))
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
		   && ((1 << (TYPE_PRECISION (integer_type_node) - (count - 1) * 4))
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
      if (warn_traditional)
	warning ("the meaning of `\\a' varies with -traditional");

      if (flag_traditional)
	return c;
      return TARGET_BELL;

    case 'v':
      return TARGET_VT;

    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
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
  if (c >= 040 && c < 0177)
    pedwarn ("unknown escape sequence `\\%c'", c);
  else
    pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

/* Value is 1 (or 2) if we should try to make the next identifier look like
   a typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion.  */
int looking_for_typename = 0;

#if 0
/* NO LONGER USED: Value is -1 if we must not see a type name.  */
void
dont_see_typename ()
{
  looking_for_typename = -1;
  if (yychar == TYPENAME || yychar == PTYPENAME)
    {
      yychar = IDENTIFIER;
      lastiddecl = 0;
    }
}
#endif

#ifdef __GNUC__
extern __inline int identifier_type ();
__inline
#endif
int
identifier_type (decl)
     tree decl;
{
  if (TREE_CODE (decl) == TEMPLATE_DECL
      && DECL_TEMPLATE_IS_CLASS (decl))
    return PTYPENAME;
  if (TREE_CODE (decl) == NAMESPACE_DECL)
    return NSNAME;
  if (TREE_CODE (decl) != TYPE_DECL)
    return IDENTIFIER;
  return TYPENAME;
}

void
see_typename ()
{
  looking_for_typename = 1;
  if (yychar < 0)
    if ((yychar = yylex()) < 0) yychar = 0;
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

tree
do_identifier (token)
     register tree token;
{
  register tree id = lastiddecl;

  if (yychar == YYEMPTY)
    yychar = yylex ();
  /* Scope class declarations before global
     declarations.  */
  if (id == IDENTIFIER_GLOBAL_VALUE (token)
      && current_class_type != 0
      && TYPE_SIZE (current_class_type) == 0
      && TREE_CODE (current_class_type) != UNINSTANTIATED_P_TYPE)
    {
      /* Could be from one of the base classes.  */
      tree field = lookup_field (current_class_type, token, 1, 0);
      if (field == 0)
	;
      else if (field == error_mark_node)
	/* We have already generated the error message.
	   But we still want to return this value.  */
	id = lookup_field (current_class_type, token, 0, 0);
      else if (TREE_CODE (field) == VAR_DECL
	       || TREE_CODE (field) == CONST_DECL)
	id = field;
      else if (TREE_CODE (field) != FIELD_DECL)
	my_friendly_abort (61);
      else
	{
	  cp_error ("invalid use of member `%D' from base class `%T'", field,
		      DECL_FIELD_CONTEXT (field));
	  id = error_mark_node;
	  return id;
	}
    }

  /* Remember that this name has been used in the class definition, as per
     [class.scope0] */
  if (id && current_class_type
      && TYPE_BEING_DEFINED (current_class_type)
      && ! IDENTIFIER_CLASS_VALUE (token))
    pushdecl_class_level (id);
    
  if (!id || id == error_mark_node)
    {
      if (id == error_mark_node && current_class_type != NULL_TREE)
	{
	  id = lookup_nested_field (token, 1);
	  /* In lookup_nested_field(), we marked this so we can gracefully
	     leave this whole mess.  */
	  if (id && id != error_mark_node && TREE_TYPE (id) == error_mark_node)
	    return id;
	}
      if (yychar == '(' || yychar == LEFT_RIGHT)
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
	  if (IDENTIFIER_GLOBAL_VALUE (token) != error_mark_node
	      || IDENTIFIER_ERROR_LOCUS (token) != current_function_decl)
	    {
	      static int undeclared_variable_notice;

	      cp_error ("`%D' undeclared (first use this function)", token);

	      if (! undeclared_variable_notice)
		{
		  error ("(Each undeclared identifier is reported only once");
		  error ("for each function it appears in.)");
		  undeclared_variable_notice = 1;
		}
	    }
	  id = error_mark_node;
	  /* Prevent repeated error messages.  */
	  IDENTIFIER_GLOBAL_VALUE (token) = error_mark_node;
	  SET_IDENTIFIER_ERROR_LOCUS (token, current_function_decl);
	}
    }

  if (TREE_CODE (id) == VAR_DECL && DECL_DEAD_FOR_LOCAL (id))
    {
      tree shadowed = DECL_SHADOWED_FOR_VAR (id);
      if (shadowed)
	{
	  if (!DECL_ERROR_REPORTED (id))
	    {
	      warning ("name lookup of `%s' changed",
		       IDENTIFIER_POINTER (token));
	      cp_warning_at ("  matches this `%D' under current ANSI rules",
			     shadowed);
	      cp_warning_at ("  matches this `%D' under old rules", id);
	      DECL_ERROR_REPORTED (id) = 1;
	    }
	  id = shadowed;
	}
      else if (!DECL_ERROR_REPORTED (id))
	{
	  static char msg[]
	    = "name lookup of `%s' changed for new ANSI `for' scoping";
	  DECL_ERROR_REPORTED (id) = 1;
	  if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (id)))
	    {
	      error (msg, IDENTIFIER_POINTER (token));
	      cp_error_at ("  cannot use obsolete binding at `%D' because it has a destructor", id);
	      id = error_mark_node;
	    }
	  else
	    {
	      pedwarn (msg, IDENTIFIER_POINTER (token));
	      cp_pedwarn_at ("  using obsolete binding at `%D'", id);
	    }
	}
    }
  /* TREE_USED is set in `hack_identifier'.  */
  if (TREE_CODE (id) == CONST_DECL)
    {
      if (IDENTIFIER_CLASS_VALUE (token) == id)
	{
	  /* Check access.  */
	  enum access_type access
	    = compute_access (TYPE_BINFO (current_class_type), id);
	  if (access == access_private)
	    cp_error ("enum `%D' is private", id);
	  /* protected is OK, since it's an enum of `this'.  */
	}
      id = DECL_INITIAL (id);
    }
  else
    id = hack_identifier (id, token, yychar);
  return id;
}

tree
identifier_typedecl_value (node)
     tree node;
{
  tree t, type;
  type = IDENTIFIER_TYPE_VALUE (node);
  if (type == NULL_TREE)
    return NULL_TREE;
#define do(X) \
  { \
    t = (X); \
    if (t && TREE_CODE (t) == TYPE_DECL && TREE_TYPE (t) == type) \
      return t; \
  }
  do (IDENTIFIER_LOCAL_VALUE (node));
  do (IDENTIFIER_CLASS_VALUE (node));
  do (IDENTIFIER_GLOBAL_VALUE (node));
#undef do
  /* Will this one ever happen?  */
  if (TYPE_NAME (type))
    return TYPE_NAME (type);

  /* We used to do an internal error of 62 here, but instead we will
     handle the return of a null appropriately in the callers.  */
  return NULL_TREE;
}

struct try_type
{
  tree *node_var;
  char unsigned_flag;
  char long_flag;
  char long_long_flag;
};

struct try_type type_sequence[] = 
{
  { &integer_type_node, 0, 0, 0},
  { &unsigned_type_node, 1, 0, 0},
  { &long_integer_type_node, 0, 1, 0},
  { &long_unsigned_type_node, 1, 1, 0},
  { &long_long_integer_type_node, 0, 1, 1},
  { &long_long_unsigned_type_node, 1, 1, 1}
};

int
real_yylex ()
{
  register int c;
  register int value;
  int wide_flag = 0;
  int dollar_seen = 0;
  int i;

  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
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
      token_buffer[0] = '\0';
      end_of_file = 1;
      if (input_redirected ())
	value = END_OF_SAVED_INPUT;
      else if (linemode)
	value = END_OF_LINE;
      else if (do_pending_expansions ())
	/* this will set yychar for us */
	return yychar;
      else
	value = ENDFILE;
      break;

    case '$':
      if (dollars_in_ident)
	{
	  dollar_seen = 1;
	  goto letter;
	}
      value = '$';
      goto done;

    case 'L':
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	register int c = getch ();
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
	put_back (c);
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
    letter:
      {
	register char *p;

	p = token_buffer;
	if (input == 0)
	  {
	    /* We know that `token_buffer' can hold at least on char,
	       so we install C immediately.
	       We may have to read the value in `putback_char', so call
	       `getch' once.  */
	    *p++ = c;
	    c = getch ();

	    /* Make this run fast.  We know that we are reading straight
	       from FINPUT in this case (since identifiers cannot straddle
	       input sources.  */
	    while (isalnum (c) || (c == '_') || c == '$')
	      {
		if (c == '$' && ! dollars_in_ident)
		  break;
		if (p >= token_buffer + maxtoken)
		  p = extend_token_buffer (p);

		*p++ = c;
		c = getc (finput);
	      }

	    if (linemode && c == '\n')
	      {
		put_back (c);
		c = EOF;
	      }
	  }
	else
	  {
	    /* We know that `token_buffer' can hold at least on char,
	       so we install C immediately.  */
	    *p++ = c;
	    c = getch ();

	    while (isalnum (c) || (c == '_') || c == '$')
	      {
		if (c == '$' && ! dollars_in_ident)
		  break;
		if (p >= token_buffer + maxtoken)
		  p = extend_token_buffer (p);

		*p++ = c;
		c = getch ();
	      }
	  }

	*p = 0;
	nextchar = c;

	value = IDENTIFIER;
	yylval.itype = 0;

      /* Try to recognize a keyword.  Uses minimum-perfect hash function */

	{
	  register struct resword *ptr;

	  if (ptr = is_reserved_word (token_buffer, p - token_buffer))
	    {
	      if (ptr->rid)
		{
		  tree old_ttype = ridpointers[(int) ptr->rid];

		  /* If this provides a type for us, then revert lexical
		     state to standard state.  */
		  if (TREE_CODE (old_ttype) == IDENTIFIER_NODE
		      && IDENTIFIER_GLOBAL_VALUE (old_ttype) != 0
		      && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (old_ttype)) == TYPE_DECL)
		    looking_for_typename = 0;
		  else if (ptr->token == AGGR || ptr->token == ENUM)
		    looking_for_typename = 1;

		  /* Check if this is a language-type declaration.
		     Just glimpse the next non-white character.  */
		  nextchar = skip_white_space (nextchar);
		  if (nextchar == '"')
		    {
		      /* We are looking at a string.  Complain
			 if the token before the string is no `extern'.
			 
			 Could cheat some memory by placing this string
			 on the temporary_, instead of the saveable_
			 obstack.  */

		      if (ptr->rid != RID_EXTERN)
			error ("invalid modifier `%s' for language string",
			       ptr->name);
		      real_yylex ();
		      value = EXTERN_LANG_STRING;
		      yylval.ttype = get_identifier (TREE_STRING_POINTER (yylval.ttype));
		      break;
		    }
		  if (ptr->token == VISSPEC)
		    {
		      switch (ptr->rid)
			{
			case RID_PUBLIC:
			  yylval.itype = access_public;
			  break;
			case RID_PRIVATE:
			  yylval.itype = access_private;
			  break;
			case RID_PROTECTED:
			  yylval.itype = access_protected;
			  break;
			default:
			  my_friendly_abort (63);
			}
		    }
		  else
		    yylval.ttype = old_ttype;
		}
	      else if (ptr->token == EQCOMPARE)
		{
		  yylval.code = NE_EXPR;
		  token_buffer[0] = '!';
		  token_buffer[1] = '=';
		  token_buffer[2] = 0;
		}
	      else if (ptr->token == ASSIGN)
		{
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
		}
	      else if (ptr->token == '&')
		{
		  yylval.code = BIT_AND_EXPR;
		  token_buffer[0] = '&';
		  token_buffer[1] = 0;
		}
	      else if (ptr->token == '|')
		{
		  yylval.code = BIT_IOR_EXPR;
		  token_buffer[0] = '|';
		  token_buffer[1] = 0;
		}
	      else if (ptr->token == '^')
		{
		  yylval.code = BIT_XOR_EXPR;
		  token_buffer[0] = '^';
		  token_buffer[1] = 0;
		}

	      value = (int) ptr->token;
	    }
	}

	/* If we did not find a keyword, look for an identifier
	   (or a typename).  */

	if (strcmp ("catch", token_buffer) == 0
	    || strcmp ("throw", token_buffer) == 0
	    || strcmp ("try", token_buffer) == 0)
	  {
	    static int did_warn = 0;
	    if (! did_warn  && ! flag_handle_exceptions)
	      {
		pedwarn ("`catch', `throw', and `try' are all C++ reserved words");
		did_warn = 1;
	      }
	  }

	if (value == IDENTIFIER || value == TYPESPEC)
	  GNU_xref_ref (current_function_decl, token_buffer);

	if (value == IDENTIFIER)
	  {
	    register tree tmp = get_identifier (token_buffer);

#if !defined(VMS) && defined(JOINER)
	    /* Make sure that user does not collide with our internal
	       naming scheme.  */
	    if (JOINER == '$'
		&& dollar_seen
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

	    /* A user-invisible read-only initialized variable
	       should be replaced by its value.  We only handle strings
	       since that's the only case used in C (and C++).  */
	    /* Note we go right after the local value for the identifier
	       (e.g., __FUNCTION__ or __PRETTY_FUNCTION__).  We used to
	       call lookup_name, but that could result in an error about
	       ambiguities.  */
	    tmp = IDENTIFIER_LOCAL_VALUE (yylval.ttype);
	    if (tmp != NULL_TREE
		&& TREE_CODE (tmp) == VAR_DECL
		&& DECL_IGNORED_P (tmp)
		&& TREE_READONLY (tmp)
		&& DECL_INITIAL (tmp) != NULL_TREE
		&& TREE_CODE (DECL_INITIAL (tmp)) == STRING_CST)
	      {
		yylval.ttype = DECL_INITIAL (tmp);
		value = STRING;
	      }
	  }
	if (value == NEW && ! global_bindings_p ())
	  {
	    value = NEW;
	    goto done;
	  }
      }
      break;

    case '.':
      {
	register int c1 = getch ();
	token_buffer[0] = c;
	token_buffer[1] = c1;
	if (c1 == '*')
	  {
	    value = DOT_STAR;
	    token_buffer[2] = 0;
	    goto done;
	  }
	if (c1 == '.')
	  {
	    c1 = getch ();
	    if (c1 == '.')
	      {
		token_buffer[2] = c1;
		token_buffer[3] = 0;
		value = ELLIPSIS;
		goto done;
	      }
	    error ("parse error at `..'");
	  }
	if (isdigit (c1))
	  {
	    put_back (c1);
	    goto resume_numerical_scan;
	  }
	nextchar = c1;
	value = '.';
	token_buffer[1] = 0;
	goto done;
      }
    case '0':  case '1':
	/* Optimize for most frequent case.  */
      {
	register int c1 = getch ();
	if (! isalnum (c1) && c1 != '.')
	  {
	    /* Terminate string.  */
	    token_buffer[0] = c;
	    token_buffer[1] = 0;
	    if (c == '0')
	      yylval.ttype = integer_zero_node;
	    else
	      yylval.ttype = integer_one_node;
	    nextchar = c1;
	    value = CONSTANT;
	    goto done;
	  }
	put_back (c1);
      }
      /* fall through... */
			  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    resume_numerical_scan:
      {
	register char *p;
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	/* for multi-precision arithmetic,
	   we actually store only HOST_BITS_PER_CHAR bits in each part.
	   The number of parts is chosen so as to be sufficient to hold
	   the enough bits to fit into the two HOST_WIDE_INTs that contain
	   the integer value (this is always at least as many bits as are
	   in a target `long long' value, but may be wider).  */
#define TOTAL_PARTS ((HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR) * 2 + 2)
	int parts[TOTAL_PARTS];
	int overflow = 0;

	enum anon1 { NOT_FLOAT, AFTER_POINT, TOO_MANY_POINTS} floatflag
	  = NOT_FLOAT;

	p = token_buffer;
	*p++ = c;

	for (count = 0; count < TOTAL_PARTS; count++)
	  parts[count] = 0;

	if (c == '0')
	  {
	    *p++ = (c = getch ());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = getch ());
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
	       || (isalnum (c) && (c != 'l') && (c != 'L')
		   && (c != 'u') && (c != 'U')
		   && (floatflag == NOT_FLOAT || ((c != 'f') && (c != 'F')))))
	  {
	    if (c == '.')
	      {
		if (base == 16)
		  error ("floating constant may not be in radix 16");
		if (floatflag == AFTER_POINT)
		  {
		    error ("malformed floating constant");
		    floatflag = TOO_MANY_POINTS;
		  }
		else
		  floatflag = AFTER_POINT;

		base = 10;
		*p++ = c = getch ();
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.
		   Otherwise, unread the following non-digit
		   and use the '.' as a structural token.  */
		if (p == token_buffer + 2 && !isdigit (c))
		  {
		    if (c == '.')
		      {
			c = getch ();
			if (c == '.')
			  {
			    *p++ = '.';
			    *p = '\0';
			    value = ELLIPSIS;
			    goto done;
			  }
			error ("parse error at `..'");
		      }
		    nextchar = c;
		    token_buffer[1] = '\0';
		    value = '.';
		    goto done;
		  }
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (isdigit (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if (c == 'e' || c == 'E')
		      {
			base = 10;
			floatflag = AFTER_POINT;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
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

		/* If the extra highest-order part ever gets anything in it,
		   the number is certainly too big.  */
		if (parts[TOTAL_PARTS - 1] != 0)
		  overflow = 1;

		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = (c = getch ());
	      }
	  }

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the string */
	*--p = 0;

	if (floatflag != NOT_FLOAT)
	  {
	    tree type = double_type_node;
	    char f_seen = 0;
	    char l_seen = 0;
	    int garbage_chars = 0;
	    REAL_VALUE_TYPE value;
	    jmp_buf handler;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((c == 'e') || (c == 'E'))
	      {
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
		if ((c == '+') || (c == '-'))
		  {
		    *p++ = c;
		    c = getch ();
		  }
		if (! isdigit (c))
		  error ("floating constant exponent has no digits");
	        while (isdigit (c))
		  {
		    if (p >= token_buffer + maxtoken - 3)
		      p = extend_token_buffer (p);
		    *p++ = c;
		    c = getch ();
		  }
	      }

	    *p = 0;
	    errno = 0;

	    /* Convert string to a double, checking for overflow.  */
	    if (setjmp (handler))
	      {
		error ("floating constant out of range");
		value = dconst0;
	      }
	    else
	      {
		set_float_handler (handler);
		/*  The second argument, machine_mode, of REAL_VALUE_ATOF
		    tells the desired precision of the binary result of
		    decimal-to-binary conversion. */

		/* Read the suffixes to choose a data type.  */
		switch (c)
		  {
		  case 'f': case 'F':
		    type = float_type_node;
		    value = REAL_VALUE_ATOF (token_buffer, TYPE_MODE (type));
		    garbage_chars = -1;
		    break;

		  case 'l': case 'L':
		    type = long_double_type_node;
		    value = REAL_VALUE_ATOF (token_buffer, TYPE_MODE (type));
		    garbage_chars = -1;
		    break;

		  default:
		    value = REAL_VALUE_ATOF (token_buffer, TYPE_MODE (type));
		  }
		set_float_handler (NULL_PTR);
	      }
	    if (pedantic
		&& (REAL_VALUE_ISINF (value)
#ifdef ERANGE
		    || (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
			&& errno == ERANGE
			/* ERANGE is also reported for underflow, so test the
			   value to distinguish overflow from that.  */
			&& (REAL_VALUES_LESS (dconst1, value)
			    || REAL_VALUES_LESS (value, dconstm1)))
#endif
		    ))
	      {
		pedwarn ("floating point number exceeds range of `%s'",
			 IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
	      }
	    /* Note: garbage_chars is -1 if first char is *not* garbage.  */
	    while (isalnum (c))
	      {
		if (c == 'f' || c == 'F')
		  {
		    if (f_seen)
		      error ("two `f's in floating constant");
		    f_seen = 1;
		  }
		if (c == 'l' || c == 'L')
		  {
		    if (l_seen)
		      error ("two `l's in floating constant");
		    l_seen = 1;
		  }
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
		garbage_chars++;
	      }

	    if (garbage_chars > 0)
	      error ("garbage at end of number");

	    /* Create a node with determined type and value.  */
	    yylval.ttype = build_real (type, value);

	    put_back (c);
	    *p = 0;
	  }
	else
	  {
	    tree type;
	    HOST_WIDE_INT high, low;
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int bytes, warn;

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
			else if (pedantic)
			  pedwarn ("ANSI C++ forbids long long integer constants");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else
		  {
		    if (isalnum (c))
		      {
			error ("garbage at end of number");
			while (isalnum (c))
			  {
			    if (p >= token_buffer + maxtoken - 3)
			      p = extend_token_buffer (p);
			    *p++ = c;
			    c = getch ();
			  }
		      }
		    break;
		  }
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
	      }

	    put_back (c);

	    /* If the constant is not long long and it won't fit in an
	       unsigned long, or if the constant is long long and won't fit
	       in an unsigned long long, then warn that the constant is out
	       of range.  */

	    /* ??? This assumes that long long and long integer types are
	       a multiple of 8 bits.  This better than the original code
	       though which assumed that long was exactly 32 bits and long
	       long was exactly 64 bits.  */

	    if (spec_long_long)
	      bytes = TYPE_PRECISION (long_long_integer_type_node) / 8;
	    else
	      bytes = TYPE_PRECISION (long_integer_type_node) / 8;

	    warn = overflow;
	    for (i = bytes; i < TOTAL_PARTS; i++)
	      if (parts[i])
		warn = 1;
	    if (warn)
	      pedwarn ("integer constant out of range");

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

#if 0
	    /* Find the first allowable type that the value fits in.  */
	    type = 0;
	    for (i = 0; i < sizeof (type_sequence) / sizeof (type_sequence[0]);
		 i++)
	      if (!(spec_long && !type_sequence[i].long_flag)
		  && !(spec_long_long && !type_sequence[i].long_long_flag)
		  && !(spec_unsigned && !type_sequence[i].unsigned_flag)
		  /* A hex or octal constant traditionally is unsigned.  */
		  && !(base != 10 && flag_traditional
		       && !type_sequence[i].unsigned_flag)
		  /* A decimal constant can't be unsigned int
		     unless explicitly specified.  */
		  && !(base == 10 && !spec_unsigned
		       && *type_sequence[i].node_var == unsigned_type_node))
		if (int_fits_type_p (yylval.ttype, *type_sequence[i].node_var))
		  {
		    type = *type_sequence[i].node_var;
		    break;
		  }
	    if (flag_traditional && type == long_unsigned_type_node
		&& !spec_unsigned)
	      type = long_integer_type_node;
	      
	    if (type == 0)
	      {
		type = long_long_integer_type_node;
		warning ("integer constant out of range");
	      }

	    /* Warn about some cases where the type of a given constant
	       changes from traditional C to ANSI C.  */
	    if (warn_traditional)
	      {
		tree other_type = 0;

		/* This computation is the same as the previous one
		   except that flag_traditional is used backwards.  */
		for (i = 0; i < sizeof (type_sequence) / sizeof (type_sequence[0]);
		     i++)
		  if (!(spec_long && !type_sequence[i].long_flag)
		      && !(spec_long_long && !type_sequence[i].long_long_flag)
		      && !(spec_unsigned && !type_sequence[i].unsigned_flag)
		      /* A hex or octal constant traditionally is unsigned.  */
		      && !(base != 10 && !flag_traditional
			   && !type_sequence[i].unsigned_flag)
		      /* A decimal constant can't be unsigned int
			 unless explicitly specified.  */
		      && !(base == 10 && !spec_unsigned
			   && *type_sequence[i].node_var == unsigned_type_node))
		    if (int_fits_type_p (yylval.ttype, *type_sequence[i].node_var))
		      {
			other_type = *type_sequence[i].node_var;
			break;
		      }
		if (!flag_traditional && type == long_unsigned_type_node
		    && !spec_unsigned)
		  type = long_integer_type_node;
	      
		if (other_type != 0 && other_type != type)
		  {
		    if (flag_traditional)
		      warning ("type of integer constant would be different without -traditional");
		    else
		      warning ("type of integer constant would be different with -traditional");
		  }
	      }

#else /* 1 */
	    if (!spec_long && !spec_unsigned
		&& !(flag_traditional && base != 10)
		&& int_fits_type_p (yylval.ttype, integer_type_node))
	      {
#if 0
		if (warn_traditional && base != 10)
		  warning ("small nondecimal constant becomes signed in ANSI C++");
#endif
		type = integer_type_node;
	      }
	    else if (!spec_long && (base != 10 || spec_unsigned)
		     && int_fits_type_p (yylval.ttype, unsigned_type_node))
	      {
		/* Nondecimal constants try unsigned even in traditional C.  */
		type = unsigned_type_node;
	      }

	    else if (!spec_unsigned && !spec_long_long
		     && int_fits_type_p (yylval.ttype, long_integer_type_node))
	      type = long_integer_type_node;

	    else if (! spec_long_long
		     && int_fits_type_p (yylval.ttype,
					 long_unsigned_type_node))
	      {
#if 0
		if (warn_traditional && !spec_unsigned)
		  warning ("large integer constant becomes unsigned in ANSI C++");
#endif
		if (flag_traditional && !spec_unsigned)
		  type = long_integer_type_node;
		else
		  type = long_unsigned_type_node;
	      }

	    else if (! spec_unsigned
		     /* Verify value does not overflow into sign bit.  */
		     && TREE_INT_CST_HIGH (yylval.ttype) >= 0
		     && int_fits_type_p (yylval.ttype,
					 long_long_integer_type_node))
	      type = long_long_integer_type_node;

	    else if (int_fits_type_p (yylval.ttype,
				      long_long_unsigned_type_node))
	      {
#if 0
		if (warn_traditional && !spec_unsigned)
		  warning ("large nondecimal constant is unsigned in ANSI C++");
#endif

		if (flag_traditional && !spec_unsigned)
		  type = long_long_integer_type_node;
		else
		  type = long_long_unsigned_type_node;
	      }

	    else
	      {
		type = long_long_integer_type_node;
		warning ("integer constant out of range");

		if (base == 10 && ! spec_unsigned && TREE_UNSIGNED (type))
		  warning ("decimal integer constant is so large that it is unsigned");
	      }
#endif

	    TREE_TYPE (yylval.ttype) = type;
	    *p = 0;
	  }

	value = CONSTANT; break;
      }

    case '\'':
    char_constant:
      {
	register int result = 0;
	register int num_chars = 0;
	unsigned width = TYPE_PRECISION (char_type_node);
	int max_chars;

	if (wide_flag)
	  {
	    width = WCHAR_TYPE_SIZE;
#ifdef MULTIBYTE_CHARS
	    max_chars = MB_CUR_MAX;
#else
	    max_chars = 1;
#endif
	  }
	else
	  max_chars = TYPE_PRECISION (integer_type_node) / width;

	while (1)
	  {
	  tryagain:

	    c = getch ();

	    if (c == '\'' || c == EOF)
	      break;

	    if (c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto tryagain;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= (1 << width))
		  warning ("escape sequence out of range for character");
#ifdef MAP_CHARACTER
		if (isprint (c))
		  c = MAP_CHARACTER (c);
#endif
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ANSI C++ forbids newline in character constant");
		lineno++;
	      }
#ifdef MAP_CHARACTER
	    else
	      c = MAP_CHARACTER (c);
#endif

	    num_chars++;
	    if (num_chars > maxtoken - 4)
	      extend_token_buffer (token_buffer);

	    token_buffer[num_chars] = c;

	    /* Merge character into result; ignore excess chars.  */
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	token_buffer[num_chars + 1] = '\'';
	token_buffer[num_chars + 2] = 0;

	if (c != '\'')
	  error ("malformatted character constant");
	else if (num_chars == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }
	else if (num_chars != 1 && ! flag_traditional)
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
		= build_int_2 (result & ((unsigned HOST_WIDE_INT) ~0
					 >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       0);
	    else
	      yylval.ttype
		= build_int_2 (result | ~((unsigned HOST_WIDE_INT) ~0
					  >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       -1);
	    if (num_chars<=1)
	      TREE_TYPE (yylval.ttype) = char_type_node;
	    else
	      TREE_TYPE (yylval.ttype) = integer_type_node;
	  }
	else
	  {
#ifdef MULTIBYTE_CHARS
	    /* Set the initial shift state and convert the next sequence.  */
	    result = 0;
	    /* In all locales L'\0' is zero and mbtowc will return zero,
	       so don't use it.  */
	    if (num_chars > 1
		|| (num_chars == 1 && token_buffer[1] != '\0'))
	      {
		wchar_t wc;
		(void) mbtowc (NULL, NULL, 0);
		if (mbtowc (& wc, token_buffer + 1, num_chars) == num_chars)
		  result = wc;
		else
		  warning ("Ignoring invalid multibyte character");
	      }
#endif
	    yylval.ttype = build_int_2 (result, 0);
	    TREE_TYPE (yylval.ttype) = wchar_type_node;
	  }

	value = CONSTANT;
	break;
      }

    case '"':
    string_constant:
      {
	register char *p;

	c = getch ();
	p = token_buffer + 1;

	while (c != '"' && c >= 0)
	  {
	    /* ignore_escape_flag is set for reading the filename in #line.  */
	    if (!ignore_escape_flag && c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto skipnewline;
		if (!wide_flag
		    && TYPE_PRECISION (char_type_node) < HOST_BITS_PER_INT
		    && c >= ((unsigned) 1 << TYPE_PRECISION (char_type_node)))
		  warning ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ANSI C++ forbids newline in string constant");
		lineno++;
	      }

	    if (p == token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    *p++ = c;

	  skipnewline:
	    c = getch ();
	    if (c == EOF) {
		error("Unterminated string");
		break;
	    }
	  }
	*p = 0;

	/* We have read the entire constant.
	   Construct a STRING_CST for the result.  */

	if (wide_flag)
	  {
	    /* If this is a L"..." wide-string, convert the multibyte string
	       to a wide character string.  */
	    char *widep = (char *) alloca ((p - token_buffer) * WCHAR_BYTES);
	    int len;

#ifdef MULTIBYTE_CHARS
	    len = mbstowcs ((wchar_t *) widep, token_buffer + 1, p - token_buffer);
	    if (len < 0 || len >= (p - token_buffer))
	      {
		warning ("Ignoring invalid multibyte string");
		len = 0;
	      }
	    bzero (widep + (len * WCHAR_BYTES), WCHAR_BYTES);
#else
	    {
	      union { long l; char c[sizeof (long)]; } u;
	      int big_endian;
	      char *wp, *cp;

	      /* Determine whether host is little or big endian.  */
	      u.l = 1;
	      big_endian = u.c[sizeof (long) - 1];
	      wp = widep + (big_endian ? WCHAR_BYTES - 1 : 0);

	      bzero (widep, (p - token_buffer) * WCHAR_BYTES);
	      for (cp = token_buffer + 1; cp < p; cp++)
		*wp = *cp, wp += WCHAR_BYTES;
	      len = p - token_buffer - 1;
	    }
#endif
	    yylval.ttype = build_string ((len + 1) * WCHAR_BYTES, widep);
	    TREE_TYPE (yylval.ttype) = wchar_array_type_node;
	  }
	else
	  {
	    yylval.ttype = build_string (p - token_buffer, token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = char_array_type_node;
	  }

	*p++ = '"';
	*p = 0;

	value = STRING; break;
      }

    case '+':
    case '-':
    case '&':
    case '|':
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

	token_buffer[1] = c1 = getch ();
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
	    }
	else if ((c == '-') && (c1 == '>'))
	  {
	    nextchar = getch ();
	    if (nextchar == '*')
	      {
		nextchar = -1;
		value = POINTSAT_STAR;
	      }
	    else
	      value = POINTSAT;
	    goto done;
	  }
	else if (c1 == '?' && (c == '<' || c == '>'))
	  {
	    token_buffer[3] = 0;

	    c1 = getch ();
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
		nextchar = c1;
	      }
	    if (pedantic)
	      pedwarn ("use of `operator %s' is not standard C++",
		       token_buffer);
	    goto done;
	  }
	/* digraphs */
	else if (c == '<' && c1 == '%')
	  { value = '{'; goto done; }
	else if (c == '<' && c1 == ':')
	  { value = '['; goto done; }
	else if (c == '%' && c1 == '>')
	  { value = '}'; goto done; }
	else if (c == '%' && c1 == ':')
	  { value = '#'; goto done; }

	nextchar = c1;
	token_buffer[1] = 0;

	value = c;
	goto done;
      }

    case ':':
      c = getch ();
      if (c == ':')
	{
	  token_buffer[1] = ':';
	  token_buffer[2] = '\0';
	  value = SCOPE;
	  yylval.itype = 1;
	}
      else if (c == '>')
	{
	  value = ']';
	  goto done;
	}
      else
	{
	  nextchar = c;
	  value = ':';
	}
      break;

    case 0:
      /* Don't make yyparse think this is eof.  */
      value = 1;
      break;

    case '(':
      /* try, weakly, to handle casts to pointers to functions.  */
      nextchar = skip_white_space (getch ());
      if (nextchar == '*')
	{
	  int next_c = skip_white_space (getch ());
	  if (next_c == ')')
	    {
	      nextchar = -1;
	      yylval.ttype = build1 (INDIRECT_REF, 0, 0);
	      value = PAREN_STAR_PAREN;
	    }
	  else
	    {
	      put_back (next_c);
	      value = c;
	    }
	}
      else if (nextchar == ')')
	{
	  nextchar = -1;
	  yylval.ttype = NULL_TREE;
	  value = LEFT_RIGHT;
	}
      else value = c;
      break;

    default:
      value = c;
    }

done:
/*  yylloc.last_line = lineno; */
#ifdef GATHER_STATISTICS
  token_count[value] += 1;
#endif

  return value;
}

typedef enum
{
  d_kind, t_kind, s_kind, r_kind, e_kind, c_kind,
  id_kind, op_id_kind, perm_list_kind, temp_list_kind,
  vec_kind, x_kind, lang_decl, lang_type, all_kinds
} tree_node_kind;
extern int tree_node_counts[];
extern int tree_node_sizes[];
extern char *tree_node_kind_names[];

/* Place to save freed lang_decls which were allocated on the
   permanent_obstack.  @@ Not currently used.  */
tree free_lang_decl_chain;

tree
build_lang_decl (code, name, type)
     enum tree_code code;
     tree name;
     tree type;
{
  register tree t = build_decl (code, name, type);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;
  else
    /* Could be that saveable is permanent and current is not.  */
    obstack = &permanent_obstack;

  if (free_lang_decl_chain && obstack == &permanent_obstack)
    {
      pi = (int *)free_lang_decl_chain;
      free_lang_decl_chain = TREE_CHAIN (free_lang_decl_chain);
    }
  else
    pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl));

  while (i > 0)
    pi[--i] = 0;

  DECL_LANG_SPECIFIC (t) = (struct lang_decl *) pi;
  LANG_DECL_PERMANENT ((struct lang_decl *) pi)
    = obstack == &permanent_obstack;
  my_friendly_assert (LANG_DECL_PERMANENT ((struct lang_decl *) pi)
	  == TREE_PERMANENT  (t), 234);
  DECL_MAIN_VARIANT (t) = t;
  if (current_lang_name == lang_name_cplusplus)
    {
      DECL_LANGUAGE (t) = lang_cplusplus;
#if 0
#ifndef NO_AUTO_OVERLOAD
      if (code == FUNCTION_DECL && name != 0
	  && ! (IDENTIFIER_LENGTH (name) == 4
		&& IDENTIFIER_POINTER (name)[0] == 'm'
		&& strcmp (IDENTIFIER_POINTER (name), "main") == 0)
	  && ! (IDENTIFIER_LENGTH (name) > 10
		&& IDENTIFIER_POINTER (name)[0] == '_'
		&& IDENTIFIER_POINTER (name)[1] == '_'
		&& strncmp (IDENTIFIER_POINTER (name)+2, "builtin_", 8) == 0))
	TREE_OVERLOADED (name) = 1;
#endif
#endif
    }
  else if (current_lang_name == lang_name_c)
    DECL_LANGUAGE (t) = lang_c;
  else my_friendly_abort (64);

#if 0 /* not yet, should get fixed properly later */
  if (code == TYPE_DECL)
    {
      tree id;
      id = get_identifier (build_overload_name (type, 1, 1));
      DECL_ASSEMBLER_NAME (t) = id;
    }

#endif
#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += sizeof(struct lang_decl);
#endif

  return t;
}

tree
build_lang_field_decl (code, name, type)
     enum tree_code code;
     tree name;
     tree type;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register tree t = build_decl (code, name, type);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl_flags) / sizeof (int);
  register int *pi;
#if 0 /* not yet, should get fixed properly later */

  if (code == TYPE_DECL)
    {
      tree id;
      id = get_identifier (build_overload_name (type, 1, 1));
      DECL_ASSEMBLER_NAME (t) = id;
    }
#endif

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;
  else
    my_friendly_assert (obstack == &permanent_obstack, 235);

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl_flags));
  while (i > 0)
    pi[--i] = 0;

  DECL_LANG_SPECIFIC (t) = (struct lang_decl *) pi;
  return t;
}

void
copy_lang_decl (node)
     tree node;
{
  int size;
  int *pi;

  if (TREE_CODE (node) == FIELD_DECL)
    size = sizeof (struct lang_decl_flags);
  else
    size = sizeof (struct lang_decl);
  pi = (int *)obstack_alloc (&permanent_obstack, size);
  bcopy ((char *)DECL_LANG_SPECIFIC (node), (char *)pi, size);
  DECL_LANG_SPECIFIC (node) = (struct lang_decl *)pi;
}

tree
make_lang_type (code)
     enum tree_code code;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register tree t = make_node (code);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_type) / sizeof (int);
  register int *pi;

  /* Set up some flags that give proper default behavior.  */
  IS_AGGR_TYPE (t) = 1;

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;
  else
    my_friendly_assert (obstack == &permanent_obstack, 236);

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_type));
  while (i > 0)
    pi[--i] = 0;

  TYPE_LANG_SPECIFIC (t) = (struct lang_type *) pi;
  CLASSTYPE_AS_LIST (t) = build_tree_list (NULL_TREE, t);
  SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, interface_unknown);
  CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
  CLASSTYPE_VBASE_SIZE (t) = integer_zero_node;
  TYPE_BINFO (t) = make_binfo (integer_zero_node, t, NULL_TREE, NULL_TREE,
			       NULL_TREE);
  CLASSTYPE_BINFO_AS_LIST (t) = build_tree_list (NULL_TREE, TYPE_BINFO (t));

  /* Make sure this is laid out, for ease of use later.
     In the presence of parse errors, the normal was of assuring
     this might not ever get executed, so we lay it out *immediately*.  */
  build_pointer_type (t);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_type] += 1;
  tree_node_sizes[(int)lang_type] += sizeof(struct lang_type);
#endif

  return t;
}

void
copy_decl_lang_specific (decl)
     tree decl;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register int *old = (int *)DECL_LANG_SPECIFIC (decl);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (decl))
    obstack = saveable_obstack;
  else
    my_friendly_assert (obstack == &permanent_obstack, 237);

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl));
  while (i-- > 0)
    pi[i] = old[i];

  DECL_LANG_SPECIFIC (decl) = (struct lang_decl *) pi;

#ifdef GATHER_STATISTICS
  tree_node_counts[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += sizeof(struct lang_decl);
#endif
}

void
dump_time_statistics ()
{
  register tree prev = 0, decl, next;
  int this_time = my_get_run_time ();
  TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time))
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
      IDENTIFIER_GLOBAL_VALUE (decl) = prev;
      prev = decl;
    }

  for (decl = prev; decl; decl = IDENTIFIER_GLOBAL_VALUE (decl))
    print_time (IDENTIFIER_POINTER (decl),
		TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (decl)));
}

void
compiler_error (s, v, v2)
     char *s;
     HOST_WIDE_INT v, v2;			/* @@also used as pointer */
{
  char buf[1024];
  sprintf (buf, s, v, v2);
  error_with_file_and_line (input_filename, lineno, "%s (compiler error)", buf);
}

void
compiler_error_with_decl (decl, s)
     tree decl;
     char *s;
{
  char *name;
  count_error (0);

  report_error_function (0);

  if (TREE_CODE (decl) == PARM_DECL)
    fprintf (stderr, "%s:%d: ",
	     DECL_SOURCE_FILE (DECL_CONTEXT (decl)),
	     DECL_SOURCE_LINE (DECL_CONTEXT (decl)));
  else
    fprintf (stderr, "%s:%d: ",
	     DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  name = lang_printable_name (decl);
  if (name)
    fprintf (stderr, s, name);
  else
    fprintf (stderr, s, "((anonymous))");
  fprintf (stderr, " (compiler error)\n");
}

void
yyerror (string)
     char *string;
{
  extern int end_of_file;
  char buf[200];

  strcpy (buf, string);

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  if (end_of_file)
    strcat (buf, input_redirected ()
	    ? " at end of saved text"
	    : " at end of input");
  else if (token_buffer[0] == 0)
    strcat (buf, " at null character");
  else if (token_buffer[0] == '"')
    strcat (buf, " before string constant");
  else if (token_buffer[0] == '\'')
    strcat (buf, " before character constant");
  else if (token_buffer[0] < 040 || (unsigned char) token_buffer[0] >= 0177)
    sprintf (buf + strlen (buf), " before character 0%o",
	     (unsigned char) token_buffer[0]);
  else
    strcat (buf, " before `%s'");

  error (buf, token_buffer);
}

#ifdef HANDLE_SYSV_PRAGMA

/* Handle a #pragma directive.  INPUT is the current input stream,
   and C is a character to reread.  Processes the entire input line
   and returns a character for the caller to reread: either \n or EOF.  */

/* This function has to be in this file, in order to get at
   the token types.  */

handle_sysv_pragma ()
{
  for (;;)
    {
      switch (yylex ())
	{
	case IDENTIFIER:
	case TYPENAME:
	case STRING:
	case CONSTANT:
	  handle_pragma_token ("ignored", yylval.ttype);
	  break;
	case '(':
	  handle_pragma_token ("(", NULL_TREE);
	  break;
	case ')':
	  handle_pragma_token (")", NULL_TREE);
	  break;
	case ',':
	  handle_pragma_token (",", NULL_TREE);
	  break;
	case '=':
	  handle_pragma_token ("=", NULL_TREE);
	  break;
	case LEFT_RIGHT:
	  handle_pragma_token ("(", NULL_TREE);
	  handle_pragma_token (")", NULL_TREE);
	  break;
	case END_OF_LINE:
	  handle_pragma_token (NULL_PTR, NULL_TREE);
	  return;
	default:
	  handle_pragma_token (NULL_PTR, NULL_TREE);
	  while (yylex () != END_OF_LINE)
	    /* continue */;
	  return;
	}
    }
}
#endif /* HANDLE_SYSV_PRAGMA */
