/* Lexical analyzer for GNU CHILL. -*- C -*-
   Copyright (C) 1992, 93, 1994, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	 General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include <setjmp.h>
#include <sys/stat.h>

#include "tree.h"
#include "input.h"

#include "lex.h"
#include "ch-tree.h"
#include "flags.h"
#include "parse.h"
#include "obstack.h"
#include "toplev.h"

#ifdef DWARF_DEBUGGING_INFO
#include "dwarfout.h"
#endif

#ifdef MULTIBYTE_CHARS
#include <locale.h>
#endif

/* include the keyword recognizers */
#include "hash.h"

FILE* finput;

#if 0
static int	last_token = 0;
/* Sun's C compiler warns about the safer sequence 
   do { .. } while 0 
   when there's a 'return' inside the braces, so don't use it */
#define RETURN_TOKEN(X) { last_token = X; return (X); }
#endif

/* This is set non-zero to force incoming tokens to lowercase. */
extern int ignore_case;

extern int module_number;
extern int serious_errors;

/* This is non-zero to recognize only uppercase special words. */
extern int special_UC;

extern struct obstack permanent_obstack;
extern struct obstack temporary_obstack;

/* forward declarations */
static void close_input_file         PROTO((char *));
static tree convert_bitstring        PROTO((char *));
static tree convert_integer          PROTO((char *));
static void maybe_downcase           PROTO((char *));
static int  maybe_number             PROTO((char *));
static tree equal_number             PROTO((void));
static void handle_use_seizefile_directive PROTO((int));
static int  handle_name		     PROTO((tree));
static char *readstring              PROTO((int, int *));
static void read_directive	     PROTO((void));
static tree read_identifier	     PROTO((int));
static tree read_number              PROTO((int));
static void skip_c_comment           PROTO((void));
static void skip_line_comment        PROTO((void));
static int  skip_whitespace          PROTO((void));
static tree string_or_char           PROTO((int, char *));

/* next variables are public, because ch-actions uses them */

/* the default grantfile name, set by lang_init */
tree default_grant_file = 0;

/* These tasking-related variables are NULL at the start of each 
   compiler pass, and are set to an expression tree if and when
   a compiler directive is parsed containing an expression.
   The NULL state is significant;  it means 'no user-specified
   signal_code (or whatever) has been parsed'. */

/* process type, set by <> PROCESS_TYPE = number <> */
tree process_type = NULL_TREE;

/* send buffer default priority,
   set by <> SEND_BUFFER_DEFAULT_PRIORITY = number <> */
tree send_buffer_prio = NULL_TREE;

/* send signal default priority,
   set by <> SEND_SIGNAL_DEFAULT_PRIORITY = number <> */
tree send_signal_prio = NULL_TREE;

/* signal code, set by <> SIGNAL_CODE = number <> */
tree signal_code = NULL_TREE;

/* flag for range checking */
int range_checking = 1;

/* flag for NULL pointer checking */
int empty_checking = 1;

/* flag to indicate making all procedure local variables
   to be STATIC */
int all_static_flag = 0;

/* flag to indicate -fruntime-checking command line option.
   Needed for initializing range_checking and empty_checking
   before pass 2 */
int runtime_checking_flag = 1;

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree ridpointers[(int) RID_MAX];

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag = 0;

static int maxtoken;		/* Current nominal length of token buffer.  */
char *token_buffer;	/* Pointer to token buffer.
			   Actual allocated length is maxtoken + 2.
			   This is not static because objc-parse.y uses it.  */

/* implement yylineno handling for flex */
#define yylineno lineno

static int inside_c_comment = 0;

static int saw_eol = 0; /* 1 if we've just seen a '\n' */
static int saw_eof = 0; /* 1 if we've just seen an EOF */

typedef struct string_list
  {
    struct string_list *next;
    char               *str;
  } STRING_LIST;

/* list of paths specified on the compiler command line by -L options. */
static STRING_LIST *seize_path_list = (STRING_LIST *)0;

/* List of seize file names.  Each TREE_VALUE is an identifier
   (file name) from a <>USE_SEIZE_FILE<> directive.
   The TREE_PURPOSE is non-NULL if a USE_SEIZE_FILE directive has been
   written to the grant file. */
static tree files_to_seize     = NULL_TREE;
/* Last node on files_to_seize list. */
static tree last_file_to_seize = NULL_TREE;
/* Pointer into files_to_seize list:  Next unparsed file to read. */
static tree next_file_to_seize = NULL_TREE;

/* The most recent use_seize_file directive. */
tree use_seizefile_name = NULL_TREE;

/* If non-NULL, the name of the seizefile we're currently processing. */
tree current_seizefile_name = NULL_TREE;

/* called to reset for pass 2 */
static void
ch_lex_init ()
{
  current_seizefile_name = NULL_TREE;

  lineno = 0;

  saw_eol = 0;
  saw_eof = 0;
  /* Initialize these compiler-directive variables. */
  process_type     = NULL_TREE;
  send_buffer_prio = NULL_TREE;
  send_signal_prio = NULL_TREE;
  signal_code      = NULL_TREE;
  all_static_flag  = 0;
  /* reinitialize rnage checking and empty checking */
  range_checking = runtime_checking_flag;
  empty_checking = runtime_checking_flag;
}


char *
init_parse (filename)
     char *filename;
{
  int lowercase_standard_names = ignore_case || ! special_UC;

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

  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;

  /* Initialize these compiler-directive variables. */
  process_type     = NULL_TREE;
  send_buffer_prio = NULL_TREE;
  send_signal_prio = NULL_TREE;
  signal_code      = NULL_TREE;

  maxtoken         = 40;
  token_buffer     = xmalloc ((unsigned)(maxtoken + 2));

  init_chill_expand ();

#define ENTER_STANDARD_NAME(RID, LOWER, UPPER) \
  ridpointers[(int) RID] = \
    get_identifier (lowercase_standard_names ? LOWER : UPPER)

  ENTER_STANDARD_NAME (RID_ALL,		"all",		"ALL");
  ENTER_STANDARD_NAME (RID_ASSERTFAIL,	"assertfail",	"ASSERTFAIL");
  ENTER_STANDARD_NAME (RID_ASSOCIATION,	"association",	"ASSOCIATION");
  ENTER_STANDARD_NAME (RID_BIN,         "bin",          "BIN");
  ENTER_STANDARD_NAME (RID_BOOL,	"bool",		"BOOL");
  ENTER_STANDARD_NAME (RID_BOOLS,	"bools",	"BOOLS");
  ENTER_STANDARD_NAME (RID_BYTE,	"byte",		"BYTE");
  ENTER_STANDARD_NAME (RID_CHAR,	"char",		"CHAR");
  ENTER_STANDARD_NAME (RID_DOUBLE,	"double",	"DOUBLE");
  ENTER_STANDARD_NAME (RID_DURATION,    "duration",     "DURATION");
  ENTER_STANDARD_NAME (RID_DYNAMIC,	"dynamic",	"DYNAMIC");
  ENTER_STANDARD_NAME (RID_ELSE,	"else",		"ELSE");
  ENTER_STANDARD_NAME (RID_EMPTY,	"empty",	"EMPTY");
  ENTER_STANDARD_NAME (RID_FALSE,	"false",	"FALSE");
  ENTER_STANDARD_NAME (RID_FLOAT,	"float",	"FLOAT");
  ENTER_STANDARD_NAME (RID_GENERAL,	"general",	"GENERAL");
  ENTER_STANDARD_NAME (RID_IN,		"in",		"IN");
  ENTER_STANDARD_NAME (RID_INLINE,	"inline",	"INLINE");
  ENTER_STANDARD_NAME (RID_INOUT,	"inout",	"INOUT");
  ENTER_STANDARD_NAME (RID_INSTANCE,	"instance",	"INSTANCE");
  ENTER_STANDARD_NAME (RID_INT,		"int",		"INT");
  ENTER_STANDARD_NAME (RID_LOC,		"loc",		"LOC");
  ENTER_STANDARD_NAME (RID_LONG,	"long",		"LONG");
  ENTER_STANDARD_NAME (RID_LONG_REAL,	"long_real",	"LONG_REAL");
  ENTER_STANDARD_NAME (RID_NULL,	"null",		"NULL");
  ENTER_STANDARD_NAME (RID_OUT,		"out",		"OUT");
  ENTER_STANDARD_NAME (RID_OVERFLOW,	"overflow",	"OVERFLOW");
  ENTER_STANDARD_NAME (RID_PTR,		"ptr",		"PTR");
  ENTER_STANDARD_NAME (RID_READ,	"read",		"READ");
  ENTER_STANDARD_NAME (RID_REAL,	"real",		"REAL");
  ENTER_STANDARD_NAME (RID_RANGE,	"range",	"RANGE");
  ENTER_STANDARD_NAME (RID_RANGEFAIL,	"rangefail",	"RANGEFAIL");
  ENTER_STANDARD_NAME (RID_RECURSIVE,	"recursive",	"RECURSIVE");
  ENTER_STANDARD_NAME (RID_SHORT,	"short",	"SHORT");
  ENTER_STANDARD_NAME (RID_SIMPLE,	"simple",	"SIMPLE");
  ENTER_STANDARD_NAME (RID_TIME,        "time",         "TIME");
  ENTER_STANDARD_NAME (RID_TRUE,	"true",		"TRUE");
  ENTER_STANDARD_NAME (RID_UBYTE,	"ubyte",	"UBYTE");
  ENTER_STANDARD_NAME (RID_UINT,	"uint",		"UINT");
  ENTER_STANDARD_NAME (RID_ULONG,	"ulong",	"ULONG");
  ENTER_STANDARD_NAME (RID_UNSIGNED,	"unsigned",	"UNSIGNED");
  ENTER_STANDARD_NAME (RID_USHORT,	"ushort",	"USHORT");
  ENTER_STANDARD_NAME (RID_VOID,	"void",		"VOID");

  return filename;
}

void
finish_parse ()
{
  if (finput != NULL)
    fclose (finput);
}

static int yywrap ();

#define YY_PUTBACK_SIZE 5
#define YY_BUF_SIZE 1000

static char yy_buffer[YY_PUTBACK_SIZE + YY_BUF_SIZE];
static char *yy_cur = yy_buffer + YY_PUTBACK_SIZE;
static char *yy_lim = yy_buffer + YY_PUTBACK_SIZE;

int yy_refill ()
{
  char *buf = yy_buffer + YY_PUTBACK_SIZE;
  int c, result;
  bcopy (yy_cur - YY_PUTBACK_SIZE, yy_buffer, YY_PUTBACK_SIZE);
  yy_cur = buf;

 retry:
  if (saw_eof)
    {
      if (yywrap ())
	return EOF;
      saw_eof = 0;
      goto retry;
    }

  result = 0;
  while (saw_eol)
    {
      c = check_newline ();
      if (c == EOF)
        {
	  saw_eof = 1;
	  goto retry;
	}
      else if (c != '\n')
	{
	  saw_eol = 0;
	  buf[result++] = c;
	}
    }
  
  while (result < YY_BUF_SIZE)
    {
      c = getc(finput);
      if (c == EOF)
        {
	  saw_eof = 1;
	  break;
	}
      buf[result++] = c;
      
      /* Because we might switch input files on a compiler directive
	 (that end with '>', don't read past a '>', just in case. */
      if (c == '>')
	break;
      
      if (c == '\n')
	{
#ifdef YYDEBUG
	  extern int yydebug;
	  if (yydebug)
            fprintf (stderr, "-------------------------- finished Line %d\n",
		     yylineno);
#endif
	  saw_eol = 1;
	  break;
	}
    }

  yy_lim = yy_cur + result;

  return yy_lim > yy_cur ? *yy_cur++ : EOF;
}

#define input() (yy_cur < yy_lim ? *yy_cur++ : yy_refill ())

#define unput(c) (*--yy_cur = (c))


int starting_pass_2 = 0;

int
yylex ()
{
  int nextc;
  int len;
  char* tmp;
  int base;
  int ch;
 retry:
  ch = input ();
  if (starting_pass_2)
    {
      starting_pass_2 = 0;
      unput (ch);
      return END_PASS_1;
    }
  switch (ch)
    {
    case ' ': case '\t': case '\n': case '\f': case '\b': case '\v': case '\r':
      goto retry;
    case '[':
      return LPC;
    case ']':
      return RPC;
    case '{':
      return LC;
    case '}':
      return RC;
    case '(':
      nextc = input ();
      if (nextc == ':')
	return LPC;
      unput (nextc);
      return LPRN;
    case ')':
      return RPRN;
    case ':':
      nextc = input ();
      if (nextc == ')')
	return RPC;
      else if (nextc == '=')
	return ASGN;
      unput (nextc);
      return COLON;
    case ',':
      return COMMA;
    case ';':
      return SC;
    case '+':
      return PLUS;
    case '-':
      nextc = input ();
      if (nextc == '>')
	return ARROW;
      if (nextc == '-')
	{
	  skip_line_comment ();
	  goto retry;
	}
      unput (nextc);
      return SUB;
    case '*':
      return MUL;
    case '=':
      return EQL;
    case '/':
      nextc = input ();
      if (nextc == '/')
	return CONCAT;
      else if (nextc == '=')
	return NE;
      else if (nextc == '*')
	{
	  skip_c_comment ();
	  goto retry;
	}
      unput (nextc);
      return DIV;
    case '<':
      nextc = input ();
      if (nextc == '=')
	return LTE;
      if (nextc == '>')
	{
	  read_directive ();
	  goto retry;
	}
      unput (nextc);
      return LT;
    case '>':
      nextc = input ();
      if (nextc == '=')
	return GTE;
      unput (nextc);
      return GT;

    case 'D': case 'd':
      base = 10;
      goto maybe_digits;
    case 'B': case 'b':
      base = 2;
      goto maybe_digits;
    case 'H': case 'h':
      base = 16;
      goto maybe_digits;
    case 'O': case 'o':
      base = 8;
      goto maybe_digits;
    case 'C': case 'c':
      nextc = input ();
      if (nextc == '\'')
	{
	  int byte_val = 0;
	  char *start;
	  int len = 0;  /* Number of hex digits seen. */
	  for (;;)
	    {
	      ch = input ();
	      if (ch == '\'')
		break;
	      if (ch == '_')
		continue;
	      if (!ISXDIGIT (ch))           /* error on non-hex digit */
		{
		  if (pass == 1)
		    error ("invalid C'xx' ");
		  break;
		}
	      if (ch >= 'a')
		ch -= ' ';
	      ch -= '0';
	      if (ch > 9)
		ch -= 7;
	      byte_val *= 16;
	      byte_val += (int)ch;

	      if (len & 1) /* collected two digits, save byte */
		obstack_1grow (&temporary_obstack, (char) byte_val);
	      len++;
	    }
	  start = obstack_finish (&temporary_obstack);
	  yylval.ttype = string_or_char (len >> 1, start);
	  obstack_free (&temporary_obstack, start);
	  return len == 2 ? SINGLECHAR : STRING;
	}
      unput (nextc);
      goto letter;

    maybe_digits:
      nextc = input ();
      if (nextc == '\'')
	{
	  char *start;
	  obstack_1grow (&temporary_obstack, ch);
	  obstack_1grow (&temporary_obstack, nextc);
	  for (;;)
	    {
	      ch = input ();
	      if (ISALNUM (ch))
		obstack_1grow (&temporary_obstack, ch);
	      else if (ch != '_')
		break;
	    }
	  obstack_1grow (&temporary_obstack, '\0');
	  start = obstack_finish (&temporary_obstack);
	  if (ch != '\'')
	    {
	      unput (ch);
	      yylval.ttype = convert_integer (start); /* Pass base? */
	      return NUMBER;
	    }
	  else
	    {
	      yylval.ttype = convert_bitstring (start);
	      return BITSTRING;
	    }
	}
      unput (nextc);
      goto letter;

    case 'A':                                   case 'E':
    case 'F':  case 'G':             case 'I':  case 'J':
    case 'K':  case 'L':  case 'M':  case 'N':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':                                   case 'e':
    case 'f':  case 'g':             case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    letter:
      return handle_name (read_identifier (ch));
    case '\'':
      tmp = readstring ('\'', &len);
      yylval.ttype = string_or_char (len, tmp);
      free (tmp);
      return len == 1 ? SINGLECHAR : STRING;
    case '\"':
      tmp = readstring ('\"', &len);
      yylval.ttype = build_chill_string (len, tmp);
      free (tmp);
      return STRING;
    case '.':
      nextc = input ();
      unput (nextc);
      if (ISDIGIT (nextc)) /* || nextc == '_')  we don't start numbers with '_' */
	goto number;
      return DOT;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    number:
      yylval.ttype = read_number (ch);
      return TREE_CODE (yylval.ttype) == REAL_CST ? FLOATING : NUMBER;
    default:
      return ch;
    }
}

static void
close_input_file (fn)
  char *fn;
{
  if (finput == NULL)
    abort ();

  if (finput != stdin && fclose (finput) == EOF)
    {
      error ("can't close %s", fn);
      abort ();
    }
  finput = NULL;
}

/* Return an identifier, starting with FIRST and then reading
   more characters using input().  Return an IDENTIFIER_NODE. */

static tree
read_identifier (first)
     int first; /* First letter of identifier */
{
  tree id;
  char *start;
  for (;;)
    {
      obstack_1grow (&temporary_obstack, first);
      first = input ();
      if (first == EOF)
	break;
      if (! ISALNUM (first) && first != '_')
	{
	  unput (first);
	  break;
	}
    }
  obstack_1grow (&temporary_obstack, '\0');
  start = obstack_finish (&temporary_obstack);
  maybe_downcase (start);
  id = get_identifier (start);
  obstack_free (&temporary_obstack, start);
  return id;
}

/* Given an identifier ID, check to see if it is a reserved name,
   and return the appropriate token type. */

static int
handle_name (id)
     tree id;
{
  struct resword *tp;
  tp = in_word_set (IDENTIFIER_POINTER (id), IDENTIFIER_LENGTH (id));
  if (tp != NULL
      && special_UC == ISUPPER ((unsigned char) tp->name[0])
      && (tp->flags == RESERVED || tp->flags == PREDEF))
    {
      if (tp->rid != NORID)
	yylval.ttype = ridpointers[tp->rid];
      else if (tp->token == THIS)
	yylval.ttype = lookup_name (get_identifier ("__whoami"));
      return tp->token;
    }
  yylval.ttype = id;
  return NAME;
}

static tree
read_number (ch)
     int ch; /* Initial character */
{
  tree num;
  char *start;
  int is_float = 0;
  for (;;)
    {
      if (ch != '_')
	obstack_1grow (&temporary_obstack, ch);
      ch = input ();
      if (! ISDIGIT (ch) && ch != '_')
	break;
    }
  if (ch == '.')
    {
      do
	{
	  if (ch != '_')
	    obstack_1grow (&temporary_obstack, ch);
	  ch = input ();
	} while (ISDIGIT (ch) || ch == '_');
      is_float++;
    }
  if (ch == 'd' || ch == 'D' || ch == 'e' || ch == 'E')
    {
      /* Convert exponent indication [eEdD] to 'e'. */
      obstack_1grow (&temporary_obstack, 'e');
      ch = input ();
      if (ch == '+' || ch == '-')
	{
	  obstack_1grow (&temporary_obstack, ch);
	  ch = input ();
	}
      if (ISDIGIT (ch) || ch == '_')
	{
	  do
	    {
	      if (ch != '_')
		obstack_1grow (&temporary_obstack, ch);
	      ch = input ();
	    } while (ISDIGIT (ch) || ch == '_');
	}
      else
	{
	  error ("malformed exponent part of floating-point literal");
	}
      is_float++;
    }
  if (ch != EOF)
    unput (ch);
  obstack_1grow (&temporary_obstack, '\0');
  start = obstack_finish (&temporary_obstack);
  if (is_float)
    {
      REAL_VALUE_TYPE value;
      tree  type = double_type_node;
      errno = 0;
      value = REAL_VALUE_ATOF (start, TYPE_MODE (type));
      obstack_free (&temporary_obstack, start);
      if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	  && REAL_VALUE_ISINF (value) && pedantic)
	pedwarn ("real number exceeds range of REAL");
      num = build_real (type, value);
    }
  else
    num = convert_integer (start);
  CH_DERIVED_FLAG (num) = 1;
  return num;
}

/* Skip to the end of a compiler directive. */

static void
skip_directive ()
{
  int ch = input ();
  for (;;)
    {
      if (ch == EOF)
	{
	  error ("end-of-file in '<>' directive");
	  break;
	}
      if (ch == '\n')
	break;
      if (ch == '<')
	{
	  ch = input ();
	  if (ch == '>')
	    break;
	}
      ch = input ();
    }
  starting_pass_2 = 0;
}

/* Read a compiler directive.  ("<>{WS}" have already been read. ) */
static void
read_directive ()
{
  struct resword *tp;
  tree id;
  int ch = skip_whitespace();
  if (ISALPHA (ch) || ch == '_')
    id = read_identifier (ch);
  else if (ch == EOF)
    {
      error ("end-of-file in '<>' directive"); 
      to_global_binding_level (); 
      return;
    }
  else
    {
      warning ("unrecognized compiler directive");
      skip_directive ();
      return;
    }
  tp = in_word_set (IDENTIFIER_POINTER (id), IDENTIFIER_LENGTH (id));
  if (tp == NULL || special_UC != ISUPPER ((unsigned char) tp->name[0]))
    {
      if (pass == 1)
	warning ("unrecognized compiler directive `%s'",
		 IDENTIFIER_POINTER (id));
    }
  else
    switch (tp->token)
      {
      case ALL_STATIC_OFF:
	all_static_flag = 0;
	break;
      case ALL_STATIC_ON:
	all_static_flag = 1;
	break;
      case EMPTY_OFF:
	empty_checking = 0;
	break;
      case EMPTY_ON:
	empty_checking = 1;
	break;
      case IGNORED_DIRECTIVE:
	break;
      case PROCESS_TYPE_TOKEN:
	process_type = equal_number ();
	break;
      case RANGE_OFF:
	range_checking = 0;
	break;
      case RANGE_ON:
	range_checking = 1;
	break;
      case SEND_SIGNAL_DEFAULT_PRIORITY: 
	send_signal_prio = equal_number ();
	break;
      case SEND_BUFFER_DEFAULT_PRIORITY:
	send_buffer_prio = equal_number ();
	break;
      case SIGNAL_CODE:
	signal_code = equal_number ();
	break;
      case USE_SEIZE_FILE:
	handle_use_seizefile_directive (0);
	break;
      case USE_SEIZE_FILE_RESTRICTED:
	handle_use_seizefile_directive (1);
	break;
      default:
	if (pass == 1)
	  warning ("unrecognized compiler directive `%s'", 
		   IDENTIFIER_POINTER (id));
	break;
      }
  skip_directive ();
}


tree
build_chill_string (len, str)
    int   len;
    char  *str;
{
  tree t;

  push_obstacks (&permanent_obstack, &permanent_obstack);
  t = build_string (len, str);
  TREE_TYPE (t) = build_string_type (char_type_node, 
				     build_int_2 (len, 0));
  CH_DERIVED_FLAG (t) = 1;
  pop_obstacks ();
  return t;
}


static tree
string_or_char (len, str)
     int   len;
     char *str;
{
  tree result;
  
  push_obstacks (&permanent_obstack, &permanent_obstack);
  if (len == 1)
    {
      result = build_int_2 ((unsigned char)str[0], 0);
      CH_DERIVED_FLAG (result) = 1;
      TREE_TYPE (result) = char_type_node;
    }
  else
    result = build_chill_string (len, str);
  pop_obstacks ();
  return result;
}


static void
maybe_downcase (str)
    char        *str;
{
  if (! ignore_case)
    return;
  while (*str)
    {
      if (ISUPPER ((unsigned char) *str))
	*str = tolower ((unsigned char)*str);
      str++;
    }
}


static int
maybe_number (s)
  char	*s;
{
  char	fc;
  
  /* check for decimal number */
  if (*s >= '0' && *s <= '9')
    {
      while (*s)
	{
	  if (*s >= '0' && *s <= '9')
	    s++;
	  else
	    return 0;
	}
      return 1;
    }
  
  fc = *s;
  if (s[1] != '\'')
    return 0;
  s += 2;
  while (*s)
    {
      switch (fc)
	{
	case 'd':
	case 'D':
	  if (*s < '0' || *s > '9')
	    return 0;
	  break;
	case 'h':
	case 'H':
	  if (!ISXDIGIT ((unsigned char) *s))
	    return 0;
	  break;
	case 'b':
	case 'B':
	  if (*s < '0' || *s > '1')
	    return 0;
	  break;
	case 'o':
	case 'O':
	  if (*s < '0' || *s > '7')
	    return 0;
	  break;
	default:
	  return 0;
	}
      s++;
    }
  return 1;
}

static char *
readstring (terminator, len)
     char terminator;
     int *len;
{
  int      c;
  unsigned allocated = 1024;
  char    *tmp = xmalloc (allocated);
  unsigned i = 0;
  
  for (;;)
    {
      c = input ();
      if (c == terminator)
	{
	  if ((c = input ()) != terminator)
	    {
	      unput (c);
	      break;
	    }
	  else
	    c = terminator;
	}
      if (c == '\n' || c == EOF)
	  goto unterminated;
      if (c == '^')
	{
	  c = input();
	  if (c == EOF || c == '\n')
	    goto unterminated;
	  if (c == '^')
	    goto storeit;
	  if (c == '(')
	    {
	      int cc, count = 0;
	      int base = 10;
	      int next_apos = 0;
	      int check_base = 1;
	      c = 0;
	      while (1)
		{
		  cc = input ();
		  if (cc == terminator)
		    {
		      if (!(terminator == '\'' && next_apos))
			{
			  error ("unterminated control sequence");
			  serious_errors++;
			  goto done;
			}
		    }
		  if (cc == EOF || cc == '\n')
		    {
		      c = cc;
		      goto unterminated;
		    }
		  if (next_apos)
		    {
		      next_apos = 0;
		      if (cc != '\'')
			{
			  error ("invalid integer literal in control sequence");
			  serious_errors++;
			  goto done;
			}
		      continue;
		    }
		  if (cc == ' ' || cc == '\t')
		    continue;
		  if (cc == ')')
		    {
		      if ((c < 0 || c > 255) && (pass == 1))
			error ("control sequence overflow");
		      if (! count && pass == 1)
			error ("invalid control sequence");
		      break;
		    }
		  else if (cc == ',')
		    {
		      if ((c < 0 || c > 255) && (pass == 1))
			error ("control sequence overflow");
		      if (! count && pass == 1)
			error ("invalid control sequence");
		      tmp[i++] = c;
		      if (i == allocated)
			{
			  allocated += 1024;
			  tmp = xrealloc (tmp, allocated);
			}
		      c = count = 0;
		      base = 10;
		      check_base = 1;
		      continue;
		    }
		  else if (cc == '_')
		    {
		      if (! count && pass == 1)
			error ("invalid integer literal in control sequence");
		      continue;
		    }
		  if (check_base)
		    {
		      if (cc == 'D' || cc == 'd')
			{
			  base = 10;
			  next_apos = 1;
			}
		      else if (cc == 'H' || cc == 'h')
			{
			  base = 16;
			  next_apos = 1;
			}
		      else if (cc == 'O' || cc == 'o')
			{
			  base = 8;
			  next_apos = 1;
			}
		      else if (cc == 'B' || cc == 'b')
			{
			  base = 2;
			  next_apos = 1;
			}
		      check_base = 0;
		      if (next_apos)
			continue;
		    }
		  if (base == 2)
		    {
		      if (cc < '0' || cc > '1')
			cc = -1;
		      else
			cc -= '0';
		    }
		  else if (base == 8)
		    {
		      if (cc < '0' || cc > '8')
			cc = -1;
		      else
			cc -= '0';
		    }
		  else if (base == 10)
		    {
		      if (! ISDIGIT (cc))
			cc = -1;
		      else
			cc -= '0';
		    }
		  else if (base == 16)
		    {
		      if (!ISXDIGIT (cc))
			cc = -1;
		      else
			{
			  if (cc >= 'a')
			    cc -= ' ';
			  cc -= '0';
			  if (cc > 9)
			    cc -= 7;
			}
		    }
		  else
		    {
		      error ("invalid base in read control sequence");
		      abort ();
		    }
		  if (cc == -1)
		    {
		      /* error in control sequence */
		      if (pass == 1)
			error ("invalid digit in control sequence");
		      cc = 0;
		    }
		  c = (c * base) + cc;
		  count++;
		}
	    }
	  else
	    c ^= 64;
	}
    storeit:
      tmp[i++] = c;
      if (i == allocated)
	{
	  allocated += 1024;
	  tmp = xrealloc (tmp, allocated);
	}
    }
 done:
  tmp [*len = i] = '\0';
  return tmp;

unterminated:
  if (c == '\n')
    unput ('\n');
  *len = 1;
  if (pass == 1)
    error ("unterminated string literal");  
  to_global_binding_level ();
  tmp[0] = '\0';
  return tmp;
}

/* Convert an integer INTCHARS into an INTEGER_CST.
   INTCHARS is on the temporary_obstack, and is popped by this function. */

static tree
convert_integer (intchars)
     char *intchars;
{
#ifdef YYDEBUG
  extern int yydebug;
#endif
  char *p = intchars;
  char         *oldp = p;
  int		base = 10, tmp;
  int           valid_chars = 0;
  int		overflow = 0;
  tree		type;
  HOST_WIDE_INT val_lo = 0, val_hi = 0;
  tree		val;
  
  /* determine the base */
  switch (*p)
    {
    case 'd':
    case 'D':
      p += 2;
      break;
    case 'o':
    case 'O':
      p += 2;
      base = 8;
      break;
    case 'h':
    case 'H':
      p += 2;
      base = 16;
      break;
    case 'b':
    case 'B':
      p += 2;
      base = 2;
      break;
    default:
      if (!ISDIGIT (*p))   /* this test is for equal_number () */
	{
	  obstack_free (&temporary_obstack, intchars);
	  return 0;
	}
      break;
    }
  
  while (*p)
    {
      tmp = *p++;
      if ((tmp == '\'') || (tmp == '_'))
	continue;
      if (tmp < '0')
	goto bad_char;
      if (tmp >= 'a')      /* uppercase the char */
	tmp -= ' ';
      switch (base)        /* validate the characters */
	{
	case 2:
	  if (tmp > '1')
	    goto bad_char;
	  break;
	case 8:
	  if (tmp > '7')
	    goto bad_char;
	  break;
	case 10:
	  if (tmp > '9')
	    goto bad_char;
	  break;
	case 16:
	  if (tmp > 'F')
	    goto bad_char;
	  if (tmp > '9' && tmp < 'A')
	    goto bad_char;
	  break;
	default:
	  abort ();
	}
      tmp -= '0';
      if (tmp > 9)
	tmp -= 7;
      if (mul_double (val_lo, val_hi, base, 0, &val_lo, &val_hi))
	overflow++;
      add_double (val_lo, val_hi, tmp, 0, &val_lo, &val_hi);
      if (val_hi < 0)
	overflow++;
      valid_chars++;
    }
 bad_char:
  obstack_free (&temporary_obstack, intchars);
  if (!valid_chars)
    {
      if (pass == 2)
	error ("invalid number format `%s'", oldp);
      return 0;
    }
  val = build_int_2 (val_lo, val_hi);
  /* We set the type to long long (or long long unsigned) so that
     constant fold of literals is less likely to overflow.  */
  if (int_fits_type_p (val, long_long_integer_type_node))
    type = long_long_integer_type_node;
  else
    {
      if (! int_fits_type_p (val, long_long_unsigned_type_node))
	overflow++;
      type = long_long_unsigned_type_node;
    }
  TREE_TYPE (val) = type;
  CH_DERIVED_FLAG (val) = 1;
  
  if (overflow)
    error ("integer literal too big");

  return val;
}

/* Convert a bitstring literal on the temporary_obstack to
   a bitstring CONSTRUCTOR.  Free the literal from the obstack. */

static tree
convert_bitstring (p)
     char *p;
{
#ifdef YYDEBUG
  extern int yydebug;
#endif
  int bl = 0, valid_chars = 0, bits_per_char = 0, c, k;
  tree initlist = NULL_TREE;
  tree val;
  
  /* Move p to stack so we can re-use temporary_obstack for result. */
  char *oldp = (char*) alloca (strlen (p) + 1);
  if (oldp == 0) fatal ("stack space exhausted");
  strcpy (oldp, p);
  obstack_free (&temporary_obstack, p);
  p = oldp;
  
  switch (*p)
    {
    case 'h':
    case 'H':
      bits_per_char = 4;
      break;
    case 'o':
    case 'O':
      bits_per_char = 3;
      break;
    case 'b':
    case 'B':
      bits_per_char = 1;
      break;
    }
  p += 2;

  while (*p)
    {
      c = *p++;
      if (c == '_' || c == '\'')
	continue;
      if (c >= 'a')
	c -= ' ';
      c -= '0';
      if (c > 9)
	c -= 7;
      valid_chars++;
      
      for (k = BYTES_BIG_ENDIAN ? bits_per_char - 1 : 0;
	   BYTES_BIG_ENDIAN ? k >= 0 : k < bits_per_char;
	   bl++, BYTES_BIG_ENDIAN ? k-- : k++)
	{
	  if (c & (1 << k))
	    initlist = tree_cons (NULL_TREE, build_int_2 (bl, 0), initlist);
        }
    }
#if 0
  /* as long as BOOLS(0) is valid it must tbe possible to
     specify an empty bitstring */
  if (!valid_chars)
    {
      if (pass == 2)
	error ("invalid number format `%s'", oldp);
      return 0;
    }
#endif
  val = build (CONSTRUCTOR,
	       build_bitstring_type (size_int (bl)),
	       NULL_TREE, nreverse (initlist));
  TREE_CONSTANT (val) = 1;
  CH_DERIVED_FLAG (val) = 1;
  return val;
}

/* Check if two filenames name the same file.
   This is done by stat'ing both files and comparing their inodes.

   Note: we have to take care of seize_path_list. Therefore do it the same
   way as in yywrap. FIXME: This probably can be done better. */

static int
same_file (filename1, filename2)
     char *filename1;
     char *filename2;
{
  struct stat s[2];
  char        *fn_input[2];
  int         i, stat_status;
  
  if (grant_only_flag)
    /* do nothing in this case */
    return 0;

  /* if filenames are equal -- return 1, cause there is no need
     to search in the include list in this case */
  if (strcmp (filename1, filename2) == 0)
    return 1;
  
  fn_input[0] = filename1;
  fn_input[1] = filename2;

  for (i = 0; i < 2; i++)
    {
      stat_status = stat (fn_input[i], &s[i]);
      if (stat_status < 0 &&
	  strchr (fn_input[i], '/') == 0)
        {
	  STRING_LIST *plp;
	  char        *path;
	  
	  for (plp = seize_path_list; plp != 0; plp = plp->next)
	    {
	      path = (char *)xmalloc (strlen (fn_input[i]) +
				      strlen (plp->str) + 2);
	      sprintf (path, "%s/%s", plp->str, fn_input[i]);
	      stat_status = stat (path, &s[i]);
	      free (path);
	      if (stat_status >= 0)
	        break;
  	    }
        }
      if (stat_status < 0)
        pfatal_with_name (fn_input[i]);
  }
  return s[0].st_ino == s[1].st_ino && s[0].st_dev == s[1].st_dev;
}

/*
 * Note that simply appending included file names to a list in this
 * way completely eliminates the need for nested files, and the
 * associated book-keeping, since the EOF processing in the lexer
 * will simply process the files one at a time, in the order that the
 * USE_SEIZE_FILE directives were scanned.
 */
static void
handle_use_seizefile_directive (restricted)
    int restricted;
{
  tree seen;
  int   len;
  int   c = skip_whitespace ();
  char *use_seizefile_str = readstring (c, &len);

  if (pass > 1)
    return;

  if (c != '\'' && c != '\"')
    {
      error ("USE_SEIZE_FILE directive must be followed by string");
      return;
    }

  use_seizefile_name = get_identifier (use_seizefile_str);
  CH_USE_SEIZEFILE_RESTRICTED (use_seizefile_name) = restricted;
  
  if (!grant_only_flag)
    {
      /* If file foo.ch contains a <> use_seize_file "bar.grt" <>,
	 and file bar.ch contains a <> use_seize_file "foo.grt" <>,
	 then if we're compiling foo.ch, we will indirectly be
	 asked to seize foo.grt.  Don't. */
      extern char *grant_file_name;
      if (strcmp (use_seizefile_str, grant_file_name) == 0)
	return;

      /* Check if the file is already on the list. */
      for (seen = files_to_seize; seen != NULL_TREE; seen = TREE_CHAIN (seen))
	if (same_file (IDENTIFIER_POINTER (TREE_VALUE (seen)),
		       use_seizefile_str))
	  return;  /* Previously seen; nothing to do. */
    }

  /* Haven't been asked to seize this file yet, so add
     its name to the list. */
  {
    tree pl = perm_tree_cons (0, use_seizefile_name, NULL_TREE);
    if (files_to_seize == NULL_TREE)
      files_to_seize = pl;
    else
      TREE_CHAIN (last_file_to_seize) = pl;
    if (next_file_to_seize == NULL_TREE)
      next_file_to_seize = pl;
    last_file_to_seize = pl;
  }
}


/*
 * get input, convert to lower case for comparison
 */
int
getlc (file)
     FILE *file;
{
  register int c;

  c = getc (file);  
  if (ISUPPER (c) && ignore_case)
    c = tolower (c);
  return c;
}

#if defined HANDLE_PRAGMA
/* Local versions of these macros, that can be passed as function pointers.  */
static int
pragma_getc ()
{
  return getc (finput);
}

static void
pragma_ungetc (arg)
     int arg;
{
  ungetc (arg, finput);
}
#endif /* HANDLE_PRAGMA */

#ifdef HANDLE_GENERIC_PRAGMAS
/* Handle a generic #pragma directive.
   BUFFER contains the text we read after `#pragma'.  Processes the entire input
   line and return non-zero iff the pragma was successfully processed.  */

static int
handle_generic_pragma (buffer)
     char * buffer;
{
  register int c;

  for (;;)
    {
      char * buff;
      
      handle_pragma_token (buffer, NULL);

      c = getc (finput);

      while (c == ' ' || c == '\t')
	c = getc (finput);
      ungetc (c, finput);
      
      if (c == '\n' || c == EOF)
	return handle_pragma_token (NULL, NULL);

      /* Read the next word of the pragma into the buffer.  */
      buff = buffer;
      do
	{
	  * buff ++ = c;
	  c = getc (finput);
	}
      while (c != EOF && isascii (c) && ! isspace (c) && c != '\n'
	     && buff < buffer + 128); /* XXX shared knowledge about size of buffer.  */
      
      ungetc (c, finput);
      
      * -- buff = 0;
    }
}
#endif /* HANDLE_GENERIC_PRAGMAS */

/* At the beginning of a line, increment the line number and process
   any #-directive on this line.  If the line is a #-directive, read
   the entire line and return a newline.  Otherwise, return the line's
   first non-whitespace character.

   (Each language front end has a check_newline() function that is called
   from lang_init() for that language.  One of the things this function
   must do is read the first line of the input file, and if it is a #line
   directive, extract the filename from it and use it to initialize
   main_input_filename.  Proper generation of debugging information in
   the normal "front end calls cpp then calls cc1XXXX environment" depends
   upon this being done.) */

int
check_newline ()
{
  register int c;

  lineno++;

  /* Read first nonwhite char on the line.  */

  c = getc (finput);

  while (c == ' ' || c == '\t')
    c = getc (finput);

  if (c != '#' || inside_c_comment)
    {
      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Read first nonwhite char after the `#'.  */

  c = getc (finput);
  while (c == ' ' || c == '\t')
    c = getc (finput);

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma', `ident', `define', or `undef'.  */

  if (ISUPPER (c) && ignore_case)
    c = tolower (c);

  if (c >= 'a' && c <= 'z')
    {
      if (c == 'p')
	{
	  if (getlc (finput) == 'r'
	      && getlc (finput) == 'a'
	      && getlc (finput) == 'g'
	      && getlc (finput) == 'm'
	      && getlc (finput) == 'a'
	      && (c = getlc (finput), ISSPACE (c)))
	    {
#ifdef HANDLE_PRAGMA
	      static char buffer [128];
	      char * buff = buffer;

	      /* Read the pragma name into a buffer.  */
	      while (c = getlc (finput), ISSPACE (c))
		continue;
	      
	      do
		{
		  * buff ++ = c;
		  c = getlc (finput);
		}
	      while (c != EOF && ! ISSPACE (c) && c != '\n'
		     && buff < buffer + 128);

	      pragma_ungetc (c);
		
	      * -- buff = 0;
	      
	      if (HANDLE_PRAGMA (pragma_getc, pragma_ungetc, buffer))
		goto skipline;
#endif /* HANDLE_PRAGMA */
	      
#ifdef HANDLE_GENERIC_PRAGMAS
	      if (handle_generic_pragma (buffer))
		goto skipline;
#endif /* HANDLE_GENERIC_PRAGMAS */
	      
	      goto skipline;
	    }
	}

      else if (c == 'd')
	{
	  if (getlc (finput) == 'e'
	      && getlc (finput) == 'f'
	      && getlc (finput) == 'i'
	      && getlc (finput) == 'n'
	      && getlc (finput) == 'e'
	      && (c = getlc (finput), ISSPACE (c)))
	    {
#if 0 /*def DWARF_DEBUGGING_INFO*/
	      if (c != '\n'
		  && (debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_define (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'u')
	{
	  if (getlc (finput) == 'n'
	      && getlc (finput) == 'd'
	      && getlc (finput) == 'e'
	      && getlc (finput) == 'f'
	      && (c = getlc (finput), ISSPACE (c)))
	    {
#if 0 /*def DWARF_DEBUGGING_INFO*/
	      if (c != '\n'
		  && (debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_undef (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'l')
	{
	  if (getlc (finput) == 'i'
	      && getlc (finput) == 'n'
	      && getlc (finput) == 'e'
	      && ((c = getlc (finput)) == ' ' || c == '\t'))
	    goto linenum;
	}
#if 0
      else if (c == 'i')
	{
	  if (getlc (finput) == 'd'
	      && getlc (finput) == 'e'
	      && getlc (finput) == 'n'
	      && getlc (finput) == 't'
	      && ((c = getlc (finput)) == ' ' || c == '\t'))
	    {
	      /* #ident.  The pedantic warning is now in cccp.c.  */

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = getlc (finput);

	      /* If no argument, ignore the line.  */
	      if (c == '\n')
		return c;

	      ungetc (c, finput);
	      token = yylex ();
	      if (token != STRING
		  || TREE_CODE (yylval.ttype) != STRING_CST)
		{
		  error ("invalid #ident");
		  goto skipline;
		}

	      if (!flag_no_ident)
		{
#ifdef ASM_OUTPUT_IDENT
		  extern FILE *asm_out_file;
		  ASM_OUTPUT_IDENT (asm_out_file, TREE_STRING_POINTER (yylval.ttype));
#endif
		}

	      /* Skip the rest of this line.  */
	      goto skipline;
	    }
	}
#endif

      error ("undefined or invalid # directive");
      goto skipline;
    }

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = getlc (finput);

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == '\n')
    return c;

  /* Something follows the #; read a token.  */

  if (ISDIGIT(c))
    {
      int old_lineno = lineno;
      int used_up = 0;
      int l = 0;
      extern struct obstack permanent_obstack;

      do
	{
	  l = l * 10 + (c - '0'); /* FIXME Not portable */
	  c = getlc(finput);
	} while (ISDIGIT(c));
      /* subtract one, because it is the following line that
	 gets the specified number */

      l--;

      /* Is this the last nonwhite stuff on the line?  */
      c = getlc (finput);
      while (c == ' ' || c == '\t')
	c = getlc (finput);
      if (c == '\n')
	{
	  /* No more: store the line number and check following line.  */
	  lineno = l;
	  return c;
	}

      /* More follows: it must be a string constant (filename).  */

      /* Read the string constant, but don't treat \ as special.  */
      ignore_escape_flag = 1;
      ignore_escape_flag = 0;

      if (c != '\"')
	{
	  error ("invalid #line");
	  goto skipline;
	}

      for (;;)
	{
	  c = getc (finput);
	  if (c == EOF || c == '\n')
	    {
	      error ("invalid #line");
	      return c;
	    }
	  if (c == '\"')
	    {
	      obstack_1grow(&permanent_obstack, 0);
	      input_filename = obstack_finish (&permanent_obstack);
	      break;
	    }
	  obstack_1grow(&permanent_obstack, c);
	}

      lineno = l;

      /* Each change of file name
	 reinitializes whether we are now in a system header.  */
      in_system_header = 0;

      if (main_input_filename == 0)
	main_input_filename = input_filename;

      /* Is this the last nonwhite stuff on the line?  */
      c = getlc (finput);
      while (c == ' ' || c == '\t')
	c = getlc (finput);
      if (c == '\n')
	return c;

      used_up = 0;

      /* `1' after file name means entering new file.
	 `2' after file name means just left a file.  */

      if (ISDIGIT (c))
	{
	  if (c == '1')
	    {
	      /* Pushing to a new file.  */
	      struct file_stack *p
		= (struct file_stack *) xmalloc (sizeof (struct file_stack));
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

	      used_up = 1;
	    }
	  else if (c == '2')
	    {
	      /* Popping out of a file.  */
	      if (input_file_stack->next)
		{
		  struct file_stack *p = input_file_stack;
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

	      used_up = 1;
	    }
	}

      /* If we have handled a `1' or a `2',
	 see if there is another number to read.  */
      if (used_up)
	{
	  /* Is this the last nonwhite stuff on the line?  */
	  c = getlc (finput);
	  while (c == ' ' || c == '\t')
	    c = getlc (finput);
	  if (c == '\n')
	    return c;
	  used_up = 0;
	}

      /* `3' after file name means this is a system header file.  */

      if (c == '3')
	in_system_header = 1;
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  while (c != '\n' && c != EOF)
    c = getc (finput);
  return c;
}


tree
get_chill_filename ()
{
  return (build_chill_string (
            strlen (input_filename) + 1,  /* +1 to get a zero terminated string */
	      input_filename));
}

tree
get_chill_linenumber ()
{
  return build_int_2 ((HOST_WIDE_INT)lineno, 0);
}


/* Assuming '/' and '*' have been read, skip until we've
   read the terminating '*' and '/'. */

static void
skip_c_comment ()
{
  int c = input();
  int start_line = lineno;

  inside_c_comment++;
  for (;;)
    if (c == EOF)
      {
	error_with_file_and_line (input_filename, start_line,
				  "unterminated comment");
	break;
      }
    else if (c != '*')
      c = input();
    else if ((c = input ()) == '/')
      break;
  inside_c_comment--;
}


/* Assuming "--" has been read, skip until '\n'. */

static void
skip_line_comment ()
{
  for (;;)
    {
      int c = input ();

      if (c == EOF)
	return;
      if (c == '\n')
	break;
    }
  unput ('\n');
}


static int
skip_whitespace ()
{
  for (;;)
    {
      int c = input ();

      if (c == EOF)
	return c;
      if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v')
	continue;
      if (c == '/')
	{
	  c = input ();
	  if (c == '*')
	    {
	      skip_c_comment ();
	      continue;
	    }
	  else
	    {
	      unput (c);
	      return '/';
	    }
	}
      if (c == '-')
	{
	  c = input ();
	  if (c == '-')
	    {
	      skip_line_comment ();
	      continue;
	    }
	  else
	    {
	      unput (c);
	      return '-';
	    }
	}
      return c;
    }
}

/*
 * avoid recursive calls to yylex to parse the ' = digits' or
 * ' = SYNvalue' which are supposed to follow certain compiler
 * directives.  Read the input stream, and return the value parsed.
 */
         /* FIXME: overflow check in here */
         /* FIXME: check for EOF around here */
static tree
equal_number ()
{
  int      c, result;
  char    *tokenbuf;
  char    *cursor;
  tree     retval = integer_zero_node;
  
  c = skip_whitespace();
  if ((char)c != '=')
    {
      if (pass == 2)
	error ("missing `=' in compiler directive");
      return integer_zero_node;
    }
  c = skip_whitespace();

  /* collect token into tokenbuf for later analysis */
  while (TRUE)
    {
      if (ISSPACE (c) || c == '<')
	break;
      obstack_1grow (&temporary_obstack, c);
      c = input ();
    }
  unput (c);             /* put uninteresting char back */
  obstack_1grow (&temporary_obstack, '\0');        /* terminate token */
  tokenbuf = obstack_finish (&temporary_obstack);
  maybe_downcase (tokenbuf);

  if (*tokenbuf == '-')
    /* will fail in the next test */
    result = BITSTRING;
  else if (maybe_number (tokenbuf))
    {
      if (pass == 1)
	return integer_zero_node;
      push_obstacks_nochange ();
      end_temporary_allocation ();
      yylval.ttype = convert_integer (tokenbuf);
      tokenbuf = 0;  /* Was freed by convert_integer. */
      result = yylval.ttype ? NUMBER : 0;
      pop_obstacks ();
    }
  else
    result = 0;
  
  if (result  == NUMBER)
    {
      retval = yylval.ttype;
    }
  else if (result == BITSTRING)
    {
      if (pass == 1)
        error ("invalid value follows `=' in compiler directive");
      goto finish;
    }
  else /* not a number */
    {
      cursor = tokenbuf;
      c = *cursor;
      if (!ISALPHA (c) && c != '_')
	{
	  if (pass == 1)
	    error ("invalid value follows `=' in compiler directive");
	  goto finish;
	}

      for (cursor = &tokenbuf[1]; *cursor != '\0'; cursor++)
	if (ISALPHA ((unsigned char) *cursor) || *cursor == '_' ||
	    ISDIGIT (*cursor))
	  continue;
	else
	  {
	    if (pass == 1)
	      error ("invalid `%c' character in name", *cursor);
	    goto finish;
	  }
      if (pass == 1)
	goto finish;
      else
	{
	  tree value = lookup_name (get_identifier (tokenbuf));
	  if (value == NULL_TREE
	      || TREE_CODE (value) != CONST_DECL
	      || TREE_CODE (DECL_INITIAL (value)) != INTEGER_CST)
	    {
	      if (pass == 2)
		error ("`%s' not integer constant synonym ",
		       tokenbuf);
	      goto finish;
	    }
	  obstack_free (&temporary_obstack, tokenbuf);
	  tokenbuf = 0;
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  retval = convert (chill_taskingcode_type_node, DECL_INITIAL (value));
	  pop_obstacks ();
	}
    }

  /* check the value */
  if (TREE_CODE (retval) != INTEGER_CST)
    {
      if (pass == 2)
	error ("invalid value follows `=' in compiler directive");
    }
  else if (TREE_INT_CST_HIGH (retval) != 0 ||
	   TREE_INT_CST_LOW (retval) > TREE_INT_CST_LOW (TYPE_MAX_VALUE (chill_unsigned_type_node)))
    {
      if (pass == 2)
	error ("value out of range in compiler directive");
    }
 finish:
  if (tokenbuf)
    obstack_free (&temporary_obstack, tokenbuf);
  return retval;
}

/*
 * add a possible grant-file path to the list
 */
void
register_seize_path (path)
     char *path;
{
  int          pathlen = strlen (path);
  char        *new_path = (char *)xmalloc (pathlen + 1);
  STRING_LIST *pl     = (STRING_LIST *)xmalloc (sizeof (STRING_LIST));
    
  /* strip off trailing slash if any */
  if (path[pathlen - 1] == '/')
    pathlen--;

  memcpy (new_path, path, pathlen);
  pl->str  = new_path;
  pl->next = seize_path_list;
  seize_path_list = pl;
}


/* Used by decode_decl to indicate that a <> use_seize_file NAME <>
   directive has been written to the grantfile. */

void
mark_use_seizefile_written (name)
     tree name;
{
  tree node;

  for (node = files_to_seize;  node != NULL_TREE; node = TREE_CHAIN (node))
    if (TREE_VALUE (node) == name)
      {
	TREE_PURPOSE (node) = integer_one_node;
	break;
      }
}


static int
yywrap ()
{
  extern char *chill_real_input_filename;

  close_input_file (input_filename);

  use_seizefile_name = NULL_TREE;

  if (next_file_to_seize && !grant_only_flag)
    {
      FILE *grt_in = NULL;
      char *seizefile_name_chars
	= IDENTIFIER_POINTER (TREE_VALUE (next_file_to_seize));

      /* find a seize file, open it.  If it's not at the path the
       * user gave us, and that path contains no slashes, look on
       * the seize_file paths, specified by the '-I' options.
       */     
      grt_in = fopen (seizefile_name_chars, "r");
      if (grt_in == NULL 
	  && strchr (seizefile_name_chars, '/') == NULL)
	{
	  STRING_LIST *plp;
	  char      *path;

	  for (plp = seize_path_list; plp != NULL; plp = plp->next)
	    {
	      path = (char *)xmalloc (strlen (seizefile_name_chars)
				      + strlen (plp->str) + 2);

	      sprintf (path, "%s/%s", plp->str, seizefile_name_chars);
	      grt_in = fopen (path, "r");
	      if (grt_in == NULL)
		free (path);
	      else
		{
		  seizefile_name_chars = path;
		  break;
		}
	    }
	}

      if (grt_in == NULL)
	pfatal_with_name (seizefile_name_chars);

      finput = grt_in;
      input_filename = seizefile_name_chars;

      lineno = 0;
      current_seizefile_name = TREE_VALUE (next_file_to_seize);

      next_file_to_seize = TREE_CHAIN (next_file_to_seize);

      saw_eof = 0;
      return 0;
    }

  if (pass == 1)
    {
      next_file_to_seize = files_to_seize;
      current_seizefile_name = NULL_TREE;

      if (strcmp (main_input_filename, "stdin"))
	finput = fopen (chill_real_input_filename, "r");
      else
	finput = stdin;
      if (finput == NULL)
	{
	  error ("can't reopen %s", chill_real_input_filename);
	  return 1;
	}
      input_filename = main_input_filename;
      ch_lex_init ();
      lineno = 0;
      /* Read a line directive if there is one.  */
      ungetc (check_newline (), finput);
      starting_pass_2 = 1;
      saw_eof = 0;
      if (module_number == 0)
	warning ("no modules seen");
      return 0;
    }
  return 1;
}
