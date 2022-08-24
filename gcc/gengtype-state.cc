/* Gengtype persistent state serialization & de-serialization.
   Useful for gengtype in plugin mode.

   Copyright (C) 2010-2022 Free Software Foundation, Inc.

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
   <http://www.gnu.org/licenses/>.

   Contributed by Jeremie Salvucci <jeremie.salvucci@free.fr>
   and Basile Starynkevitch <basile@starynkevitch.net>
*/

#ifdef HOST_GENERATOR_FILE
#include "config.h"
#define GENERATOR_FILE 1
#else
#include "bconfig.h"
#endif
#include "system.h"
#include "errors.h"	/* For fatal.  */
#include "version.h"	/* For version_string & pkgversion_string.  */
#include "obstack.h"
#include "gengtype.h"



/* Gives the file location of a type, if any.  */
static inline struct fileloc*
type_lineloc (const_type_p ty)
{
  if (!ty)
    return NULL;
  switch (ty->kind)
    {
    case TYPE_NONE:
      gcc_unreachable ();
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_USER_STRUCT:
    case TYPE_UNDEFINED:
      return CONST_CAST (struct fileloc*, &ty->u.s.line);
    case TYPE_SCALAR:
    case TYPE_STRING:
    case TYPE_POINTER:
    case TYPE_ARRAY:
    case TYPE_CALLBACK:
      return NULL;
    default:
      gcc_unreachable ();
    }
}

/* The state file has simplistic lispy lexical tokens.  Its lexer gives
   a linked list of struct state_token_st, through the peek_state_token
   function.  Lexical tokens are consumed with next_state_tokens.  */


/* The lexical kind of each lispy token.  */
enum state_token_en
{
  STOK_NONE,                    /* Never used.  */
  STOK_INTEGER,                 /* Integer token.  */
  STOK_STRING,                  /* String token.  */
  STOK_LEFTPAR,                 /* Left opening parenthesis.  */
  STOK_RIGHTPAR,                /* Right closing parenthesis.  */
  STOK_NAME                     /* hash-consed name or identifier.  */
};


/* Structure and hash-table used to share identifiers or names.  */
struct state_ident_st
{
  /* TODO: We could improve the parser by reserving identifiers for
     state keywords and adding a keyword number for them.  That would
     mean adding another field in this state_ident_st struct.  */
  char stid_name[1];		/* actually bigger & null terminated */
};
static htab_t state_ident_tab;


/* The state_token_st structure is for lexical tokens in the read
   state file.  The stok_kind field discriminates the union.  Tokens
   are allocated by peek_state_token which calls read_a_state_token
   which allocate them.  Tokens are freed by calls to
   next_state_tokens.  Token are organized in a FIFO look-ahead queue
   filled by peek_state_token.  */
struct state_token_st
{
  enum state_token_en stok_kind;	/* the lexical kind
					   discriminates the stok_un
					   union  */
  int stok_line;			/* the line number */
  int stok_col;				/* the column number */
  const char *stok_file;		/* the file path */
  struct state_token_st *stok_next;	/* the next token in the
					   queue, when peeked */
  union		                        /* discriminated by stok_kind! */
  {
    int stok_num;			/* when STOK_INTEGER */
    char stok_string[1];		/* when STOK_STRING, actual size is
					   bigger and null terminated */
    struct state_ident_st *stok_ident;	/* when STOK_IDENT */
    void *stok_ptr;		        /* null otherwise */
  }
  stok_un;
};




#define NULL_STATE_TOKEN (struct state_token_st*)0

/* the state_token pointer contains the leftmost current token.  The
   tokens are organized in a linked queue, using stok_next, for token
   look-ahead.  */
struct state_token_st *state_token = NULL_STATE_TOKEN;

/* Used by the reading lexer.  */
static FILE *state_file;
static const char *state_path = NULL;
static int state_line = 0;
static long state_bol = 0;	/* offset of beginning of line */

/* A class for writing out s-expressions, keeping track of newlines and
   nested indentation.  */
class s_expr_writer
{
public:
  s_expr_writer ();

  void write_new_line ();
  void write_any_indent (int leading_spaces);

  void begin_s_expr (const char *tag);
  void end_s_expr ();

private:
  int m_indent_amount;
  int m_had_recent_newline;
}; // class s_expr_writer

/* A class for writing out "gtype.state".  */
class state_writer : public s_expr_writer
{
public:
  state_writer ();

private:
  void write_state_fileloc (struct fileloc *floc);
  void write_state_fields (pair_p fields);
  void write_state_a_string (const char *s);
  void write_state_string_option (options_p current);
  void write_state_type_option (options_p current);
  void write_state_nested_option (options_p current);
  void write_state_option (options_p current);
  void write_state_options (options_p opt);
  void write_state_lang_bitmap (lang_bitmap bitmap);
  void write_state_version (const char *version);
  void write_state_scalar_type (type_p current);
  void write_state_string_type (type_p current);
  void write_state_callback_type (type_p current);
  void write_state_undefined_type (type_p current);
  void write_state_struct_union_type (type_p current, const char *kindstr);
  void write_state_struct_type (type_p current);
  void write_state_user_struct_type (type_p current);
  void write_state_union_type (type_p current);
  void write_state_lang_struct_type (type_p current);
  void write_state_pointer_type (type_p current);
  void write_state_array_type (type_p current);
  void write_state_gc_used (enum gc_used_enum gus);
  void write_state_common_type_content (type_p current);
  void write_state_type (type_p current);
  void write_state_pair (pair_p current);
  int write_state_pair_list (pair_p list);
  void write_state_typedefs (void);
  void write_state_structures (void);
  void write_state_variables (void);
  void write_state_srcdir (void);
  void write_state_files_list (void);
  void write_state_languages (void);

  friend void write_state (const char *state_path);

private:
  /* Counter of written types.  */
  int m_state_written_type_count;
}; // class state_writer


/* class s_expr_writer's trivial constructor.  */
s_expr_writer::s_expr_writer ()
  : m_indent_amount (0),
    m_had_recent_newline (0)
{
}

/* Write a newline to the output file, merging adjacent newlines.  */
void
s_expr_writer::write_new_line (void)
{
  /* Don't add a newline if we've just had one.  */
  if (!m_had_recent_newline)
    {
      fprintf (state_file, "\n");
      m_had_recent_newline = 1;
    }
}

/* If we've just had a newline, write the indentation amount, potentially
   omitting some spaces.

   LEADING_SPACES exists to support code that writes strings with leading
   spaces (e.g " foo") which might occur within a line, or could be the first
   thing on a line.  By passing leading_spaces == 1, when such a string is the
   first thing on a line, write_any_indent () swallows the successive
   leading spaces into the indentation so that the "foo" begins at the expected
   column.  */
void
s_expr_writer::write_any_indent (int leading_spaces)
{
  int i;
  int amount = m_indent_amount - leading_spaces;
  if (m_had_recent_newline)
    for (i = 0; i < amount; i++)
      fprintf (state_file, " ");
  m_had_recent_newline = 0;
}

/* Write the beginning of a new s-expresion e.g. "(!foo "
   The writer automatically adds whitespace to show the hierarchical
   structure of the expressions, so each one starts on a new line,
   and any within it will be at an increased indentation level.  */
void
s_expr_writer::begin_s_expr (const char *tag)
{
  write_new_line ();
  write_any_indent (0);
  fprintf (state_file, "(!%s ", tag);
  m_indent_amount++;
}

/* Write out the end of an s-expression: any necssessary indentation,
   a closing parenthesis, and a new line.  */
void
s_expr_writer::end_s_expr (void)
{
  m_indent_amount--;
  write_any_indent (0);
  fprintf (state_file, ")");
  write_new_line ();
}


/* class state_writer's trivial constructor.  */
state_writer::state_writer ()
  : s_expr_writer (),
    m_state_written_type_count (0)
{
}


/* Fatal error messages when reading the state.  They are extremely
   unlikely, and only appear when this gengtype-state.cc file is buggy,
   or when reading a gengtype state which was not generated by the
   same version of gengtype or GCC.  */


/* Fatal message while reading state.  */
static void 
fatal_reading_state (struct state_token_st* tok, const char*msg)
{
  if (tok)
    fatal ("%s:%d:%d: Invalid state file; %s",
	   tok->stok_file, tok->stok_line, tok->stok_col, 
	   msg); 
  else
    fatal ("%s:%d: Invalid state file; %s", 
	   state_path, state_line, msg);
}


/* Fatal printf-like message while reading state.  This can't be a
   function, because there is no way to pass a va_arg to a variant of
   fatal.  */
#define fatal_reading_state_printf(Tok,Fmt,...) do {	\
    struct state_token_st* badtok = Tok;		\
    if (badtok)						\
      fatal ("%s:%d:%d: Invalid state file; " Fmt,	\
	      badtok->stok_file,			\
	      badtok->stok_line,			\
	      badtok->stok_col, __VA_ARGS__);		\
    else						\
      fatal ("%s:%d: Invalid state file; " Fmt,		\
	     state_path, state_line, __VA_ARGS__);	\
  } while (0)


/* Find or allocate an identifier in our name hash table.  */
static struct state_ident_st *
state_ident_by_name (const char *name, enum insert_option optins)
{
  void **slot = NULL;
  int namlen = 0;
  struct state_ident_st *stid = NULL;

  if (!name || !name[0])
    return NULL;

  slot = htab_find_slot (state_ident_tab, name, optins);
  if (!slot)
    return NULL;

  namlen = strlen (name);
  stid =
    (struct state_ident_st *) xmalloc (sizeof (struct state_ident_st) +
				       namlen);
  memset (stid, 0, sizeof (struct state_ident_st) + namlen);
  strcpy (stid->stid_name, name);
  *slot = stid;

  return stid;
}

/* Our token lexer is heavily inspired by MELT's lexer, and share some
   code with the file gcc/melt-runtime.c of the GCC MELT branch!  We
   really want the gengtype state to be easily parsable by MELT.  This
   is a usual lispy lexing routine, dealing with spaces and comments,
   numbers, parenthesis, names, strings.  */
static struct state_token_st *
read_a_state_token (void)
{
  int c = 0;
  long curoff = 0;
  struct state_token_st *tk = NULL;

 again: /* Read again, e.g. after a comment or spaces.  */
  c = getc (state_file);
  if (c == EOF)
    return NULL;

  /* Handle spaces, count lines.  */
  if (c == '\n')
    {
      state_line++;
      state_bol = curoff = ftell (state_file);
      goto again;
    };
  if (ISSPACE (c))
    goto again;
  /* Skip comments starting with semi-colon.  */
  if (c == ';')
    {	
      do
	{
	  c = getc (state_file);
	}
      while (c > 0 && c != '\n');
      if (c == '\n')
	{
	  state_line++;
	  state_bol = curoff = ftell (state_file);
	}
      goto again;
    };
  /* Read signed numbers.  */
  if (ISDIGIT (c) || c == '-' || c == '+')
    {				/* number */
      int n = 0;
      ungetc (c, state_file);
      curoff = ftell (state_file);
      if (fscanf (state_file, "%d", &n) <= 0)
	fatal_reading_state (NULL_STATE_TOKEN, "Lexical error in number");
      tk = XCNEW (struct state_token_st);
      tk->stok_kind = STOK_INTEGER;
      tk->stok_line = state_line;
      tk->stok_col = curoff - state_bol;
      tk->stok_file = state_path;
      tk->stok_next = NULL;
      tk->stok_un.stok_num = n;

      return tk;
    }
  /* Read an opening left parenthesis.  */
  else if (c == '(')
    {
      curoff = ftell (state_file);
      tk = XCNEW (struct state_token_st);
      tk->stok_kind = STOK_LEFTPAR;
      tk->stok_line = state_line;
      tk->stok_col = curoff - state_bol;
      tk->stok_file = state_path;
      tk->stok_next = NULL;

      return tk;
    }
  /* Read an closing right parenthesis.  */
  else if (c == ')')
    {
      curoff = ftell (state_file);
      tk = XCNEW (struct state_token_st);
      tk->stok_kind = STOK_RIGHTPAR;
      tk->stok_line = state_line;
      tk->stok_col = curoff - state_bol;
      tk->stok_file = state_path;
      tk->stok_next = NULL;

      return tk;
    }
  /* Read identifiers, using an obstack.  */
  else if (ISALPHA (c) || c == '_' || c == '$' || c == '!' || c == '#')
    {
      struct obstack id_obstack;
      struct state_ident_st *sid = NULL;
      char *ids = NULL;
      obstack_init (&id_obstack);
      curoff = ftell (state_file);
      while (ISALNUM (c) || c == '_' || c == '$' || c == '!' || c == '#')
	{
	  obstack_1grow (&id_obstack, c);
	  c = getc (state_file);
	  if (c < 0)
	    break;
	};
      if (c >= 0)
	ungetc (c, state_file);
      obstack_1grow (&id_obstack, (char) 0);
      ids = XOBFINISH (&id_obstack, char *);
      sid = state_ident_by_name (ids, INSERT);
      obstack_free (&id_obstack, NULL);
      ids = NULL;
      tk = XCNEW (struct state_token_st);
      tk->stok_kind = STOK_NAME;
      tk->stok_line = state_line;
      tk->stok_col = curoff - state_bol;
      tk->stok_file = state_path;
      tk->stok_next = NULL;
      tk->stok_un.stok_ident = sid;

      return tk;
    }
  /* Read a string, dealing with escape sequences a la C! */
  else if (c == '"')
    {
      char *cstr = NULL;
      int cslen = 0;
      struct obstack bstring_obstack;
      obstack_init (&bstring_obstack);
      curoff = ftell (state_file);
      while ((c = getc (state_file)) != '"' && c >= 0)
	{
	  if (ISPRINT (c) && c != '\\')
	    obstack_1grow (&bstring_obstack, (char) c);
	  else if (ISSPACE (c) && c != '\n')
	    obstack_1grow (&bstring_obstack, (char) c);
	  else if (c == '\\')
	    {
	      c = getc (state_file);
	      switch (c)
		{
		case 'a':
		  obstack_1grow (&bstring_obstack, '\a');
		  break;
		case 'b':
		  obstack_1grow (&bstring_obstack, '\b');
		  break;
		case 't':
		  obstack_1grow (&bstring_obstack, '\t');
		  break;
		case 'n':
		  obstack_1grow (&bstring_obstack, '\n');
		  break;
		case 'v':
		  obstack_1grow (&bstring_obstack, '\v');
		  break;
		case 'f':
		  obstack_1grow (&bstring_obstack, '\f');
		  break;
		case 'r':
		  obstack_1grow (&bstring_obstack, '\r');
		  break;
		case '"':
		  obstack_1grow (&bstring_obstack, '\"');
		  break;
		case '\\':
		  obstack_1grow (&bstring_obstack, '\\');
		  break;
		case ' ':
		  obstack_1grow (&bstring_obstack, ' ');
		  break;
		case 'x':
		  {
		    unsigned int cx = 0;
		    if (fscanf (state_file, "%02x", &cx) > 0 && cx > 0)
		      obstack_1grow (&bstring_obstack, cx);
		    else
		      fatal_reading_state
			(NULL_STATE_TOKEN,
			 "Lexical error in string hex escape");
		    getc (state_file);
		    break;
		  }
		default:
		  fatal_reading_state
		    (NULL_STATE_TOKEN,
		     "Lexical error - unknown string escape");
		}
	    }
	  else
	    fatal_reading_state (NULL_STATE_TOKEN, "Lexical error...");
	};
      if (c != '"')
	fatal_reading_state (NULL_STATE_TOKEN, "Unterminated string");
      obstack_1grow (&bstring_obstack, '\0');
      cstr = XOBFINISH (&bstring_obstack, char *);
      cslen = strlen (cstr);
      tk = (struct state_token_st *)
	xcalloc (sizeof (struct state_token_st) + cslen, 1);
      tk->stok_kind = STOK_STRING;
      tk->stok_line = state_line;
      tk->stok_col = curoff - state_bol;
      tk->stok_file = state_path;
      tk->stok_next = NULL;
      strcpy (tk->stok_un.stok_string, cstr);
      obstack_free (&bstring_obstack, NULL);

      return tk;
    }
  /* Got an unexpected character.  */
  fatal_reading_state_printf
    (NULL_STATE_TOKEN,
     "Lexical error at offset %ld - bad character \\%03o = '%c'",
     ftell (state_file), c, c);
}

/* Used for lexical look-ahead.  Retrieves the lexical token of rank
   DEPTH, starting with 0 when reading the state file.  Gives null on
   end of file.  */
static struct state_token_st *
peek_state_token (int depth)
{
  int remdepth = depth;
  struct state_token_st **ptoken = &state_token;
  struct state_token_st *tok = NULL;

  while (remdepth >= 0)
    {
      if (*ptoken == NULL)
	{
	  *ptoken = tok = read_a_state_token ();
	  if (tok == NULL)
	    return NULL;
	}
      tok = *ptoken;
      ptoken = &((*ptoken)->stok_next);
      remdepth--;
    }

  return tok;
}

/* Consume the next DEPTH tokens and free them.  */
static void
next_state_tokens (int depth)
{
  struct state_token_st *n;

  while (depth > 0)
    {
      if (state_token != NULL)
	{
	  n = state_token->stok_next;
	  free (state_token);
	  state_token = n;
	}
      else
	fatal_reading_state (NULL_STATE_TOKEN, "Tokens stack empty");

      depth--;
    }
}

/* Safely retrieve the lexical kind of a token.  */
static inline enum state_token_en
state_token_kind (struct state_token_st *p)
{
  if (p == NULL)
    return STOK_NONE;
  else
    return p->stok_kind;
}

/* Test if a token is a given name i.e. an identifier.  */
static inline bool
state_token_is_name (struct state_token_st *p, const char *name)
{
  if (p == NULL)
    return false;

  if (p->stok_kind != STOK_NAME)
    return false;

  return !strcmp (p->stok_un.stok_ident->stid_name, name);
}


/* Following routines are useful for serializing datas.
 *
 * We want to serialize :
 *          - typedefs list
 *          - structures list
 *          - variables list
 *
 * So, we have one routine for each kind of data.  The main writing
 * routine is write_state.  The main reading routine is
 * read_state.  Most writing routines write_state_FOO have a
 * corresponding reading routine read_state_FOO.  Reading is done in a
 * recursive descending way, and any read error is fatal.
 */

/* When reading the state, we need to remember the previously seen
   types by their state_number, since GTY-ed types are usually
   shared.  */
static htab_t state_seen_types;

/* Return the length of a linked list made of pairs.  */
static int pair_list_length (pair_p list);

/* Compute the length of a list of pairs, starting from the first
   one.  */
static int
pair_list_length (pair_p list)
{
  int nbpair = 0;
  pair_p l = NULL;
  for (l = list; l; l = l->next)
    nbpair++;
  return nbpair;
}

/* Write a file location.  Files relative to $(srcdir) are quite
   frequent and are handled specially.  This ensures that two gengtype
   state file-s produced by gengtype on the same GCC source tree are
   very similar and can be reasonably compared with diff, even if the
   two GCC source trees have different absolute paths.  */
void
state_writer::write_state_fileloc (struct fileloc *floc)
{

  if (floc != NULL && floc->line > 0)
    {
      const char *srcrelpath = NULL;
      gcc_assert (floc->file != NULL);
      /* Most of the files are inside $(srcdir) so it is worth to
         handle them specially.  */
      srcrelpath = get_file_srcdir_relative_path (floc->file);
      if (srcrelpath != NULL)
	{
	  begin_s_expr ("srcfileloc");
	  write_state_a_string (srcrelpath);
	}
      else
	{
	  begin_s_expr ("fileloc");
	  write_state_a_string (get_input_file_name (floc->file));
	}
      fprintf (state_file, " %d", floc->line);
      end_s_expr ();
    }
  else
    fprintf (state_file, "nil ");
}

/* Write a list of fields.  */
void
state_writer::write_state_fields (pair_p fields)
{
  int nbfields = pair_list_length (fields);
  int nbpairs = 0;
  begin_s_expr ("fields");
  fprintf (state_file, "%d ", nbfields);
  nbpairs = write_state_pair_list (fields);
  gcc_assert (nbpairs == nbfields);
  end_s_expr ();
}

/* Write a null-terminated string in our lexical convention, very
   similar to the convention of C.  */
void
state_writer::write_state_a_string (const char *s)
{
  char c;

  write_any_indent (1);

  fputs (" \"", state_file);
  for (; *s != 0; s++)
    {
      c = *s;
      switch (c)
	{
	case '\a':
	  fputs ("\\a", state_file);
	  break;
	case '\b':
	  fputs ("\\b", state_file);
	  break;
	case '\t':
	  fputs ("\\t", state_file);
	  break;
	case '\n':
	  fputs ("\\n", state_file);
	  break;
	case '\v':
	  fputs ("\\v", state_file);
	  break;
	case '\f':
	  fputs ("\\f", state_file);
	  break;
	case '\r':
	  fputs ("\\r", state_file);
	  break;
	case '\"':
	  fputs ("\\\"", state_file);
	  break;
	case '\\':
	  fputs ("\\\\", state_file);
	  break;
	default:
	  if (ISPRINT (c))
	    putc (c, state_file);
	  else
	    fprintf (state_file, "\\x%02x", (unsigned) c);
	}
    }
  fputs ("\"", state_file);
}

/* Our option-s have three kinds, each with its writer.  */
void
state_writer::write_state_string_option (options_p current)
{
  write_any_indent (0);
  fprintf (state_file, "string ");
  if (current->info.string != NULL)
    write_state_a_string (current->info.string);
  else
    fprintf (state_file, " nil ");
}

void
state_writer::write_state_type_option (options_p current)
{
  write_any_indent (0);
  fprintf (state_file, "type ");
  write_state_type (current->info.type);
}

void
state_writer::write_state_nested_option (options_p current)
{
  write_any_indent (0);
  fprintf (state_file, "nested ");
  write_state_type (current->info.nested->type);
  if (current->info.nested->convert_from != NULL)
    write_state_a_string (current->info.nested->convert_from);
  else
    {
      write_any_indent (1);
      fprintf (state_file, " nil ");
    }

  if (current->info.nested->convert_to != NULL)
    write_state_a_string (current->info.nested->convert_to);
  else
    {
      write_any_indent (1);
      fprintf (state_file, " nil ");
    }
}

void
state_writer::write_state_option (options_p current)
{
  begin_s_expr ("option");

  write_any_indent (0);
  if (current->name != NULL)
    fprintf (state_file, "%s ", current->name);
  else
    fprintf (state_file, "nil ");

  switch (current->kind)
    {
    case OPTION_STRING:
      write_state_string_option (current);
      break;
    case OPTION_TYPE:
      write_state_type_option (current);
      break;
    case OPTION_NESTED:
      write_state_nested_option (current);
      break;
    default:
      fatal ("Option tag unknown");
    }

  /* Terminate the "option" s-expression.  */
  end_s_expr ();
}



/* Write a list of GTY options.  */
void
state_writer::write_state_options (options_p opt)
{
  options_p current;

  if (opt == NULL)
    {
	write_any_indent (0);
	fprintf (state_file, "nil ");
      return;
    }

  begin_s_expr ("options");
  for (current = opt; current != NULL; current = current->next)
      write_state_option (current);
  end_s_expr ();
}


/* Write a bitmap representing a set of GCC front-end languages.  */
void
state_writer::write_state_lang_bitmap (lang_bitmap bitmap)
{
  write_any_indent (0);
  fprintf (state_file, "%d ", (int) bitmap);
}

/* Write version information.  */
void
state_writer::write_state_version (const char *version)
{
  begin_s_expr ("version");
  write_state_a_string (version);
  end_s_expr ();
}

/* Write a scalar type.  We have only two of these.  */
void
state_writer::write_state_scalar_type (type_p current)
{
  write_any_indent (0);
  if (current == &scalar_nonchar)
    fprintf (state_file, "scalar_nonchar ");
  else if (current == &scalar_char)
    fprintf (state_file, "scalar_char ");
  else
    fatal ("Unexpected type in write_state_scalar_type");

  write_state_common_type_content (current);
}

/* Write the string type.  There is only one such thing! */
void
state_writer::write_state_string_type (type_p current)
{
  if (current == &string_type)
    {
      write_any_indent (0);
      fprintf (state_file, "string ");
      write_state_common_type_content (current);
    }
  else
    fatal ("Unexpected type in write_state_string_type");
}

/* Write the callback type.  There is only one such thing! */
void
state_writer::write_state_callback_type (type_p current)
{
  if (current == &callback_type)
    {
      write_any_indent (0);
      fprintf (state_file, "callback ");
      write_state_common_type_content (current);
    }
  else
    fatal ("Unexpected type in write_state_callback_type");
}

/* Write an undefined type.  */
void
state_writer::write_state_undefined_type (type_p current)
{
  DBGPRINTF ("undefined type @ %p #%d '%s'", (void *) current,
	     current->state_number, current->u.s.tag);
  write_any_indent (0);
  fprintf (state_file, "undefined ");
  gcc_assert (current->gc_used == GC_UNUSED);
  write_state_common_type_content (current);
  if (current->u.s.tag != NULL)
    write_state_a_string (current->u.s.tag);
  else
    {
      write_any_indent (0);
      fprintf (state_file, "nil");
    }

  write_state_fileloc (type_lineloc (current));
}


/* Common code to write structure like types.  */
void
state_writer::write_state_struct_union_type (type_p current,
					     const char *kindstr)
{
  DBGPRINTF ("%s type @ %p #%d '%s'", kindstr, (void *) current,
	     current->state_number, current->u.s.tag);
  write_any_indent (0);
  fprintf (state_file, "%s ", kindstr);
  write_state_common_type_content (current);
  if (current->u.s.tag != NULL)
    write_state_a_string (current->u.s.tag);
  else
    {
      write_any_indent (0);
      fprintf (state_file, "nil");
    }

  write_state_fileloc (type_lineloc (current));
  write_state_fields (current->u.s.fields);
  write_state_options (current->u.s.opt);
  write_state_lang_bitmap (current->u.s.bitmap);
}


/* Write a GTY struct type.  */
void
state_writer::write_state_struct_type (type_p current)
{
  write_state_struct_union_type (current, "struct");
  write_state_type (current->u.s.lang_struct);
  write_state_type (current->u.s.base_class);
}

/* Write a GTY user-defined struct type.  */
void
state_writer::write_state_user_struct_type (type_p current)
{
  DBGPRINTF ("user_struct type @ %p #%d '%s'", (void *) current,
	     current->state_number, current->u.s.tag);
  write_any_indent (0);
  fprintf (state_file, "user_struct ");
  write_state_common_type_content (current);
  if (current->u.s.tag != NULL)
    write_state_a_string (current->u.s.tag);
  else
    {
      write_any_indent (0);
      fprintf (state_file, "nil");
    }
  write_state_fileloc (type_lineloc (current));
  write_state_fields (current->u.s.fields);
}

/* write a GTY union type.  */
void
state_writer::write_state_union_type (type_p current)
{
  write_state_struct_union_type (current, "union");
  write_state_type (current->u.s.lang_struct);
}

/* Write a lang_struct type.  This is tricky and was painful to debug,
   we deal with the next field specifically within their lang_struct
   subfield, which points to a linked list of homonumous types.
   Change this function with extreme care, see also
   read_state_lang_struct_type.  */
void
state_writer::write_state_lang_struct_type (type_p current)
{
  int nbhomontype = 0;
  type_p hty = NULL;
  const char *homoname = 0;
  write_state_struct_union_type (current, "lang_struct");
  /* lang_struct-ures are particularly tricky, since their
     u.s.lang_struct field gives a list of homonymous struct-s or
     union-s! */
  DBGPRINTF ("lang_struct @ %p #%d", (void *) current, current->state_number);
  for (hty = current->u.s.lang_struct; hty != NULL; hty = hty->next)
    {
      nbhomontype++;
      DBGPRINTF ("homonymous #%d hty @ %p #%d '%s'", nbhomontype,
		 (void *) hty, hty->state_number, hty->u.s.tag);
      /* Every member of the homonymous list should have the same tag.  */
      gcc_assert (union_or_struct_p (hty));
      gcc_assert (hty->u.s.lang_struct == current);
      if (!homoname)
	homoname = hty->u.s.tag;
      gcc_assert (strcmp (homoname, hty->u.s.tag) == 0);
    }
  begin_s_expr ("homotypes");
  fprintf (state_file, "%d", nbhomontype);
  for (hty = current->u.s.lang_struct; hty != NULL; hty = hty->next)
    write_state_type (hty);
  end_s_expr ();
}

/* Write a pointer type.  */
void
state_writer::write_state_pointer_type (type_p current)
{
  write_any_indent (0);
  fprintf (state_file, "pointer ");
  write_state_common_type_content (current);
  write_state_type (current->u.p);
}

/* Write an array type.  */
void
state_writer::write_state_array_type (type_p current)
{
  write_any_indent (0);
  fprintf (state_file, "array ");
  write_state_common_type_content (current);
  if (current->u.a.len != NULL)
    write_state_a_string (current->u.a.len);
  else
    {
      write_any_indent (1);
      fprintf (state_file, " nil");
    }

  write_any_indent (1);
  fprintf (state_file, " ");
  write_state_type (current->u.a.p);
}

/* Write the gc_used information.  */
void
state_writer::write_state_gc_used (enum gc_used_enum gus)
{
  write_any_indent (1);
  switch (gus)
    {
    case GC_UNUSED:
      fprintf (state_file, " gc_unused");
      break;
    case GC_USED:
      fprintf (state_file, " gc_used");
      break;
    case GC_MAYBE_POINTED_TO:
      fprintf (state_file, " gc_maybe_pointed_to");
      break;
    case GC_POINTED_TO:
      fprintf (state_file, " gc_pointed_to");
      break;
    default:
      gcc_unreachable ();
    }
}

/* Utility routine to write the common content of all types.  Notice
   that the next field is *not* written on purpose.  */
void
state_writer::write_state_common_type_content (type_p current)
{
  write_any_indent (0);
  fprintf (state_file, "%d ", current->state_number);
  /* We do not write the next type, because list of types are
     explicitly written.  However, lang_struct are special in that
     respect.  See function write_state_lang_struct_type for more.  */
  write_state_type (current->pointer_to);
  write_state_gc_used (current->gc_used);
}


/* The important and recursive routine writing GTY types as understood
   by gengtype.  Types which have a positive state_number have already
   been seen and written.  */
void
state_writer::write_state_type (type_p current)
{
  write_any_indent (0);
  if (current == NULL)
    {
      fprintf (state_file, "nil ");
      return;
    }

  begin_s_expr ("type");

  if (current->state_number > 0)
    {
      write_any_indent (0);
      fprintf (state_file, "already_seen %d", current->state_number);
    }
  else
    {
      m_state_written_type_count++;
      DBGPRINTF ("writing type #%d @%p old number %d", m_state_written_type_count,
		 (void *) current, current->state_number);
      current->state_number = m_state_written_type_count;
      switch (current->kind)
	{
	case TYPE_NONE:
	  gcc_unreachable ();
	case TYPE_UNDEFINED:
	  write_state_undefined_type (current);
	  break;
	case TYPE_STRUCT:
	  write_state_struct_type (current);
	  break;
	case TYPE_USER_STRUCT:
	  write_state_user_struct_type (current);
	  break;
	case TYPE_UNION:
	  write_state_union_type (current);
	  break;
	case TYPE_POINTER:
	  write_state_pointer_type (current);
	  break;
	case TYPE_ARRAY:
	  write_state_array_type (current);
	  break;
	case TYPE_LANG_STRUCT:
	  write_state_lang_struct_type (current);
	  break;
	case TYPE_SCALAR:
	  write_state_scalar_type (current);
	  break;
	case TYPE_STRING:
	  write_state_string_type (current);
	  break;
	case TYPE_CALLBACK:
	  write_state_callback_type (current);
	  break;
	}
    }

  /* Terminate the "type" s-expression.  */
  end_s_expr ();
}


/* Write a pair.  */
void
state_writer::write_state_pair (pair_p current)
{
  if (current == NULL)
    {
      write_any_indent (0);
      fprintf (state_file, "nil)");
      return;
    }

  begin_s_expr ("pair");

  if (current->name != NULL)
    write_state_a_string (current->name);
  else
    write_state_a_string ("nil");

  write_state_type (current->type);
  write_state_fileloc (&(current->line));
  write_state_options (current->opt);

  /* Terminate the "pair" s-expression.  */
  end_s_expr ();
}

/* Write a pair list and return the number of pairs written.  */
int
state_writer::write_state_pair_list (pair_p list)
{
  int nbpair = 0;
  pair_p current;

  for (current = list; current != NULL; current = current->next)
    {
      write_state_pair (current);
      nbpair++;
    }
  return nbpair;

}

/* When writing imported linked lists, like typedefs, structures, ... we count
   their length first and write it.  This eases the reading, and enables an
   extra verification on the number of actually read items.  */

/* Write our typedefs.  */
void
state_writer::write_state_typedefs (void)
{
  int nbtypedefs = pair_list_length (typedefs);
  int nbpairs = 0;
  begin_s_expr ("typedefs");
  fprintf (state_file, "%d", nbtypedefs);
  nbpairs = write_state_pair_list (typedefs);
  gcc_assert (nbpairs == nbtypedefs);
  end_s_expr ();
  if (verbosity_level >= 2)
    printf ("%s wrote %d typedefs\n", progname, nbtypedefs);
}

/* Write our structures.  */
void
state_writer::write_state_structures (void)
{
  int nbstruct = 0;
  type_p current;

  for (current = structures; current != NULL; current = current->next)
    nbstruct++;

  begin_s_expr ("structures");
  fprintf (state_file, "%d", nbstruct);

  for (current = structures; current != NULL; current = current->next)
    {
      write_new_line ();
      write_state_type (current);
    }

  /* Terminate the "structures" s-expression.  */
  end_s_expr ();
  if (verbosity_level >= 2)
    printf ("%s wrote %d structures in state\n", progname, nbstruct);
}

/* Write our variables.  */
void
state_writer::write_state_variables (void)
{
  int nbvars = pair_list_length (variables);
  int nbpairs = 0;
  begin_s_expr ("variables");
  fprintf (state_file, "%d", nbvars);
  nbpairs = write_state_pair_list (variables);
  gcc_assert (nbpairs == nbvars);
  end_s_expr ();
  if (verbosity_level >= 2)
    printf ("%s wrote %d variables.\n", progname, nbvars);
}

/* Write the source directory.  File locations within the source
   directory have been written specifically.  */
void
state_writer::write_state_srcdir (void)
{
  begin_s_expr ("srcdir");
  write_state_a_string (srcdir);
  end_s_expr ();
}

/* Count and write the list of our files.  */
void
state_writer::write_state_files_list (void)
{
  int i = 0;
  /* Write the list of files with their lang_bitmap.  */
  begin_s_expr ("fileslist");
  fprintf (state_file, "%d %d", (int) num_gt_files, (int) num_build_headers);
  for (i = 0; i < (int) num_gt_files; i++)
    {
      const char *cursrcrelpath = NULL;
      const input_file *curfil = gt_files[i];
      /* Most of the files are inside $(srcdir) so it is worth to
         handle them specially.  */
      cursrcrelpath = get_file_srcdir_relative_path (curfil);
      if (cursrcrelpath)
	{
	  begin_s_expr ("srcfile");
	  fprintf (state_file, "%d ", get_lang_bitmap (curfil));
	  write_state_a_string (cursrcrelpath);
	}
      else
	{
	  begin_s_expr ("file");
	  fprintf (state_file, "%d ", get_lang_bitmap (curfil));
	  write_state_a_string (get_input_file_name (curfil));
	}
      /* Terminate the inner s-expression (either "srcfile" or "file").   */
      end_s_expr ();
    }
  /* Terminate the "fileslist" s-expression.  */
  end_s_expr ();
}

/* Write the list of GCC front-end languages.  */
void
state_writer::write_state_languages (void)
{
  int i = 0;
  begin_s_expr ("languages");
  fprintf (state_file, "%d", (int) num_lang_dirs);
  for (i = 0; i < (int) num_lang_dirs; i++)
    {
      /* Languages names are identifiers, we expect only letters or
         underscores or digits in them.  In particular, C++ is not a
         valid language name, but cp is valid.  */
      fprintf (state_file, " %s", lang_dir_names[i]);
    }
  end_s_expr ();
}

/* Write the trailer.  */
static void
write_state_trailer (void)
{
  /* This test should probably catch IO errors like disk full...  */
  if (fputs ("\n(!endfile)\n", state_file) == EOF)
    fatal ("failed to write state trailer [%s]", xstrerror (errno));
}

/* The write_state routine is the only writing routine called by main
   in gengtype.cc.  To avoid messing the state if gengtype is
   interrupted or aborted, we write a temporary file and rename it
   after having written it in totality.  */
void
write_state (const char *state_path)
{
  long statelen = 0;
  time_t now = 0;
  char *temp_state_path = NULL;
  char tempsuffix[40];
  time (&now);

  /* We write a unique temporary file which is renamed when complete
   * only.  So even if gengtype is interrupted, the written state file
   * won't be partially written, since the temporary file is not yet
   * renamed in that case.  */
  memset (tempsuffix, 0, sizeof (tempsuffix));
  snprintf (tempsuffix, sizeof (tempsuffix) - 1, "-%ld-%d.tmp", (long) now,
	    (int) getpid ());
  temp_state_path = concat (state_path, tempsuffix, NULL);
  state_file = fopen (temp_state_path, "w");
  if (state_file == NULL)
    fatal ("Failed to open file %s for writing state: %s",
	   temp_state_path, xstrerror (errno));
  if (verbosity_level >= 3)
    printf ("%s writing state file %s temporarily in %s\n",
	    progname, state_path, temp_state_path);
  /* This is the first line of the state.  Perhaps the file utility
     could know about that, so don't change it often.  */
  fprintf (state_file, ";;;;@@@@ GCC gengtype state\n");
  /* Output a few comments for humans. */
  fprintf (state_file,
	   ";;; DON'T EDIT THIS FILE, since generated by GCC's gengtype\n");
  fprintf (state_file,
	   ";;; The format of this file is tied to a particular version of GCC.\n");
  fprintf (state_file,
	   ";;; Don't parse this file wihout knowing GCC gengtype internals.\n");
  fprintf (state_file,
	   ";;; This file should be parsed by the same %s which wrote it.\n",
	   progname);

  state_writer sw;

  /* The first non-comment significant line gives the version string.  */
  sw.write_state_version (version_string);
  sw.write_state_srcdir ();
  sw.write_state_languages ();
  sw.write_state_files_list ();
  sw.write_state_structures ();
  sw.write_state_typedefs ();
  sw.write_state_variables ();
  write_state_trailer ();
  statelen = ftell (state_file);
  if (ferror (state_file))
    fatal ("output error when writing state file %s [%s]",
	   temp_state_path, xstrerror (errno));
  if (fclose (state_file))
    fatal ("failed to close state file %s [%s]",
	   temp_state_path, xstrerror (errno));
  if (rename (temp_state_path, state_path))
    fatal ("failed to rename %s to state file %s [%s]", temp_state_path,
	   state_path, xstrerror (errno));
  free (temp_state_path);

  if (verbosity_level >= 1)
    printf ("%s wrote state file %s of %ld bytes with %d GTY-ed types\n",
	    progname, state_path, statelen, sw.m_state_written_type_count);

}

/** End of writing routines!  The corresponding reading routines follow.  **/



/* Forward declarations, since some read_state_* functions are
   recursive! */
static void read_state_fileloc (struct fileloc *line);
static void read_state_options (options_p *opt);
static void read_state_type (type_p *current);
static void read_state_pair (pair_p *pair);
/* Return the number of pairs actually read.  */
static int read_state_pair_list (pair_p *list);
static void read_state_fields (pair_p *fields);
static void read_state_common_type_content (type_p current);




/* Record into the state_seen_types hash-table a type which we are
   reading, to enable recursive or circular references to it.  */
static void
record_type (type_p type)
{
  void **slot;

  slot = htab_find_slot (state_seen_types, type, INSERT);
  gcc_assert (slot);

  *slot = type;
}

/* Read an already seen type.  */
static void
read_state_already_seen_type (type_p *type)
{
  struct state_token_st *t0 = peek_state_token (0);

  if (state_token_kind (t0) == STOK_INTEGER)
    {
      void **slot = NULL;
      struct type loctype = { TYPE_SCALAR, 0, 0, 0, GC_UNUSED, {0} };

      loctype.state_number = t0->stok_un.stok_num;
      slot = htab_find_slot (state_seen_types, &loctype, NO_INSERT);
      if (slot == NULL)
	{
	  fatal_reading_state (t0, "Unknown type");
	}

      next_state_tokens (1);
      *type = (type_p) *slot;
    }
  else
    {
      fatal_reading_state (t0, "Bad seen type");
    }
}


/* Read the scalar_nonchar type.  */
static void
read_state_scalar_nonchar_type (type_p *type)
{
  *type = &scalar_nonchar;
  read_state_common_type_content (*type);
}


/* Read the scalar_char type.  */
static void
read_state_scalar_char_type (type_p *type)
{
  *type = &scalar_char;
  read_state_common_type_content (*type);
}

/* Read the string_type.  */
static void
read_state_string_type (type_p *type)
{
  *type = &string_type;
  read_state_common_type_content (*type);
}

/* Read the callback_type.  */
static void
read_state_callback_type (type_p *type)
{
  *type = &callback_type;
  read_state_common_type_content (*type);
}


/* Read a lang_bitmap representing a set of GCC front-end languages.  */
static void
read_state_lang_bitmap (lang_bitmap *bitmap)
{
  struct state_token_st *t;

  t = peek_state_token (0);
  if (state_token_kind (t) == STOK_INTEGER)
    {
      *bitmap = t->stok_un.stok_num;
      next_state_tokens (1);
    }
  else
    {
      fatal_reading_state (t, "Bad syntax for bitmap");
    }
}


/* Read an undefined type.  */
static void
read_state_undefined_type (type_p type)
{
  struct state_token_st *t0;

  type->kind = TYPE_UNDEFINED;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      if (state_token_is_name (t0, "nil"))
	{
	  type->u.s.tag = NULL;
	  DBGPRINTF ("read anonymous undefined type @%p #%d",
		     (void *) type, type->state_number);
	}
      else
	{
	  type->u.s.tag = xstrdup (t0->stok_un.stok_string);
	  DBGPRINTF ("read undefined type @%p #%d '%s'",
		     (void *) type, type->state_number, type->u.s.tag);
	}

      next_state_tokens (1);
      read_state_fileloc (&(type->u.s.line));
    }
  else
    {
      fatal_reading_state (t0, "Bad tag in undefined type");
    }
}


/* Read a GTY-ed struct type.  */
static void
read_state_struct_type (type_p type)
{
  struct state_token_st *t0;

  type->kind = TYPE_STRUCT;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      if (state_token_is_name (t0, "nil"))
	{
	  type->u.s.tag = NULL;
	  DBGPRINTF ("read anonymous struct type @%p #%d",
		     (void *) type, type->state_number);
	}
      else
	{
	  type->u.s.tag = xstrdup (t0->stok_un.stok_string);
	  DBGPRINTF ("read struct type @%p #%d '%s'",
		     (void *) type, type->state_number, type->u.s.tag);
	}

      next_state_tokens (1);
      read_state_fileloc (&(type->u.s.line));
      read_state_fields (&(type->u.s.fields));
      read_state_options (&(type->u.s.opt));
      read_state_lang_bitmap (&(type->u.s.bitmap));
      read_state_type (&(type->u.s.lang_struct));
      read_state_type (&(type->u.s.base_class));
      if (type->u.s.base_class)
	add_subclass (type->u.s.base_class, type);
    }
  else
    {
      fatal_reading_state (t0, "Bad tag in struct type");
    }
}


/* Read a GTY-ed user-provided struct TYPE.  */

static void
read_state_user_struct_type (type_p type)
{
  struct state_token_st *t0;

  type->kind = TYPE_USER_STRUCT;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      if (state_token_is_name (t0, "nil"))
	{
	  type->u.s.tag = NULL;
	  DBGPRINTF ("read anonymous struct type @%p #%d",
		     (void *) type, type->state_number);
	}
      else
	{
	  type->u.s.tag = xstrdup (t0->stok_un.stok_string);
	  DBGPRINTF ("read struct type @%p #%d '%s'",
		     (void *) type, type->state_number, type->u.s.tag);
	}

      next_state_tokens (1);
      read_state_fileloc (&(type->u.s.line));
      read_state_fields (&(type->u.s.fields));
    }
  else
    {
      fatal_reading_state (t0, "Bad tag in user-struct type");
    }
}


/* Read a GTY-ed union type.  */
static void
read_state_union_type (type_p type)
{
  struct state_token_st *t0;

  type->kind = TYPE_UNION;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      if (state_token_is_name (t0, "nil"))
	{
	  type->u.s.tag = NULL;
	  DBGPRINTF ("read anonymous union type @%p #%d",
		     (void *) type, type->state_number);
	}
      else
	{
	  type->u.s.tag = xstrdup (t0->stok_un.stok_string);
	  DBGPRINTF ("read union type @%p #%d '%s'",
		     (void *) type, type->state_number, type->u.s.tag);
	}
      next_state_tokens (1);
      read_state_fileloc (&(type->u.s.line));
      read_state_fields (&(type->u.s.fields));
      read_state_options (&(type->u.s.opt));
      read_state_lang_bitmap (&(type->u.s.bitmap));
      read_state_type (&(type->u.s.lang_struct));
    }
  else
    fatal_reading_state (t0, "Bad tag in union type");
}


/* Read a GTY-ed pointer type.  */
static void
read_state_pointer_type (type_p type)
{
  type->kind = TYPE_POINTER;
  read_state_common_type_content (type);
  DBGPRINTF ("read pointer type @%p #%d", (void *) type, type->state_number);
  read_state_type (&(type->u.p));
}


/* Read a GTY-ed array type.  */
static void
read_state_array_type (type_p type)
{
  struct state_token_st *t0;

  type->kind = TYPE_ARRAY;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      type->u.a.len = xstrdup (t0->stok_un.stok_string);
      DBGPRINTF ("read array type @%p #%d length '%s'",
		 (void *) type, type->state_number, type->u.a.len);
      next_state_tokens (1);
    }

  else if (state_token_is_name (t0, "nil"))
    {
      type->u.a.len = NULL;
      DBGPRINTF ("read array type @%p #%d without length",
		 (void *) type, type->state_number);
      next_state_tokens (1);
    }

  else
    fatal_reading_state (t0, "Bad array name type");
  read_state_type (&(type->u.a.p));
}



/* Read a lang_struct type for GTY-ed struct-s which depends upon GCC
   front-end languages.  This is a tricky function and it was painful
   to debug.  Change it with extreme care.  See also
   write_state_lang_struct_type.  */
static void
read_state_lang_struct_type (type_p type)
{
  struct state_token_st *t0 = NULL;
  struct state_token_st *t1 = NULL;
  struct state_token_st *t2 = NULL;

  type->kind = TYPE_LANG_STRUCT;
  read_state_common_type_content (type);
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      if (state_token_is_name (t0, "nil"))
	{
	  DBGPRINTF ("read anonymous lang_struct type @%p #%d",
		     (void *) type, type->state_number);
	  type->u.s.tag = NULL;
	}
      else
	{
	  type->u.s.tag = xstrdup (t0->stok_un.stok_string);
	  DBGPRINTF ("read lang_struct type @%p #%d '%s'",
		     (void *) type, type->state_number, type->u.s.tag);
	}
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Bad tag in lang struct type");
  read_state_fileloc (&(type->u.s.line));
  read_state_fields (&(type->u.s.fields));
  read_state_options (&(type->u.s.opt));
  read_state_lang_bitmap (&(type->u.s.bitmap));
  /* Within lang_struct-ures, the lang_struct field is a linked list
     of homonymous types! */
  t0 = peek_state_token (0);
  t1 = peek_state_token (1);
  t2 = peek_state_token (2);
  /* Parse (!homotypes <number-types> <type-1> .... <type-n>) */
  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!homotypes")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      type_p *prevty = &type->u.s.lang_struct;
      int nbhomotype = t2->stok_un.stok_num;
      int i = 0;
      t0 = t1 = t2 = NULL;
      next_state_tokens (3);
      for (i = 0; i < nbhomotype; i++)
	{
	  read_state_type (prevty);
	  t0 = peek_state_token (0);
	  if (*prevty)
	    prevty = &(*prevty)->next;
	  else
	      fatal_reading_state (t0,
				   "expecting type in homotype list for lang_struct");
	};
      if (state_token_kind (t0) != STOK_RIGHTPAR)
	fatal_reading_state (t0,
			     "expecting ) in homotype list for lang_struct");
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "expecting !homotypes for lang_struct");
}


/* Read the gc used information.  */
static void
read_state_gc_used (enum gc_used_enum *pgus)
{
  struct state_token_st *t0 = peek_state_token (0);
  if (state_token_is_name (t0, "gc_unused"))
    *pgus = GC_UNUSED;
  else if (state_token_is_name (t0, "gc_used"))
    *pgus = GC_USED;
  else if (state_token_is_name (t0, "gc_maybe_pointed_to"))
    *pgus = GC_MAYBE_POINTED_TO;
  else if (state_token_is_name (t0, "gc_pointed_to"))
    *pgus = GC_POINTED_TO;
  else
    fatal_reading_state (t0, "invalid gc_used information");
  next_state_tokens (1);
}


/* Utility function to read the common content of types.  */
static void
read_state_common_type_content (type_p current)
{
  struct state_token_st *t0 = peek_state_token (0);

  if (state_token_kind (t0) == STOK_INTEGER)
    {
      current->state_number = t0->stok_un.stok_num;
      next_state_tokens (1);
      record_type (current);
    }
  else
      fatal_reading_state_printf (t0,
				  "Expected integer for state_number line %d",
				  state_line);
  /* We don't read the next field of the type.  */
  read_state_type (&current->pointer_to);
  read_state_gc_used (&current->gc_used);
}


/* Read a GTY-ed type.  */
void
read_state_type (type_p *current)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);

  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!type"))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      if (state_token_is_name (t0, "already_seen"))
	{
	  next_state_tokens (1);
	  read_state_already_seen_type (current);
	}
      else
	{
	  t0 = peek_state_token (0);

	  if (state_token_is_name (t0, "scalar_nonchar"))
	    {
	      next_state_tokens (1);
	      read_state_scalar_nonchar_type (current);
	    }
	  else if (state_token_is_name (t0, "scalar_char"))
	    {
	      next_state_tokens (1);
	      read_state_scalar_char_type (current);
	    }
	  else if (state_token_is_name (t0, "string"))
	    {
	      next_state_tokens (1);
	      read_state_string_type (current);
	    }
	  else if (state_token_is_name (t0, "callback"))
	    {
	      next_state_tokens (1);
	      read_state_callback_type (current);
	    }
	  else if (state_token_is_name (t0, "undefined"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_undefined_type (*current);
	    }
	  else if (state_token_is_name (t0, "struct"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_struct_type (*current);
	    }
	  else if (state_token_is_name (t0, "union"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_union_type (*current);
	    }
	  else if (state_token_is_name (t0, "lang_struct"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_lang_struct_type (*current);
	    }
	  else if (state_token_is_name (t0, "pointer"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_pointer_type (*current);
	    }
	  else if (state_token_is_name (t0, "array"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_array_type (*current);
	    }
	  else if (state_token_is_name (t0, "user_struct"))
	    {
	      *current = XCNEW (struct type);
	      next_state_tokens (1);
	      read_state_user_struct_type (*current);
	    }
	  else
	    fatal_reading_state (t0, "bad type in (!type");
	}
      t0 = peek_state_token (0);
      if (state_token_kind (t0) != STOK_RIGHTPAR)
	fatal_reading_state (t0, "missing ) in type");
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      next_state_tokens (1);
      *current = NULL;
    }
  else
    fatal_reading_state (t0, "bad type syntax");
}


/* Read a file location.  Files within the source directory are dealt
   with specifically.  */
void
read_state_fileloc (struct fileloc *floc)
{
  bool issrcfile = false;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);

  gcc_assert (floc != NULL);
  gcc_assert (srcdir != NULL);

  if (state_token_kind (t0) == STOK_LEFTPAR &&
      (state_token_is_name (t1, "!fileloc")
       || (issrcfile = state_token_is_name (t1, "!srcfileloc"))))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      t1 = peek_state_token (1);
      if (state_token_kind (t0) == STOK_STRING &&
	  state_token_kind (t1) == STOK_INTEGER)
	{
	  char *path = t0->stok_un.stok_string;
	  if (issrcfile)
	    {
	      static const char dirsepstr[2] = { DIR_SEPARATOR, (char) 0 };
	      char *fullpath = concat (srcdir, dirsepstr, path, NULL);
	      floc->file = input_file_by_name (fullpath);
	      free (fullpath);
	    }
	  else
	    floc->file = input_file_by_name (path);
	  floc->line = t1->stok_un.stok_num;
	  next_state_tokens (2);
	}
      else
	fatal_reading_state (t0,
			     "Bad fileloc syntax, expected path string and line");
      t0 = peek_state_token (0);
      if (state_token_kind (t0) != STOK_RIGHTPAR)
	fatal_reading_state (t0, "Bad fileloc syntax, expected )");
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      next_state_tokens (1);
      floc->file = NULL;
      floc->line = 0;
    }
  else
    fatal_reading_state (t0, "Bad fileloc syntax");
}


/* Read the fields of a GTY-ed type.  */
void
read_state_fields (pair_p *fields)
{
  pair_p tmp = NULL;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!fields")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      int nbfields = t2->stok_un.stok_num;
      int nbpairs = 0;
      next_state_tokens (3);
      nbpairs = read_state_pair_list (&tmp);
      t0 = peek_state_token (0);
      if (nbpairs != nbfields)
	fatal_reading_state_printf
	  (t0,
	   "Mismatched fields number, expected %d got %d", nbpairs, nbfields);
      if (state_token_kind (t0) == STOK_RIGHTPAR)
	next_state_tokens (1);
      else
	fatal_reading_state (t0, "Bad fields expecting )");
    }

  *fields = tmp;
}


/* Read a string option.  */
static void
read_state_string_option (options_p opt)
{
  struct state_token_st *t0 = peek_state_token (0);
  opt->kind = OPTION_STRING;
  if (state_token_kind (t0) == STOK_STRING)
    {
      opt->info.string = xstrdup (t0->stok_un.stok_string);
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      opt->info.string = NULL;
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Missing name in string option");
}


/* Read a type option.  */
static void
read_state_type_option (options_p opt)
{
  opt->kind = OPTION_TYPE;
  read_state_type (&(opt->info.type));
}


/* Read a nested option.  */
static void
read_state_nested_option (options_p opt)
{
  struct state_token_st *t0;

  opt->info.nested = XCNEW (struct nested_ptr_data);
  opt->kind = OPTION_NESTED;
  read_state_type (&(opt->info.nested->type));
  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      opt->info.nested->convert_from = xstrdup (t0->stok_un.stok_string);
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      opt->info.nested->convert_from = NULL;
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Bad nested convert_from option");

  t0 = peek_state_token (0);
  if (state_token_kind (t0) == STOK_STRING)
    {
      opt->info.nested->convert_to = xstrdup (t0->stok_un.stok_string);
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      opt->info.nested->convert_to = NULL;
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Bad nested convert_from option");
}


/* Read an GTY option.  */
static void
read_state_option (options_p *opt)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);

  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!option"))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      if (state_token_kind (t0) == STOK_NAME)
	{
	  *opt = XCNEW (struct options);
	  if (state_token_is_name (t0, "nil"))
	    (*opt)->name = NULL;
	  else
	    (*opt)->name = t0->stok_un.stok_ident->stid_name;
	  next_state_tokens (1);
	  t0 = peek_state_token (0);
	  if (state_token_kind (t0) == STOK_NAME)
	    {
	      if (state_token_is_name (t0, "string"))
		{
		  next_state_tokens (1);
		  read_state_string_option (*opt);
		}
	      else if (state_token_is_name (t0, "type"))
		{
		  next_state_tokens (1);
		  read_state_type_option (*opt);
		}
	      else if (state_token_is_name (t0, "nested"))
		{
		  next_state_tokens (1);
		  read_state_nested_option (*opt);
		}
	      else
		fatal_reading_state (t0, "Bad option type");
	      t0 = peek_state_token (0);
	      if (state_token_kind (t0) != STOK_RIGHTPAR)
		fatal_reading_state (t0, "Bad syntax in option, expecting )");

	      next_state_tokens (1);
	    }
	  else
	    fatal_reading_state (t0, "Missing option type");
	}
      else
	fatal_reading_state (t0, "Bad name for option");
    }
  else
    fatal_reading_state (t0, "Bad option, waiting for )");
}

/* Read a list of options.  */
void
read_state_options (options_p *opt)
{
  options_p head = NULL;
  options_p previous = NULL;
  options_p current_option = NULL;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);

  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!options"))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      while (state_token_kind (t0) != STOK_RIGHTPAR)
	{
	  read_state_option (&current_option);
	  if (head == NULL)
	    {
	      head = current_option;
	      previous = head;
	    }
	  else
	    {
	      previous->next = current_option;
	      previous = current_option;
	    }
	  t0 = peek_state_token (0);
	}
      next_state_tokens (1);
    }
  else if (state_token_is_name (t0, "nil"))
    {
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Bad options syntax");

  *opt = head;
}


/* Read a version, and check against the version of the gengtype.  */
static void
read_state_version (const char *ver_string)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);

  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!version"))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      t1 = peek_state_token (1);
      if (state_token_kind (t0) == STOK_STRING &&
	  state_token_kind (t1) == STOK_RIGHTPAR)
	{
	  /* Check that the read version string is the same as current
	     version.  */
	  if (strcmp (ver_string, t0->stok_un.stok_string))
	    fatal_reading_state_printf (t0,
					"version string mismatch; expecting %s but got %s",
					ver_string,
					t0->stok_un.stok_string);
	  next_state_tokens (2);
	}
      else
	fatal_reading_state (t0, "Missing version or right parenthesis");
    }
  else
    fatal_reading_state (t0, "Bad version syntax");
}


/* Read a pair.  */
void
read_state_pair (pair_p *current)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!pair"))
    {
      *current = XCNEW (struct pair);
      next_state_tokens (2);
      t0 = peek_state_token (0);
      if (state_token_kind (t0) == STOK_STRING)
	{
	  if (strcmp (t0->stok_un.stok_string, "nil") == 0)
	    {
	      (*current)->name = NULL;
	    }
	  else
	    {
	      (*current)->name = xstrdup (t0->stok_un.stok_string);
	    }
	  next_state_tokens (1);
	  read_state_type (&((*current)->type));
	  read_state_fileloc (&((*current)->line));
	  read_state_options (&((*current)->opt));
	  t0 = peek_state_token (0);
	  if (state_token_kind (t0) == STOK_RIGHTPAR)
	    {
	      next_state_tokens (1);
	    }
	  else
	    {
	      fatal_reading_state (t0, "Bad syntax for pair, )");
	    }
	}
      else
	{
	  fatal_reading_state (t0, "Bad name for pair");
	}
    }
  else if (state_token_kind (t0) == STOK_NAME &&
	   state_token_is_name (t0, "nil"))
    {
      next_state_tokens (1);
      *current = NULL;
    }
  else
    fatal_reading_state_printf (t0, "Bad syntax for pair, (!pair %d",
				state_token->stok_kind);
}


/* Return the number of pairs actually read.  */
int
read_state_pair_list (pair_p *list)
{
  int nbpair = 0;
  pair_p head = NULL;
  pair_p previous = NULL;
  pair_p tmp = NULL;
  struct state_token_st *t0 = peek_state_token (0);
  while (t0 && state_token_kind (t0) != STOK_RIGHTPAR)
    {
      read_state_pair (&tmp);
      if (head == NULL)
	{
	  head = tmp;
	  previous = head;
	}
      else
	{
	  previous->next = tmp;
	  previous = tmp;
	}
      t0 = peek_state_token (0);
      nbpair++;
    }

  /* don't consume the ); the caller will eat it.  */
  *list = head;
  return nbpair;
}

/* Read the typedefs.  */
static void
read_state_typedefs (pair_p *typedefs)
{
  int nbtypedefs = 0;
  pair_p list = NULL;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!typedefs")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      int nbpairs = 0;
      nbtypedefs = t2->stok_un.stok_num;
      next_state_tokens (3);
      nbpairs = read_state_pair_list (&list);
      t0 = peek_state_token (0);
      if (nbpairs != nbtypedefs)
	fatal_reading_state_printf
	  (t0,
	   "invalid number of typedefs, expected %d but got %d",
	   nbtypedefs, nbpairs);
      if (state_token_kind (t0) == STOK_RIGHTPAR)
	next_state_tokens (1);
      else
	fatal_reading_state (t0, "Bad typedefs syntax )");
    }
  else
    fatal_reading_state (t0, "Bad typedefs syntax (!typedefs");

  if (verbosity_level >= 2)
    printf ("%s read %d typedefs from state\n", progname, nbtypedefs);
  *typedefs = list;
}


/* Read the structures.  */
static void
read_state_structures (type_p *structures)
{
  type_p head = NULL;
  type_p previous = NULL;
  type_p tmp;
  int nbstruct = 0, countstruct = 0;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!structures")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      nbstruct = t2->stok_un.stok_num;
      next_state_tokens (3);
      t0 = peek_state_token (0);
      while (t0 && state_token_kind (t0) != STOK_RIGHTPAR)
	{
	  tmp = NULL;
	  read_state_type (&tmp);
	  countstruct++;
	  if (head == NULL)
	    {
	      head = tmp;
	      previous = head;
	    }
	  else
	    {
	      previous->next = tmp;
	      previous = tmp;
	    }
	  t0 = peek_state_token (0);
	}
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "Bad structures syntax");
  if (countstruct != nbstruct)
    fatal_reading_state_printf (NULL_STATE_TOKEN, 
				"expected %d structures but got %d",
				nbstruct, countstruct);
  if (verbosity_level >= 2)
    printf ("%s read %d structures from state\n", progname, nbstruct);
  *structures = head;
}


/* Read the variables.  */
static void
read_state_variables (pair_p *variables)
{
  pair_p list = NULL;
  int nbvars = 0;
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!variables")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      int nbpairs = 0;
      nbvars = t2->stok_un.stok_num;
      next_state_tokens (3);
      nbpairs = read_state_pair_list (&list);
      t0 = peek_state_token (0);
      if (nbpairs != nbvars)
	fatal_reading_state_printf
	  (t0, "Invalid number of variables, expected %d but got %d",
	   nbvars, nbpairs);
      if (state_token_kind (t0) == STOK_RIGHTPAR)
	next_state_tokens (1);
      else
	fatal_reading_state (t0, "Waiting for ) in variables");
    }
  else
    fatal_reading_state (t0, "Bad variables syntax");
  *variables = list;
  if (verbosity_level >= 2)
    printf ("%s read %d variables from state\n", progname, nbvars);
}


/* Read the source directory.  */
static void
read_state_srcdir (void)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  if (state_token_kind (t0) == STOK_LEFTPAR &&
      state_token_is_name (t1, "!srcdir"))
    {
      next_state_tokens (2);
      t0 = peek_state_token (0);
      t1 = peek_state_token (1);
      if (state_token_kind (t0) == STOK_STRING &&
	  state_token_kind (t1) == STOK_RIGHTPAR)
	{
	  srcdir = xstrdup (t0->stok_un.stok_string);
	  srcdir_len = strlen (srcdir);
	  next_state_tokens (2);
	  return;
	}
    }

  fatal_reading_state (t0, "Bad srcdir in state_file");
}


/* Read the sequence of GCC front-end languages.  */
static void
read_state_languages (void)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);
  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!languages")
      && state_token_kind (t2) == STOK_INTEGER)
    {
      int i = 0;
      num_lang_dirs = t2->stok_un.stok_num;
      lang_dir_names = XCNEWVEC (const char *, num_lang_dirs);
      next_state_tokens (3);
      t0 = t1 = t2 = NULL;
      for (i = 0; i < (int) num_lang_dirs; i++)
	{
	  t0 = peek_state_token (0);
	  if (state_token_kind (t0) != STOK_NAME)
	    fatal_reading_state (t0, "expecting language name in state file");
	  lang_dir_names[i] = t0->stok_un.stok_ident->stid_name;
	  next_state_tokens (1);
	}
      t0 = peek_state_token (0);
      if (state_token_kind (t0) != STOK_RIGHTPAR)
	fatal_reading_state (t0, "missing ) in languages list of state file");
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "expecting languages list in state file");

}

/* Read the sequence of files.  */
static void
read_state_files_list (void)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);
  struct state_token_st *t3 = peek_state_token (3);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!fileslist")
      && state_token_kind (t2) == STOK_INTEGER
      && state_token_kind (t3) == STOK_INTEGER)
    {
      int i = 0, j = 0;
      num_gt_files = t2->stok_un.stok_num;
      num_build_headers = t3->stok_un.stok_num;
      next_state_tokens (4);
      t0 = t1 = t2 = t3 = NULL;
      gt_files = XCNEWVEC (const input_file *, num_gt_files);
      build_headers = XCNEWVEC (const char *, num_build_headers);
      for (i = 0; i < (int) num_gt_files; i++)
	{
	  bool issrcfile = FALSE;
	  t0 = t1 = t2 = NULL;
	  t0 = peek_state_token (0);
	  t1 = peek_state_token (1);
	  t2 = peek_state_token (2);
	  if (state_token_kind (t0) == STOK_LEFTPAR
	      && (state_token_is_name (t1, "!file")
		  || (issrcfile = state_token_is_name (t1, "!srcfile")))
	      && state_token_kind (t2) == STOK_INTEGER)
	    {
	      lang_bitmap bmap = t2->stok_un.stok_num;
	      next_state_tokens (3);
	      t0 = t1 = t2 = NULL;
	      t0 = peek_state_token (0);
	      t1 = peek_state_token (1);
	      if (state_token_kind (t0) == STOK_STRING
		  && state_token_kind (t1) == STOK_RIGHTPAR)
		{
		  const char *fnam = t0->stok_un.stok_string;
		  /* Allocate & fill a gt_file entry with space for the lang_bitmap before! */
		  input_file *curgt = NULL;
		  if (issrcfile)
		    {
		      static const char dirsepstr[2] =
			{ DIR_SEPARATOR, (char) 0 };
		      char *fullpath = concat (srcdir, dirsepstr, fnam, NULL);
		      curgt = input_file_by_name (fullpath);
		      free (fullpath);
		    }
		  else
		    {
		      curgt = input_file_by_name (fnam);
		      /* Look for a header file created during the build,
			 which looks like "./<filename>.h".  */
		      int len = strlen (fnam);
		      if (len >= 5
			  && fnam[0] == '.'
			  && IS_DIR_SEPARATOR (fnam[1])
			  && fnam[len-2] == '.'
			  && fnam[len-1] == 'h')
			{
			  char *buf = (char *) xmalloc (len - 1);
			  /* Strip the leading "./" from the filename.  */
			  strcpy (buf, &fnam[2]);
			  build_headers[j++] = buf;
			}
		    }
		  set_lang_bitmap (curgt, bmap);
		  gt_files[i] = curgt;
		  next_state_tokens (2);
		}
	      else
		fatal_reading_state (t0,
				     "bad file in !fileslist of state file");
	    }
	  else
	    fatal_reading_state (t0,
				 "expecting file in !fileslist of state file");
	};
      t0 = peek_state_token (0);
      if (state_token_kind (t0) != STOK_RIGHTPAR)
	fatal_reading_state (t0, "missing ) for !fileslist in state file");
      next_state_tokens (1);
    }
  else
    fatal_reading_state (t0, "missing !fileslist in state file");
}


/* Read the trailer.  */
static void
read_state_trailer (void)
{
  struct state_token_st *t0 = peek_state_token (0);
  struct state_token_st *t1 = peek_state_token (1);
  struct state_token_st *t2 = peek_state_token (2);

  if (state_token_kind (t0) == STOK_LEFTPAR
      && state_token_is_name (t1, "!endfile")
      && state_token_kind (t2) == STOK_RIGHTPAR)
    next_state_tokens (3);
  else
    fatal_reading_state (t0, "missing !endfile in state file");
}


/* Utility functions for the state_seen_types hash table.  */
static unsigned
hash_type_number (const void *ty)
{
  const struct type *type = (const struct type *) ty;

  return type->state_number;
}

static int
equals_type_number (const void *ty1, const void *ty2)
{
  const struct type *type1 = (const struct type *) ty1;
  const struct type *type2 = (const struct type *) ty2;

  return type1->state_number == type2->state_number;
}


/* The function reading the state, called by main from gengtype.cc.  */
void
read_state (const char *path)
{
  state_file = fopen (path, "r");
  if (state_file == NULL)
    fatal ("Failed to open state file %s for reading [%s]", path,
	   xstrerror (errno));
  state_path = path;
  state_line = 1;

  if (verbosity_level >= 1)
    {
      printf ("%s reading state file %s;", progname, state_path);
      if (verbosity_level >= 2)
	putchar ('\n');
      fflush (stdout);
    }

  state_seen_types =
    htab_create (2017, hash_type_number, equals_type_number, NULL);
  state_ident_tab =
    htab_create (4027, htab_hash_string, htab_eq_string, NULL);
  read_state_version (version_string);
  read_state_srcdir ();
  read_state_languages ();
  read_state_files_list ();
  read_state_structures (&structures);
  if (ferror (state_file))
    fatal_reading_state_printf
      (NULL_STATE_TOKEN, "input error while reading state [%s]",
       xstrerror (errno));
  read_state_typedefs (&typedefs);
  read_state_variables (&variables);
  read_state_trailer ();

  if (verbosity_level >= 1)
    {
      printf ("%s read %ld bytes.\n", progname, ftell (state_file));
      fflush (stdout);
    };

  if (fclose (state_file))
    fatal ("failed to close read state file %s [%s]",
	   path, xstrerror (errno));
  state_file = NULL;
  state_path = NULL;
}

/* End of file gengtype-state.cc.  */
