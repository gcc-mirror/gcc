/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"
#include "prefix.h"
#include "intl.h"
#include "mkdeps.h"
#include "cppdefault.h"

/* Windows does not natively support inodes, and neither does MSDOS.
   Cygwin's emulation can generate non-unique inodes, so don't use it.
   VMS has non-numeric inodes.  */
#ifdef VMS
# define INO_T_EQ(A, B) (!memcmp (&(A), &(B), sizeof (A)))
# define INO_T_COPY(DEST, SRC) memcpy(&(DEST), &(SRC), sizeof (SRC))
#else
# if (defined _WIN32 && ! defined (_UWIN)) || defined __MSDOS__
#  define INO_T_EQ(A, B) 0
# else
#  define INO_T_EQ(A, B) ((A) == (B))
# endif
# define INO_T_COPY(DEST, SRC) (DEST) = (SRC)
#endif

/* Internal structures and prototypes.  */

/* A `struct pending_option' remembers one -D, -A, -U, -include, or
   -imacros switch.  */
typedef void (* cl_directive_handler) PARAMS ((cpp_reader *, const char *));
struct pending_option
{
  struct pending_option *next;
  const char *arg;
  cl_directive_handler handler;
};

/* The `pending' structure accumulates all the options that are not
   actually processed until we hit cpp_read_main_file.  It consists of
   several lists, one for each type of option.  We keep both head and
   tail pointers for quick insertion.  */
struct cpp_pending
{
  struct pending_option *directive_head, *directive_tail;

  struct search_path *quote_head, *quote_tail;
  struct search_path *brack_head, *brack_tail;
  struct search_path *systm_head, *systm_tail;
  struct search_path *after_head, *after_tail;

  struct pending_option *imacros_head, *imacros_tail;
  struct pending_option *include_head, *include_tail;
};

#ifdef __STDC__
#define APPEND(pend, list, elt) \
  do {  if (!(pend)->list##_head) (pend)->list##_head = (elt); \
	else (pend)->list##_tail->next = (elt); \
	(pend)->list##_tail = (elt); \
  } while (0)
#else
#define APPEND(pend, list, elt) \
  do {  if (!(pend)->list/**/_head) (pend)->list/**/_head = (elt); \
	else (pend)->list/**/_tail->next = (elt); \
	(pend)->list/**/_tail = (elt); \
  } while (0)
#endif

static void path_include		PARAMS ((cpp_reader *,
						 char *, int));
static void init_library		PARAMS ((void));
static void init_builtins		PARAMS ((cpp_reader *));
static void mark_named_operators	PARAMS ((cpp_reader *));
static void append_include_chain	PARAMS ((cpp_reader *,
						 char *, int, int));
static struct search_path * remove_dup_dir	PARAMS ((cpp_reader *,
						 struct search_path *,
						 struct search_path **));
static struct search_path * remove_dup_nonsys_dirs PARAMS ((cpp_reader *,
						 struct search_path **,
						 struct search_path *));
static struct search_path * remove_dup_dirs PARAMS ((cpp_reader *,
						 struct search_path **));
static void merge_include_chains	PARAMS ((cpp_reader *));
static bool push_include		PARAMS ((cpp_reader *,
						 struct pending_option *));
static void free_chain			PARAMS ((struct pending_option *));
static void init_standard_includes	PARAMS ((cpp_reader *));
static void read_original_filename	PARAMS ((cpp_reader *));
static void new_pending_directive	PARAMS ((struct cpp_pending *,
						 const char *,
						 cl_directive_handler));
static int parse_option			PARAMS ((const char *));
static void post_options		PARAMS ((cpp_reader *));

/* Fourth argument to append_include_chain: chain to use.
   Note it's never asked to append to the quote chain.  */
enum { BRACKET = 0, SYSTEM, AFTER };

/* If we have designated initializers (GCC >2.7) these tables can be
   initialized, constant data.  Otherwise, they have to be filled in at
   runtime.  */
#if HAVE_DESIGNATED_INITIALIZERS

#define init_trigraph_map()  /* Nothing.  */
#define TRIGRAPH_MAP \
__extension__ const uchar _cpp_trigraph_map[UCHAR_MAX + 1] = {

#define END };
#define s(p, v) [p] = v,

#else

#define TRIGRAPH_MAP uchar _cpp_trigraph_map[UCHAR_MAX + 1] = { 0 }; \
 static void init_trigraph_map PARAMS ((void)) { \
 unsigned char *x = _cpp_trigraph_map;

#define END }
#define s(p, v) x[p] = v;

#endif

TRIGRAPH_MAP
  s('=', '#')	s(')', ']')	s('!', '|')
  s('(', '[')	s('\'', '^')	s('>', '}')
  s('/', '\\')	s('<', '{')	s('-', '~')
END

#undef s
#undef END
#undef TRIGRAPH_MAP

/* Given a colon-separated list of file names PATH,
   add all the names to the search path for include files.  */
static void
path_include (pfile, list, path)
     cpp_reader *pfile;
     char *list;
     int path;
{
  char *p, *q, *name;

  p = list;

  do
    {
      /* Find the end of this name.  */
      q = p;
      while (*q != 0 && *q != PATH_SEPARATOR) q++;
      if (q == p)
	{
	  /* An empty name in the path stands for the current directory.  */
	  name = (char *) xmalloc (2);
	  name[0] = '.';
	  name[1] = 0;
	}
      else
	{
	  /* Otherwise use the directory that is named.  */
	  name = (char *) xmalloc (q - p + 1);
	  memcpy (name, p, q - p);
	  name[q - p] = 0;
	}

      append_include_chain (pfile, name, path, path == SYSTEM);

      /* Advance past this name.  */
      if (*q == 0)
	break;
      p = q + 1;
    }
  while (1);
}

/* Append DIR to include path PATH.  DIR must be allocated on the
   heap; this routine takes responsibility for freeing it.  CXX_AWARE
   is nonzero if the header contains extern "C" guards for C++,
   otherwise it is zero.  */
static void
append_include_chain (pfile, dir, path, cxx_aware)
     cpp_reader *pfile;
     char *dir;
     int path;
     int cxx_aware;
{
  struct cpp_pending *pend = CPP_OPTION (pfile, pending);
  struct search_path *new;
  struct stat st;
  unsigned int len;

  if (*dir == '\0')
    {
      free (dir);
      dir = xstrdup (".");
    }
  _cpp_simplify_pathname (dir);

  if (stat (dir, &st))
    {
      /* Dirs that don't exist are silently ignored.  */
      if (errno != ENOENT)
	cpp_errno (pfile, DL_ERROR, dir);
      else if (CPP_OPTION (pfile, verbose))
	fprintf (stderr, _("ignoring nonexistent directory \"%s\"\n"), dir);
      free (dir);
      return;
    }

  if (!S_ISDIR (st.st_mode))
    {
      cpp_error_with_line (pfile, DL_ERROR, 0, 0, "%s: Not a directory", dir);
      free (dir);
      return;
    }

  len = strlen (dir);
  if (len > pfile->max_include_len)
    pfile->max_include_len = len;

  new = (struct search_path *) xmalloc (sizeof (struct search_path));
  new->name = dir;
  new->len = len;
  INO_T_COPY (new->ino, st.st_ino);
  new->dev  = st.st_dev;
  /* Both systm and after include file lists should be treated as system
     include files since these two lists are really just a concatenation
     of one "system" list.  */
  if (path == SYSTEM || path == AFTER)
    new->sysp = cxx_aware ? 1 : 2;
  else
    new->sysp = 0;
  new->name_map = NULL;
  new->next = NULL;

  switch (path)
    {
    case BRACKET:	APPEND (pend, brack, new); break;
    case SYSTEM:	APPEND (pend, systm, new); break;
    case AFTER:		APPEND (pend, after, new); break;
    }
}

/* Handle a duplicated include path.  PREV is the link in the chain
   before the duplicate, or NULL if the duplicate is at the head of
   the chain.  The duplicate is removed from the chain and freed.
   Returns PREV.  */
static struct search_path *
remove_dup_dir (pfile, prev, head_ptr)
     cpp_reader *pfile;
     struct search_path *prev;
     struct search_path **head_ptr;
{
  struct search_path *cur;

  if (prev != NULL)
    {
      cur = prev->next;
      prev->next = cur->next;
    }
  else
    {
      cur = *head_ptr;
      *head_ptr = cur->next;
    }

  if (CPP_OPTION (pfile, verbose))
    fprintf (stderr, _("ignoring duplicate directory \"%s\"\n"), cur->name);

  free ((PTR) cur->name);
  free (cur);

  return prev;
}

/* Remove duplicate non-system directories for which there is an equivalent
   system directory latter in the chain.  The range for removal is between
   *HEAD_PTR and END.  Returns the directory before END, or NULL if none.
   This algorithm is quadratic in the number system directories, which is
   acceptable since there aren't usually that many of them.  */
static struct search_path *
remove_dup_nonsys_dirs (pfile, head_ptr, end)
     cpp_reader *pfile;
     struct search_path **head_ptr;
     struct search_path *end;
{
  int sysdir = 0;
  struct search_path *prev = NULL, *cur, *other;

  for (cur = *head_ptr; cur; cur = cur->next)
    {
      if (cur->sysp)
	{
	  sysdir = 1;
	  for (other = *head_ptr, prev = NULL;
	       other != end;
	       other = other ? other->next : *head_ptr)
	    {
	      if (!other->sysp
		  && INO_T_EQ (cur->ino, other->ino)
		  && cur->dev == other->dev)
		{
		  other = remove_dup_dir (pfile, prev, head_ptr);
		  if (CPP_OPTION (pfile, verbose))
		    fprintf (stderr,
  _("  as it is a non-system directory that duplicates a system directory\n"));
		}
	      prev = other;
	    }
	}
    }

  if (!sysdir)
    for (cur = *head_ptr; cur != end; cur = cur->next)
      prev = cur;

  return prev;
}

/* Remove duplicate directories from a chain.  Returns the tail of the
   chain, or NULL if the chain is empty.  This algorithm is quadratic
   in the number of -I switches, which is acceptable since there
   aren't usually that many of them.  */
static struct search_path *
remove_dup_dirs (pfile, head_ptr)
     cpp_reader *pfile;
     struct search_path **head_ptr;
{
  struct search_path *prev = NULL, *cur, *other;

  for (cur = *head_ptr; cur; cur = cur->next)
    {
      for (other = *head_ptr; other != cur; other = other->next)
	if (INO_T_EQ (cur->ino, other->ino) && cur->dev == other->dev)
	  {
	    cur = remove_dup_dir (pfile, prev, head_ptr);
	    break;
	  }
      prev = cur;
    }

  return prev;
}

/* Merge the four include chains together in the order quote, bracket,
   system, after.  Remove duplicate dirs (as determined by
   INO_T_EQ()).  The system_include and after_include chains are never
   referred to again after this function; all access is through the
   bracket_include path.  */
static void
merge_include_chains (pfile)
     cpp_reader *pfile;
{
  struct search_path *quote, *brack, *systm, *qtail;

  struct cpp_pending *pend = CPP_OPTION (pfile, pending);

  quote = pend->quote_head;
  brack = pend->brack_head;
  systm = pend->systm_head;
  qtail = pend->quote_tail;

  /* Paste together bracket, system, and after include chains.  */
  if (systm)
    pend->systm_tail->next = pend->after_head;
  else
    systm = pend->after_head;

  if (brack)
    pend->brack_tail->next = systm;
  else
    brack = systm;

  /* This is a bit tricky.  First we drop non-system dupes of system
     directories from the merged bracket-include list.  Next we drop
     dupes from the bracket and quote include lists.  Then we drop
     non-system dupes from the merged quote-include list.  Finally,
     if qtail and brack are the same directory, we cut out brack and
     move brack up to point to qtail.

     We can't just merge the lists and then uniquify them because
     then we may lose directories from the <> search path that should
     be there; consider -Ifoo -Ibar -I- -Ifoo -Iquux.  It is however
     safe to treat -Ibar -Ifoo -I- -Ifoo -Iquux as if written
     -Ibar -I- -Ifoo -Iquux.  */

  remove_dup_nonsys_dirs (pfile, &brack, systm);
  remove_dup_dirs (pfile, &brack);

  if (quote)
    {
      qtail = remove_dup_dirs (pfile, &quote);
      qtail->next = brack;

      qtail = remove_dup_nonsys_dirs (pfile, &quote, brack);

      /* If brack == qtail, remove brack as it's simpler.  */
      if (qtail && brack && INO_T_EQ (qtail->ino, brack->ino)
	  && qtail->dev == brack->dev)
	brack = remove_dup_dir (pfile, qtail, &quote);
    }
  else
    quote = brack;

  CPP_OPTION (pfile, quote_include) = quote;
  CPP_OPTION (pfile, bracket_include) = brack;
}

/* A set of booleans indicating what CPP features each source language
   requires.  */
struct lang_flags
{
  char c99;
  char cplusplus;
  char extended_numbers;
  char std;
  char dollars_in_ident;
  char cplusplus_comments;
  char digraphs;
};

/* ??? Enable $ in identifiers in assembly? */
static const struct lang_flags lang_defaults[] =
{ /*              c99 c++ xnum std dollar c++comm digr  */
  /* GNUC89 */  { 0,  0,  1,   0,   1,     1,      1     },
  /* GNUC99 */  { 1,  0,  1,   0,   1,     1,      1     },
  /* STDC89 */  { 0,  0,  0,   1,   0,     0,      0     },
  /* STDC94 */  { 0,  0,  0,   1,   0,     0,      1     },
  /* STDC99 */  { 1,  0,  1,   1,   0,     1,      1     },
  /* GNUCXX */  { 0,  1,  1,   0,   1,     1,      1     },
  /* CXX98  */  { 0,  1,  1,   1,   0,     1,      1     },
  /* ASM    */  { 0,  0,  1,   0,   0,     1,      0     }
};

/* Sets internal flags correctly for a given language.  */
void
cpp_set_lang (pfile, lang)
     cpp_reader *pfile;
     enum c_lang lang;
{
  const struct lang_flags *l = &lang_defaults[(int) lang];

  CPP_OPTION (pfile, lang) = lang;

  CPP_OPTION (pfile, c99)		 = l->c99;
  CPP_OPTION (pfile, cplusplus)		 = l->cplusplus;
  CPP_OPTION (pfile, extended_numbers)	 = l->extended_numbers;
  CPP_OPTION (pfile, std)		 = l->std;
  CPP_OPTION (pfile, trigraphs)		 = l->std;
  CPP_OPTION (pfile, dollars_in_ident)	 = l->dollars_in_ident;
  CPP_OPTION (pfile, cplusplus_comments) = l->cplusplus_comments;
  CPP_OPTION (pfile, digraphs)		 = l->digraphs;
}

#ifdef HOST_EBCDIC
static int opt_comp PARAMS ((const void *, const void *));

/* Run-time sorting of options array.  */
static int
opt_comp (p1, p2)
     const void *p1, *p2;
{
  return strcmp (((struct cl_option *) p1)->opt_text,
		 ((struct cl_option *) p2)->opt_text);
}
#endif

/* init initializes library global state.  It might not need to
   do anything depending on the platform and compiler.  */
static void
init_library ()
{
  static int initialized = 0;

  if (! initialized)
    {
      initialized = 1;

#ifdef HOST_EBCDIC
      /* For non-ASCII hosts, the cl_options array needs to be sorted at
	 runtime.  */
      qsort (cl_options, N_OPTS, sizeof (struct cl_option), opt_comp);
#endif

      /* Set up the trigraph map.  This doesn't need to do anything if
	 we were compiled with a compiler that supports C99 designated
	 initializers.  */
      init_trigraph_map ();
    }
}

/* Initialize a cpp_reader structure.  */
cpp_reader *
cpp_create_reader (lang)
     enum c_lang lang;
{
  cpp_reader *pfile;

  /* Initialize this instance of the library if it hasn't been already.  */
  init_library ();

  pfile = (cpp_reader *) xcalloc (1, sizeof (cpp_reader));

  cpp_set_lang (pfile, lang);
  CPP_OPTION (pfile, warn_import) = 1;
  CPP_OPTION (pfile, warn_multichar) = 1;
  CPP_OPTION (pfile, discard_comments) = 1;
  CPP_OPTION (pfile, discard_comments_in_macro_exp) = 1;
  CPP_OPTION (pfile, show_column) = 1;
  CPP_OPTION (pfile, tabstop) = 8;
  CPP_OPTION (pfile, operator_names) = 1;
  CPP_OPTION (pfile, warn_endif_labels) = 1;
  CPP_OPTION (pfile, warn_long_long) = !CPP_OPTION (pfile, c99);

  CPP_OPTION (pfile, pending) =
    (struct cpp_pending *) xcalloc (1, sizeof (struct cpp_pending));

  /* Default CPP arithmetic to something sensible for the host for the
     benefit of dumb users like fix-header.  */
  CPP_OPTION (pfile, precision) = CHAR_BIT * sizeof (long);
  CPP_OPTION (pfile, char_precision) = CHAR_BIT;
  CPP_OPTION (pfile, wchar_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, int_precision) = CHAR_BIT * sizeof (int);
  CPP_OPTION (pfile, unsigned_char) = 0;
  CPP_OPTION (pfile, unsigned_wchar) = 1;

  /* Initialize the line map.  Start at logical line 1, so we can use
     a line number of zero for special states.  */
  init_line_maps (&pfile->line_maps);
  pfile->line = 1;

  /* Initialize lexer state.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);

  /* Set up static tokens.  */
  pfile->avoid_paste.type = CPP_PADDING;
  pfile->avoid_paste.val.source = NULL;
  pfile->eof.type = CPP_EOF;
  pfile->eof.flags = 0;

  /* Create a token buffer for the lexer.  */
  _cpp_init_tokenrun (&pfile->base_run, 250);
  pfile->cur_run = &pfile->base_run;
  pfile->cur_token = pfile->base_run.base;

  /* Initialize the base context.  */
  pfile->context = &pfile->base_context;
  pfile->base_context.macro = 0;
  pfile->base_context.prev = pfile->base_context.next = 0;

  /* Aligned and unaligned storage.  */
  pfile->a_buff = _cpp_get_buff (pfile, 0);
  pfile->u_buff = _cpp_get_buff (pfile, 0);

  /* The expression parser stack.  */
  _cpp_expand_op_stack (pfile);

  /* Initialize the buffer obstack.  */
  gcc_obstack_init (&pfile->buffer_ob);

  _cpp_init_includes (pfile);

  return pfile;
}

/* Free resources used by PFILE.  Accessing PFILE after this function
   returns leads to undefined behavior.  Returns the error count.  */
void
cpp_destroy (pfile)
     cpp_reader *pfile;
{
  struct search_path *dir, *dirn;
  cpp_context *context, *contextn;
  tokenrun *run, *runn;

  free_chain (CPP_OPTION (pfile, pending)->include_head);
  free (CPP_OPTION (pfile, pending));
  free (pfile->op_stack);

  while (CPP_BUFFER (pfile) != NULL)
    _cpp_pop_buffer (pfile);

  if (pfile->out.base)
    free (pfile->out.base);

  if (pfile->macro_buffer)
    {
      free ((PTR) pfile->macro_buffer);
      pfile->macro_buffer = NULL;
      pfile->macro_buffer_len = 0;
    }

  if (pfile->deps)
    deps_free (pfile->deps);
  obstack_free (&pfile->buffer_ob, 0);

  _cpp_destroy_hashtable (pfile);
  _cpp_cleanup_includes (pfile);

  _cpp_free_buff (pfile->a_buff);
  _cpp_free_buff (pfile->u_buff);
  _cpp_free_buff (pfile->free_buffs);

  for (run = &pfile->base_run; run; run = runn)
    {
      runn = run->next;
      free (run->base);
      if (run != &pfile->base_run)
	free (run);
    }

  for (dir = CPP_OPTION (pfile, quote_include); dir; dir = dirn)
    {
      dirn = dir->next;
      free ((PTR) dir->name);
      free (dir);
    }

  for (context = pfile->base_context.next; context; context = contextn)
    {
      contextn = context->next;
      free (context);
    }

  free_line_maps (&pfile->line_maps);
  free (pfile);
}

/* This structure defines one built-in identifier.  A node will be
   entered in the hash table under the name NAME, with value VALUE.

   There are two tables of these.  builtin_array holds all the
   "builtin" macros: these are handled by builtin_macro() in
   cppmacro.c.  Builtin is somewhat of a misnomer -- the property of
   interest is that these macros require special code to compute their
   expansions.  The value is a "builtin_type" enumerator.

   operator_array holds the C++ named operators.  These are keywords
   which act as aliases for punctuators.  In C++, they cannot be
   altered through #define, and #if recognizes them as operators.  In
   C, these are not entered into the hash table at all (but see
   <iso646.h>).  The value is a token-type enumerator.  */
struct builtin
{
  const uchar *name;
  unsigned short len;
  unsigned short value;
};

#define B(n, t)    { DSC(n), t }
static const struct builtin builtin_array[] =
{
  B("__TIME__",		 BT_TIME),
  B("__DATE__",		 BT_DATE),
  B("__FILE__",		 BT_FILE),
  B("__BASE_FILE__",	 BT_BASE_FILE),
  B("__LINE__",		 BT_SPECLINE),
  B("__INCLUDE_LEVEL__", BT_INCLUDE_LEVEL),
  /* Keep builtins not used for -traditional-cpp at the end, and
     update init_builtins() if any more are added.  */
  B("_Pragma",		 BT_PRAGMA),
  B("__STDC__",		 BT_STDC),
};

static const struct builtin operator_array[] =
{
  B("and",	CPP_AND_AND),
  B("and_eq",	CPP_AND_EQ),
  B("bitand",	CPP_AND),
  B("bitor",	CPP_OR),
  B("compl",	CPP_COMPL),
  B("not",	CPP_NOT),
  B("not_eq",	CPP_NOT_EQ),
  B("or",	CPP_OR_OR),
  B("or_eq",	CPP_OR_EQ),
  B("xor",	CPP_XOR),
  B("xor_eq",	CPP_XOR_EQ)
};
#undef B

/* Mark the C++ named operators in the hash table.  */
static void
mark_named_operators (pfile)
     cpp_reader *pfile;
{
  const struct builtin *b;

  for (b = operator_array;
       b < (operator_array + ARRAY_SIZE (operator_array));
       b++)
    {
      cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
      hp->flags |= NODE_OPERATOR;
      hp->value.operator = b->value;
    }
}

/* Subroutine of cpp_read_main_file; reads the builtins table above and
   enters them, and language-specific macros, into the hash table.  */
static void
init_builtins (pfile)
     cpp_reader *pfile;
{
  const struct builtin *b;
  size_t n = ARRAY_SIZE (builtin_array);

  if (CPP_OPTION (pfile, traditional))
    n -= 2;

  for(b = builtin_array; b < builtin_array + n; b++)
    {
      cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
      hp->type = NT_MACRO;
      hp->flags |= NODE_BUILTIN | NODE_WARN;
      hp->value.builtin = b->value;
    }

  if (CPP_OPTION (pfile, cplusplus))
    _cpp_define_builtin (pfile, "__cplusplus 1");
  else if (CPP_OPTION (pfile, lang) == CLK_ASM)
    _cpp_define_builtin (pfile, "__ASSEMBLER__ 1");
  else if (CPP_OPTION (pfile, lang) == CLK_STDC94)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199409L");
  else if (CPP_OPTION (pfile, c99))
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199901L");

  if (CPP_OPTION (pfile, objc))
    _cpp_define_builtin (pfile, "__OBJC__ 1");

  if (pfile->cb.register_builtins)
    (*pfile->cb.register_builtins) (pfile);
}

/* And another subroutine.  This one sets up the standard include path.  */
static void
init_standard_includes (pfile)
     cpp_reader *pfile;
{
  char *path;
  const struct default_include *p;
  const char *specd_prefix = CPP_OPTION (pfile, include_prefix);

  /* Several environment variables may add to the include search path.
     CPATH specifies an additional list of directories to be searched
     as if specified with -I, while C_INCLUDE_PATH, CPLUS_INCLUDE_PATH,
     etc. specify an additional list of directories to be searched as
     if specified with -isystem, for the language indicated.  */

  GET_ENVIRONMENT (path, "CPATH");
  if (path != 0 && *path != 0)
    path_include (pfile, path, BRACKET);

  switch ((CPP_OPTION (pfile, objc) << 1) + CPP_OPTION (pfile, cplusplus))
    {
    case 0:
      GET_ENVIRONMENT (path, "C_INCLUDE_PATH");
      break;
    case 1:
      GET_ENVIRONMENT (path, "CPLUS_INCLUDE_PATH");
      break;
    case 2:
      GET_ENVIRONMENT (path, "OBJC_INCLUDE_PATH");
      break;
    case 3:
      GET_ENVIRONMENT (path, "OBJCPLUS_INCLUDE_PATH");
      break;
    }
  if (path != 0 && *path != 0)
    path_include (pfile, path, SYSTEM);

  /* Search "translated" versions of GNU directories.
     These have /usr/local/lib/gcc... replaced by specd_prefix.  */
  if (specd_prefix != 0 && cpp_GCC_INCLUDE_DIR_len)
    {
      /* Remove the `include' from /usr/local/lib/gcc.../include.
	 GCC_INCLUDE_DIR will always end in /include.  */
      int default_len = cpp_GCC_INCLUDE_DIR_len;
      char *default_prefix = (char *) alloca (default_len + 1);
      int specd_len = strlen (specd_prefix);

      memcpy (default_prefix, cpp_GCC_INCLUDE_DIR, default_len);
      default_prefix[default_len] = '\0';

      for (p = cpp_include_defaults; p->fname; p++)
	{
	  /* Some standard dirs are only for C++.  */
	  if (!p->cplusplus
	      || (CPP_OPTION (pfile, cplusplus)
		  && !CPP_OPTION (pfile, no_standard_cplusplus_includes)))
	    {
	      /* Does this dir start with the prefix?  */
	      if (!strncmp (p->fname, default_prefix, default_len))
		{
		  /* Yes; change prefix and add to search list.  */
		  int flen = strlen (p->fname);
		  int this_len = specd_len + flen - default_len;
		  char *str = (char *) xmalloc (this_len + 1);
		  memcpy (str, specd_prefix, specd_len);
		  memcpy (str + specd_len,
			  p->fname + default_len,
			  flen - default_len + 1);

		  append_include_chain (pfile, str, SYSTEM, p->cxx_aware);
		}
	    }
	}
    }

  /* Search ordinary names for GNU include directories.  */
  for (p = cpp_include_defaults; p->fname; p++)
    {
      /* Some standard dirs are only for C++.  */
      if (!p->cplusplus
	  || (CPP_OPTION (pfile, cplusplus)
	      && !CPP_OPTION (pfile, no_standard_cplusplus_includes)))
	{
	  char *str = update_path (p->fname, p->component);
	  append_include_chain (pfile, str, SYSTEM, p->cxx_aware);
	}
    }
}

/* Pushes a command line -imacro and -include file indicated by P onto
   the buffer stack.  Returns nonzero if successful.  */
static bool
push_include (pfile, p)
     cpp_reader *pfile;
     struct pending_option *p;
{
  cpp_token header;

  /* Later: maybe update this to use the #include "" search path
     if cpp_read_file fails.  */
  header.type = CPP_STRING;
  header.val.str.text = (const unsigned char *) p->arg;
  header.val.str.len = strlen (p->arg);
  /* Make the command line directive take up a line.  */
  pfile->line++;

  return _cpp_execute_include (pfile, &header, IT_CMDLINE);
}

/* Frees a pending_option chain.  */
static void
free_chain (head)
     struct pending_option *head;
{
  struct pending_option *next;

  while (head)
    {
      next = head->next;
      free (head);
      head = next;
    }
}

/* Sanity-checks are dependent on command-line options, so it is
   called as a subroutine of cpp_read_main_file ().  */
#if ENABLE_CHECKING
static void sanity_checks PARAMS ((cpp_reader *));
static void sanity_checks (pfile)
     cpp_reader *pfile;
{
  cppchar_t test = 0;
  size_t max_precision = 2 * CHAR_BIT * sizeof (cpp_num_part);

  /* Sanity checks for assumptions about CPP arithmetic and target
     type precisions made by cpplib.  */
  test--;
  if (test < 1)
    cpp_error (pfile, DL_ICE, "cppchar_t must be an unsigned type");

  if (CPP_OPTION (pfile, precision) > max_precision)
    cpp_error (pfile, DL_ICE,
	       "preprocessor arithmetic has maximum precision of %lu bits; target requires %lu bits",
	       (unsigned long) max_precision,
	       (unsigned long) CPP_OPTION (pfile, precision));

  if (CPP_OPTION (pfile, precision) < CPP_OPTION (pfile, int_precision))
    cpp_error (pfile, DL_ICE,
	       "CPP arithmetic must be at least as precise as a target int");

  if (CPP_OPTION (pfile, char_precision) < 8)
    cpp_error (pfile, DL_ICE, "target char is less than 8 bits wide");

  if (CPP_OPTION (pfile, wchar_precision) < CPP_OPTION (pfile, char_precision))
    cpp_error (pfile, DL_ICE,
	       "target wchar_t is narrower than target char");

  if (CPP_OPTION (pfile, int_precision) < CPP_OPTION (pfile, char_precision))
    cpp_error (pfile, DL_ICE,
	       "target int is narrower than target char");

  /* This is assumed in eval_token() and could be fixed if necessary.  */
  if (sizeof (cppchar_t) > sizeof (cpp_num_part))
    cpp_error (pfile, DL_ICE, "CPP half-integer narrower than CPP character");

  if (CPP_OPTION (pfile, wchar_precision) > BITS_PER_CPPCHAR_T)
    cpp_error (pfile, DL_ICE,
	       "CPP on this host cannot handle wide character constants over %lu bits, but the target requires %lu bits",
	       (unsigned long) BITS_PER_CPPCHAR_T,
	       (unsigned long) CPP_OPTION (pfile, wchar_precision));
}
#else
# define sanity_checks(PFILE)
#endif

/* Add a dependency target.  Can be called any number of times before
   cpp_read_main_file().  If no targets have been added before
   cpp_read_main_file(), then the default target is used.  */
void
cpp_add_dependency_target (pfile, target, quote)
     cpp_reader *pfile;
     const char *target;
     int quote;
{
  if (!pfile->deps)
    pfile->deps = deps_init ();

  deps_add_target (pfile->deps, target, quote);
}

/* This is called after options have been parsed, and partially
   processed.  Setup for processing input from the file named FNAME,
   or stdin if it is the empty string.  Return the original filename
   on success (e.g. foo.i->foo.c), or NULL on failure.  */
const char *
cpp_read_main_file (pfile, fname, table)
     cpp_reader *pfile;
     const char *fname;
     hash_table *table;
{
  sanity_checks (pfile);

  post_options (pfile);

  /* The front ends don't set up the hash table until they have
     finished processing the command line options, so initializing the
     hashtable is deferred until now.  */
  _cpp_init_hashtable (pfile, table);

  /* Set up the include search path now.  */
  if (! CPP_OPTION (pfile, no_standard_includes))
    init_standard_includes (pfile);

  merge_include_chains (pfile);

  /* With -v, print the list of dirs to search.  */
  if (CPP_OPTION (pfile, verbose))
    {
      struct search_path *l;
      fprintf (stderr, _("#include \"...\" search starts here:\n"));
      for (l = CPP_OPTION (pfile, quote_include); l; l = l->next)
	{
	  if (l == CPP_OPTION (pfile, bracket_include))
	    fprintf (stderr, _("#include <...> search starts here:\n"));
	  fprintf (stderr, " %s\n", l->name);
	}
      fprintf (stderr, _("End of search list.\n"));
    }

  if (CPP_OPTION (pfile, deps.style) != DEPS_NONE)
    {
      if (!pfile->deps)
	pfile->deps = deps_init ();

      /* Set the default target (if there is none already).  */
      deps_add_default_target (pfile->deps, fname);
    }

  /* Open the main input file.  */
  if (!_cpp_read_file (pfile, fname))
    return NULL;

  /* Set this here so the client can change the option if it wishes,
     and after stacking the main file so we don't trace the main
     file.  */
  pfile->line_maps.trace_includes = CPP_OPTION (pfile, print_include_names);

  /* For foo.i, read the original filename foo.c now, for the benefit
     of the front ends.  */
  if (CPP_OPTION (pfile, preprocessed))
    read_original_filename (pfile);

  return pfile->map->to_file;
}

/* For preprocessed files, if the first tokens are of the form # NUM.
   handle the directive so we know the original file name.  This will
   generate file_change callbacks, which the front ends must handle
   appropriately given their state of initialization.  */
static void
read_original_filename (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token, *token1;

  /* Lex ahead; if the first tokens are of the form # NUM, then
     process the directive, otherwise back up.  */
  token = _cpp_lex_direct (pfile);
  if (token->type == CPP_HASH)
    {
      token1 = _cpp_lex_direct (pfile);
      _cpp_backup_tokens (pfile, 1);

      /* If it's a #line directive, handle it.  */
      if (token1->type == CPP_NUMBER)
	{
	  _cpp_handle_directive (pfile, token->flags & PREV_WHITE);
	  return;
	}
    }

  /* Backup as if nothing happened.  */
  _cpp_backup_tokens (pfile, 1);
}

/* Handle pending command line options: -D, -U, -A, -imacros and
   -include.  This should be called after debugging has been properly
   set up in the front ends.  */
void
cpp_finish_options (pfile)
     cpp_reader *pfile;
{
  /* Mark named operators before handling command line macros.  */
  if (CPP_OPTION (pfile, cplusplus) && CPP_OPTION (pfile, operator_names))
    mark_named_operators (pfile);

  /* Install builtins and process command line macros etc. in the order
     they appeared, but only if not already preprocessed.  */
  if (! CPP_OPTION (pfile, preprocessed))
    {
      struct pending_option *p;

      /* Prevent -Wunused-macros with command-line redefinitions.  */
      pfile->first_unused_line = (unsigned int) -1;
      _cpp_do_file_change (pfile, LC_RENAME, _("<built-in>"), 1, 0);
      init_builtins (pfile);
      _cpp_do_file_change (pfile, LC_RENAME, _("<command line>"), 1, 0);
      for (p = CPP_OPTION (pfile, pending)->directive_head; p; p = p->next)
	(*p->handler) (pfile, p->arg);

      /* Scan -imacros files after -D, -U, but before -include.
	 pfile->next_include_file is NULL, so _cpp_pop_buffer does not
	 push -include files.  */
      for (p = CPP_OPTION (pfile, pending)->imacros_head; p; p = p->next)
	if (push_include (pfile, p))
	  cpp_scan_nooutput (pfile);

      pfile->next_include_file = &CPP_OPTION (pfile, pending)->include_head;
      _cpp_maybe_push_include_file (pfile);
    }

  pfile->first_unused_line = pfile->line;

  free_chain (CPP_OPTION (pfile, pending)->imacros_head);
  free_chain (CPP_OPTION (pfile, pending)->directive_head);
}

/* Push the next buffer on the stack given by -include, if any.  */
void
_cpp_maybe_push_include_file (pfile)
     cpp_reader *pfile;
{
  if (pfile->next_include_file)
    {
      struct pending_option *head = *pfile->next_include_file;

      while (head && !push_include (pfile, head))
	head = head->next;

      if (head)
	pfile->next_include_file = &head->next;
      else
	{
	  /* All done; restore the line map from <command line>.  */
	  _cpp_do_file_change (pfile, LC_RENAME,
			       pfile->line_maps.maps[0].to_file, 1, 0);
	  /* Don't come back here again.  */
	  pfile->next_include_file = NULL;
	}
    }
}

/* This is called at the end of preprocessing.  It pops the last
   buffer and writes dependency output, and returns the number of
   errors.
 
   Maybe it should also reset state, such that you could call
   cpp_start_read with a new filename to restart processing.  */
int
cpp_finish (pfile, deps_stream)
     cpp_reader *pfile;
     FILE *deps_stream;
{
  /* Warn about unused macros before popping the final buffer.  */
  if (CPP_OPTION (pfile, warn_unused_macros))
    cpp_forall_identifiers (pfile, _cpp_warn_if_unused_macro, NULL);

  /* cpplex.c leaves the final buffer on the stack.  This it so that
     it returns an unending stream of CPP_EOFs to the client.  If we
     popped the buffer, we'd dereference a NULL buffer pointer and
     segfault.  It's nice to allow the client to do worry-free excess
     cpp_get_token calls.  */
  while (pfile->buffer)
    _cpp_pop_buffer (pfile);

  /* Don't write the deps file if there are errors.  */
  if (CPP_OPTION (pfile, deps.style) != DEPS_NONE
      && deps_stream && pfile->errors == 0)
    {
      deps_write (pfile->deps, deps_stream, 72);

      if (CPP_OPTION (pfile, deps.phony_targets))
	deps_phony_targets (pfile->deps, deps_stream);
    }

  /* Report on headers that could use multiple include guards.  */
  if (CPP_OPTION (pfile, print_include_names))
    _cpp_report_missing_guards (pfile);

  return pfile->errors;
}

/* Add a directive to be handled later in the initialization phase.  */
static void
new_pending_directive (pend, text, handler)
     struct cpp_pending *pend;
     const char *text;
     cl_directive_handler handler;
{
  struct pending_option *o = (struct pending_option *)
    xmalloc (sizeof (struct pending_option));

  o->arg = text;
  o->next = NULL;
  o->handler = handler;
  APPEND (pend, directive, o);
}

/* Irix6 "cc -n32" and OSF4 cc have problems with char foo[] = ("string");
   I.e. a const string initializer with parens around it.  That is
   what N_("string") resolves to, so we make no_* be macros instead.  */
#define no_ass N_("assertion missing after %s")
#define no_dir N_("directory name missing after %s")
#define no_fil N_("file name missing after %s")
#define no_mac N_("macro name missing after %s")
#define no_pth N_("path name missing after %s")

/* This is the list of all command line options, with the leading
   "-" removed.  It must be sorted in ASCII collating order.  */
#define COMMAND_LINE_OPTIONS                                                  \
  DEF_OPT("A",                        no_ass, OPT_A)                          \
  DEF_OPT("D",                        no_mac, OPT_D)                          \
  DEF_OPT("I",                        no_dir, OPT_I)                          \
  DEF_OPT("U",                        no_mac, OPT_U)                          \
  DEF_OPT("idirafter",                no_dir, OPT_idirafter)                  \
  DEF_OPT("imacros",                  no_fil, OPT_imacros)                    \
  DEF_OPT("include",                  no_fil, OPT_include)                    \
  DEF_OPT("iprefix",                  no_pth, OPT_iprefix)                    \
  DEF_OPT("isystem",                  no_dir, OPT_isystem)                    \
  DEF_OPT("iwithprefix",              no_dir, OPT_iwithprefix)                \
  DEF_OPT("iwithprefixbefore",        no_dir, OPT_iwithprefixbefore)

#define DEF_OPT(text, msg, code) code,
enum opt_code
{
  COMMAND_LINE_OPTIONS
  N_OPTS
};
#undef DEF_OPT

struct cl_option
{
  const char *opt_text;
  const char *msg;
  size_t opt_len;
  enum opt_code opt_code;
};

#define DEF_OPT(text, msg, code) { text, msg, sizeof(text) - 1, code },
#ifdef HOST_EBCDIC
static struct cl_option cl_options[] =
#else
static const struct cl_option cl_options[] =
#endif
{
  COMMAND_LINE_OPTIONS
};
#undef DEF_OPT
#undef COMMAND_LINE_OPTIONS

/* Perform a binary search to find which, if any, option the given
   command-line matches.  Returns its index in the option array,
   negative on failure.  Complications arise since some options can be
   suffixed with an argument, and multiple complete matches can occur,
   e.g. -pedantic and -pedantic-errors.  */
static int
parse_option (input)
     const char *input;
{
  unsigned int md, mn, mx;
  size_t opt_len;
  int comp;

  mn = 0;
  mx = N_OPTS;

  while (mx > mn)
    {
      md = (mn + mx) / 2;

      opt_len = cl_options[md].opt_len;
      comp = strncmp (input, cl_options[md].opt_text, opt_len);

      if (comp > 0)
	mn = md + 1;
      else if (comp < 0)
	mx = md;
      else
	{
	  if (input[opt_len] == '\0')
	    return md;
	  /* We were passed more text.  If the option takes an argument,
	     we may match a later option or we may have been passed the
	     argument.  The longest possible option match succeeds.
	     If the option takes no arguments we have not matched and
	     continue the search (e.g. input="stdc++" match was "stdc").  */
	  mn = md + 1;
	  if (cl_options[md].msg)
	    {
	      /* Scan forwards.  If we get an exact match, return it.
		 Otherwise, return the longest option-accepting match.
		 This loops no more than twice with current options.  */
	      mx = md;
	      for (; mn < (unsigned int) N_OPTS; mn++)
		{
		  opt_len = cl_options[mn].opt_len;
		  if (strncmp (input, cl_options[mn].opt_text, opt_len))
		    break;
		  if (input[opt_len] == '\0')
		    return mn;
		  if (cl_options[mn].msg)
		    mx = mn;
		}
	      return mx;
	    }
	}
    }

  return -1;
}

/* Handle one command-line option in (argc, argv).
   Can be called multiple times, to handle multiple sets of options.
   Returns number of strings consumed.  */
int
cpp_handle_option (pfile, argc, argv)
     cpp_reader *pfile;
     int argc;
     char **argv;
{
  int i = 0;
  struct cpp_pending *pend = CPP_OPTION (pfile, pending);

    {
      enum opt_code opt_code;
      int opt_index;
      const char *arg = 0;

      /* Skip over '-'.  */
      opt_index = parse_option (&argv[i][1]);
      if (opt_index < 0)
	return i;

      opt_code = cl_options[opt_index].opt_code;
      if (cl_options[opt_index].msg)
	{
	  arg = &argv[i][cl_options[opt_index].opt_len + 1];
	  if (arg[0] == '\0')
	    {
	      arg = argv[++i];
	      if (!arg)
		{
		  cpp_error (pfile, DL_ERROR,
			     cl_options[opt_index].msg, argv[i - 1]);
		  return argc;
		}
	    }
	}

      switch (opt_code)
	{
	case N_OPTS: /* Shut GCC up.  */
	  break;

	case OPT_D:
	  new_pending_directive (pend, arg, cpp_define);
	  break;
	case OPT_iprefix:
	  CPP_OPTION (pfile, include_prefix) = arg;
	  CPP_OPTION (pfile, include_prefix_len) = strlen (arg);
	  break;

	case OPT_A:
	  if (arg[0] == '-')
	    new_pending_directive (pend, arg + 1, cpp_unassert);
	  else
	    new_pending_directive (pend, arg, cpp_assert);
	  break;
	case OPT_U:
	  new_pending_directive (pend, arg, cpp_undef);
	  break;
	case OPT_I:           /* Add directory to path for includes.  */
	  if (!strcmp (arg, "-"))
	    {
	      /* -I- means:
		 Use the preceding -I directories for #include "..."
		 but not #include <...>.
		 Don't search the directory of the present file
		 for #include "...".  (Note that -I. -I- is not the same as
		 the default setup; -I. uses the compiler's working dir.)  */
	      if (! CPP_OPTION (pfile, ignore_srcdir))
		{
		  pend->quote_head = pend->brack_head;
		  pend->quote_tail = pend->brack_tail;
		  pend->brack_head = 0;
		  pend->brack_tail = 0;
		  CPP_OPTION (pfile, ignore_srcdir) = 1;
		}
	      else
		{
		  cpp_error (pfile, DL_ERROR, "-I- specified twice");
		  return argc;
		}
	    }
	  else
	    append_include_chain (pfile, xstrdup (arg), BRACKET, 0);
	  break;
	case OPT_isystem:
	  /* Add directory to beginning of system include path, as a system
	     include directory.  */
	  append_include_chain (pfile, xstrdup (arg), SYSTEM, 0);
	  break;
	case OPT_include:
	case OPT_imacros:
	  {
	    struct pending_option *o = (struct pending_option *)
	      xmalloc (sizeof (struct pending_option));
	    o->arg = arg;
	    o->next = NULL;

	    if (opt_code == OPT_include)
	      APPEND (pend, include, o);
	    else
	      APPEND (pend, imacros, o);
	  }
	  break;
	case OPT_iwithprefix:
	  /* Add directory to end of path for includes,
	     with the default prefix at the front of its name.  */
	  /* fall through */
	case OPT_iwithprefixbefore:
	  /* Add directory to main path for includes,
	     with the default prefix at the front of its name.  */
	  {
	    char *fname;
	    int len;

	    len = strlen (arg);

	    if (CPP_OPTION (pfile, include_prefix) != 0)
	      {
		size_t ipl = CPP_OPTION (pfile, include_prefix_len);
		fname = xmalloc (ipl + len + 1);
		memcpy (fname, CPP_OPTION (pfile, include_prefix), ipl);
		memcpy (fname + ipl, arg, len + 1);
	      }
	    else if (cpp_GCC_INCLUDE_DIR_len)
	      {
		fname = xmalloc (cpp_GCC_INCLUDE_DIR_len + len + 1);
		memcpy (fname, cpp_GCC_INCLUDE_DIR, cpp_GCC_INCLUDE_DIR_len);
		memcpy (fname + cpp_GCC_INCLUDE_DIR_len, arg, len + 1);
	      }
	    else
	      fname = xstrdup (arg);

	    append_include_chain (pfile, fname,
			  opt_code == OPT_iwithprefix ? SYSTEM: BRACKET, 0);
	  }
	  break;
	case OPT_idirafter:
	  /* Add directory to end of path for includes.  */
	  append_include_chain (pfile, xstrdup (arg), AFTER, 0);
	  break;
	}
    }
  return i + 1;
}

/* Handle command-line options in (argc, argv).
   Can be called multiple times, to handle multiple sets of options.
   Returns if an unrecognized option is seen.
   Returns number of strings consumed.  */
int
cpp_handle_options (pfile, argc, argv)
     cpp_reader *pfile;
     int argc;
     char **argv;
{
  int i;
  int strings_processed;

  for (i = 0; i < argc; i += strings_processed)
    {
      strings_processed = cpp_handle_option (pfile, argc - i, argv + i);
      if (strings_processed == 0)
	break;
    }

  return i;
}

static void
post_options (pfile)
     cpp_reader *pfile;
{
  /* -Wtraditional is not useful in C++ mode.  */
  if (CPP_OPTION (pfile, cplusplus))
    CPP_OPTION (pfile, warn_traditional) = 0;

  /* Permanently disable macro expansion if we are rescanning
     preprocessed text.  Read preprocesed source in ISO mode.  */
  if (CPP_OPTION (pfile, preprocessed))
    {
      pfile->state.prevent_expansion = 1;
      CPP_OPTION (pfile, traditional) = 0;
    }

  /* Traditional CPP does not accurately track column information.  */
  if (CPP_OPTION (pfile, traditional))
    CPP_OPTION (pfile, show_column) = 0;
}
