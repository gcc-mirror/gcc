/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.
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
#include "output.h"
#include "prefix.h"
#include "intl.h"
#include "version.h"
#include "mkdeps.h"
#include "cppdefault.h"

/* Predefined symbols, built-in macros, and the default include path.  */

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME)	do { (VAR) = getenv (NAME); } while (0)
#endif

/* Windows does not natively support inodes, and neither does MSDOS.
   Cygwin's emulation can generate non-unique inodes, so don't use it.
   VMS has non-numeric inodes. */
#ifdef VMS
# define INO_T_EQ(a, b) (!memcmp (&(a), &(b), sizeof (a)))
#else
# if (defined _WIN32 && ! defined (_UWIN)) || defined __MSDOS__
#  define INO_T_EQ(a, b) 0
# else
#  define INO_T_EQ(a, b) ((a) == (b))
# endif
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
   actually processed until we hit cpp_start_read.  It consists of
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

static void print_help                  PARAMS ((void));
static void path_include		PARAMS ((cpp_reader *,
						 char *, int));
static void init_library		PARAMS ((void));
static void init_builtins		PARAMS ((cpp_reader *));
static void append_include_chain	PARAMS ((cpp_reader *,
						 char *, int, int));
struct search_path * remove_dup_dir	PARAMS ((cpp_reader *,
						 struct search_path *));
struct search_path * remove_dup_dirs PARAMS ((cpp_reader *,
						 struct search_path *));
static void merge_include_chains	PARAMS ((cpp_reader *));
static void do_includes			PARAMS ((cpp_reader *,
						 struct pending_option *,
						 int));
static void set_lang			PARAMS ((cpp_reader *, enum c_lang));
static void init_dependency_output	PARAMS ((cpp_reader *));
static void init_standard_includes	PARAMS ((cpp_reader *));
static void new_pending_directive	PARAMS ((struct cpp_pending *,
						 const char *,
						 cl_directive_handler));
static void output_deps			PARAMS ((cpp_reader *));
static int parse_option			PARAMS ((const char *));

/* Fourth argument to append_include_chain: chain to use.
   Note it's never asked to append to the quote chain.  */
enum { BRACKET = 0, SYSTEM, AFTER };

/* If we have designated initializers (GCC >2.7) these tables can be
   initialized, constant data.  Otherwise, they have to be filled in at
   runtime.  */
#if HAVE_DESIGNATED_INITIALIZERS

#define init_trigraph_map()  /* Nothing.  */
#define TRIGRAPH_MAP \
__extension__ const U_CHAR _cpp_trigraph_map[UCHAR_MAX + 1] = {

#define END };
#define s(p, v) [p] = v,

#else

#define TRIGRAPH_MAP U_CHAR _cpp_trigraph_map[UCHAR_MAX + 1] = { 0 }; \
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

      append_include_chain (pfile, name, path, 0);

      /* Advance past this name.  */
      if (*q == 0)
	break;
      p = q + 1;
    }
  while (1);
}

/* Append DIR to include path PATH.  DIR must be permanently allocated
   and writable. */
static void
append_include_chain (pfile, dir, path, cxx_aware)
     cpp_reader *pfile;
     char *dir;
     int path;
     int cxx_aware ATTRIBUTE_UNUSED;
{
  struct cpp_pending *pend = CPP_OPTION (pfile, pending);
  struct search_path *new;
  struct stat st;
  unsigned int len;

  if (*dir == '\0')
    dir = xstrdup (".");
  _cpp_simplify_pathname (dir);
  if (stat (dir, &st))
    {
      /* Dirs that don't exist are silently ignored.  */
      if (errno != ENOENT)
	cpp_notice_from_errno (pfile, dir);
      else if (CPP_OPTION (pfile, verbose))
	fprintf (stderr, _("ignoring nonexistent directory \"%s\"\n"), dir);
      return;
    }

  if (!S_ISDIR (st.st_mode))
    {
      cpp_notice (pfile, "%s: Not a directory", dir);
      return;
    }

  len = strlen (dir);
  if (len > pfile->max_include_len)
    pfile->max_include_len = len;

  new = (struct search_path *) xmalloc (sizeof (struct search_path));
  new->name = dir;
  new->len = len;
  new->ino  = st.st_ino;
  new->dev  = st.st_dev;
  /* Both systm and after include file lists should be treated as system
     include files since these two lists are really just a concatenation
     of one "system" list. */
  if (path == SYSTEM || path == AFTER)
#ifdef NO_IMPLICIT_EXTERN_C
    new->sysp = 1;
#else
    new->sysp = cxx_aware ? 1 : 2;
#endif
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
   before the duplicate.  The duplicate is removed from the chain and
   freed.  Returns PREV.  */
struct search_path *
remove_dup_dir (pfile, prev)
     cpp_reader *pfile;
     struct search_path *prev;
{
  struct search_path *cur = prev->next;

  if (CPP_OPTION (pfile, verbose))
    fprintf (stderr, _("ignoring duplicate directory \"%s\"\n"), cur->name);

  prev->next = cur->next;
  free ((PTR) cur->name);
  free (cur);

  return prev;
}

/* Remove duplicate directories from a chain.  Returns the tail of the
   chain, or NULL if the chain is empty.  This algorithm is quadratic
   in the number of -I switches, which is acceptable since there
   aren't usually that many of them.  */
struct search_path *
remove_dup_dirs (pfile, head)
     cpp_reader *pfile;
     struct search_path *head;
{
  struct search_path *prev = NULL, *cur, *other;

  for (cur = head; cur; cur = cur->next)
    {
      for (other = head; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino) && cur->dev == other->dev)
	  {
	    cur = remove_dup_dir (pfile, prev);
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
   bracket_include path.

   For the future: Check if the directory is empty (but
   how?) and possibly preload the include hash.  */

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

  /* This is a bit tricky.  First we drop dupes from the quote-include
     list.  Then we drop dupes from the bracket-include list.
     Finally, if qtail and brack are the same directory, we cut out
     brack and move brack up to point to qtail.

     We can't just merge the lists and then uniquify them because
     then we may lose directories from the <> search path that should
     be there; consider -Ifoo -Ibar -I- -Ifoo -Iquux. It is however
     safe to treat -Ibar -Ifoo -I- -Ifoo -Iquux as if written
     -Ibar -I- -Ifoo -Iquux.  */

  remove_dup_dirs (pfile, brack);
  qtail = remove_dup_dirs (pfile, quote);

  if (quote)
    {
      qtail->next = brack;

      /* If brack == qtail, remove brack as it's simpler.  */
      if (INO_T_EQ (qtail->ino, brack->ino) && qtail->dev == brack->dev)
	brack = remove_dup_dir (pfile, qtail);
    }
  else
      quote = brack;

  CPP_OPTION (pfile, quote_include) = quote;
  CPP_OPTION (pfile, bracket_include) = brack;
}

/* Sets internal flags correctly for a given language, and defines
   macros if necessary.  */
static void
set_lang (pfile, lang)
     cpp_reader *pfile;
     enum c_lang lang;
{
  /* Defaults.  */
  CPP_OPTION (pfile, lang) = lang;
  CPP_OPTION (pfile, objc) = 0;
  CPP_OPTION (pfile, cplusplus) = 0;
  CPP_OPTION (pfile, extended_numbers) = 1; /* Allowed in GNU C and C99.  */

  switch (lang)
    {
      /* GNU C.  */
    case CLK_GNUC99:
      CPP_OPTION (pfile, trigraphs) = 0;
      CPP_OPTION (pfile, dollars_in_ident) = 1;
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 1;
      CPP_OPTION (pfile, c99) = 1;
      break;
    case CLK_GNUC89:
      CPP_OPTION (pfile, trigraphs) = 0;
      CPP_OPTION (pfile, dollars_in_ident) = 1;
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 1;
      CPP_OPTION (pfile, c99) = 0;
      break;

      /* ISO C.  */
    case CLK_STDC94:
    case CLK_STDC89:
      CPP_OPTION (pfile, trigraphs) = 1;
      CPP_OPTION (pfile, dollars_in_ident) = 0;
      CPP_OPTION (pfile, cplusplus_comments) = 0;
      CPP_OPTION (pfile, digraphs) = lang == CLK_STDC94;
      CPP_OPTION (pfile, c99) = 0;
      CPP_OPTION (pfile, extended_numbers) = 0;
      break;
    case CLK_STDC99:
      CPP_OPTION (pfile, trigraphs) = 1;
      CPP_OPTION (pfile, dollars_in_ident) = 0;
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 1;
      CPP_OPTION (pfile, c99) = 1;
      break;

      /* Objective C.  */
    case CLK_OBJCXX:
      CPP_OPTION (pfile, cplusplus) = 1;
    case CLK_OBJC:
      CPP_OPTION (pfile, trigraphs) = 0;
      CPP_OPTION (pfile, dollars_in_ident) = 1;
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 1;
      CPP_OPTION (pfile, c99) = 0;
      CPP_OPTION (pfile, objc) = 1;
      break;

      /* C++.  */
    case CLK_GNUCXX:
    case CLK_CXX98:
      CPP_OPTION (pfile, cplusplus) = 1;
      CPP_OPTION (pfile, trigraphs) = lang == CLK_CXX98;
      CPP_OPTION (pfile, dollars_in_ident) = lang == CLK_GNUCXX;
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 1;
      CPP_OPTION (pfile, c99) = 0;
      break;

      /* Assembler.  */
    case CLK_ASM:
      CPP_OPTION (pfile, trigraphs) = 0;
      CPP_OPTION (pfile, dollars_in_ident) = 0;	/* Maybe not?  */
      CPP_OPTION (pfile, cplusplus_comments) = 1;
      CPP_OPTION (pfile, digraphs) = 0; 
      CPP_OPTION (pfile, c99) = 0;
      break;
    }
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

/* Initialize a cpp_reader structure. */
cpp_reader *
cpp_create_reader (table, lang)
     hash_table *table;
     enum c_lang lang;
{
  struct spec_nodes *s;
  cpp_reader *pfile;

  /* Initialise this instance of the library if it hasn't been already.  */
  init_library ();

  pfile = (cpp_reader *) xcalloc (1, sizeof (cpp_reader));

  set_lang (pfile, lang);
  CPP_OPTION (pfile, warn_import) = 1;
  CPP_OPTION (pfile, discard_comments) = 1;
  CPP_OPTION (pfile, show_column) = 1;
  CPP_OPTION (pfile, tabstop) = 8;
  CPP_OPTION (pfile, operator_names) = 1;

  CPP_OPTION (pfile, pending) =
    (struct cpp_pending *) xcalloc (1, sizeof (struct cpp_pending));

  /* It's simplest to just create this struct whether or not it will
     be needed.  */
  pfile->deps = deps_init ();

  /* Initialize lexer state.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);

  /* Indicate date and time not yet calculated.  */
  pfile->date.type = CPP_EOF;

  /* Initialise the base context.  */
  pfile->context = &pfile->base_context;
  pfile->base_context.macro = 0;
  pfile->base_context.prev = pfile->base_context.next = 0;

  /* Identifier pool initially 8K.  Unaligned, permanent pool.  */
  _cpp_init_pool (&pfile->ident_pool, 8 * 1024, 1, 0);

  /* Argument pool initially 8K.  Aligned, temporary pool.  */
  _cpp_init_pool (&pfile->argument_pool, 8 * 1024, 0, 1);

  /* Macro pool initially 8K.  Aligned, permanent pool.  */
  _cpp_init_pool (&pfile->macro_pool, 8 * 1024, 0, 0);

  /* Initialise the buffer obstack.  */
  gcc_obstack_init (&pfile->buffer_ob);

  /* Initialise the hashtable.  */
  _cpp_init_hashtable (pfile, table);

  _cpp_init_directives (pfile);
  _cpp_init_includes (pfile);
  _cpp_init_internal_pragmas (pfile);

  /* Initialize the special nodes.  */
  s = &pfile->spec_nodes;
  s->n_L                = cpp_lookup (pfile, DSC("L"));
  s->n_defined		= cpp_lookup (pfile, DSC("defined"));
  s->n_true		= cpp_lookup (pfile, DSC("true"));
  s->n_false		= cpp_lookup (pfile, DSC("false"));
  s->n__Pragma		= cpp_lookup (pfile, DSC("_Pragma"));
  s->n__STRICT_ANSI__   = cpp_lookup (pfile, DSC("__STRICT_ANSI__"));
  s->n__CHAR_UNSIGNED__ = cpp_lookup (pfile, DSC("__CHAR_UNSIGNED__"));
  s->n__VA_ARGS__       = cpp_lookup (pfile, DSC("__VA_ARGS__"));
  s->n__VA_ARGS__->flags |= NODE_DIAGNOSTIC;

  return pfile;
}

/* Free resources used by PFILE.  Accessing PFILE after this function
   returns leads to undefined behaviour.  */
int
cpp_destroy (pfile)
     cpp_reader *pfile;
{
  int result;
  struct search_path *dir, *dirn;
  cpp_context *context, *contextn;

  while (CPP_BUFFER (pfile) != NULL)
    cpp_pop_buffer (pfile);

  if (pfile->macro_buffer)
    {
      free ((PTR) pfile->macro_buffer);
      pfile->macro_buffer = NULL;
      pfile->macro_buffer_len = 0;
    }

  deps_free (pfile->deps);
  obstack_free (&pfile->buffer_ob, 0);

  _cpp_destroy_hashtable (pfile);
  _cpp_cleanup_includes (pfile);
  _cpp_free_lookaheads (pfile);

  _cpp_free_pool (&pfile->ident_pool);
  _cpp_free_pool (&pfile->macro_pool);
  _cpp_free_pool (&pfile->argument_pool);

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

  result = pfile->errors;
  free (pfile);

  return result;
}


/* This structure defines one built-in identifier.  A node will be
   entered in the hash table under the name NAME, with value VALUE (if
   any).  If flags has OPERATOR, the node's operator field is used; if
   flags has BUILTIN the node's builtin field is used.  Macros that are
   known at build time should not be flagged BUILTIN, as then they do
   not appear in macro dumps with e.g. -dM or -dD.

   Two values are not compile time constants, so we tag
   them in the FLAGS field instead:
   VERS		value is the global version_string, quoted
   ULP		value is the global user_label_prefix

   Also, macros with CPLUS set in the flags field are entered only for C++.  */

struct builtin
{
  const U_CHAR *name;
  const char *value;
  unsigned char builtin;
  unsigned char operator;
  unsigned short flags;
  unsigned short len;
};
#define VERS		0x01
#define ULP		0x02
#define CPLUS		0x04
#define BUILTIN		0x08
#define OPERATOR  	0x10

#define B(n, t)       { U n, 0, t, 0, BUILTIN, sizeof n - 1 }
#define C(n, v)       { U n, v, 0, 0, 0, sizeof n - 1 }
#define X(n, f)       { U n, 0, 0, 0, f, sizeof n - 1 }
#define O(n, c, f)    { U n, 0, 0, c, OPERATOR | f, sizeof n - 1 }
static const struct builtin builtin_array[] =
{
  B("__TIME__",		 BT_TIME),
  B("__DATE__",		 BT_DATE),
  B("__FILE__",		 BT_FILE),
  B("__BASE_FILE__",	 BT_BASE_FILE),
  B("__LINE__",		 BT_SPECLINE),
  B("__INCLUDE_LEVEL__", BT_INCLUDE_LEVEL),

  X("__VERSION__",		VERS),
  X("__USER_LABEL_PREFIX__",	ULP),
  C("__REGISTER_PREFIX__",	REGISTER_PREFIX),
  C("__HAVE_BUILTIN_SETJMP__",	"1"),
#ifndef NO_BUILTIN_SIZE_TYPE
  C("__SIZE_TYPE__",		SIZE_TYPE),
#endif
#ifndef NO_BUILTIN_PTRDIFF_TYPE
  C("__PTRDIFF_TYPE__",		PTRDIFF_TYPE),
#endif
#ifndef NO_BUILTIN_WCHAR_TYPE
  C("__WCHAR_TYPE__",		WCHAR_TYPE),
#endif
#ifndef NO_BUILTIN_WINT_TYPE
  C("__WINT_TYPE__",		WINT_TYPE),
#endif
#ifdef STDC_0_IN_SYSTEM_HEADERS
  B("__STDC__",		 BT_STDC),
#else
  C("__STDC__",		 "1"),
#endif

  /* Named operators known to the preprocessor.  These cannot be #defined
     and always have their stated meaning.  They are treated like normal
     identifiers except for the type code and the meaning.  Most of them
     are only for C++ (but see iso646.h).  */
  O("and",	CPP_AND_AND, CPLUS),
  O("and_eq",	CPP_AND_EQ,  CPLUS),
  O("bitand",	CPP_AND,     CPLUS),
  O("bitor",	CPP_OR,      CPLUS),
  O("compl",	CPP_COMPL,   CPLUS),
  O("not",	CPP_NOT,     CPLUS),
  O("not_eq",	CPP_NOT_EQ,  CPLUS),
  O("or",	CPP_OR_OR,   CPLUS),
  O("or_eq",	CPP_OR_EQ,   CPLUS),
  O("xor",	CPP_XOR,     CPLUS),
  O("xor_eq",	CPP_XOR_EQ,  CPLUS)
};
#undef B
#undef C
#undef X
#undef O
#define builtin_array_end \
 builtin_array + sizeof(builtin_array)/sizeof(struct builtin)

/* Subroutine of cpp_start_read; reads the builtins table above and
   enters the macros into the hash table.  */
static void
init_builtins (pfile)
     cpp_reader *pfile;
{
  const struct builtin *b;

  for(b = builtin_array; b < builtin_array_end; b++)
    {
      if ((b->flags & CPLUS) && ! CPP_OPTION (pfile, cplusplus))
	continue;

      if ((b->flags & OPERATOR) && ! CPP_OPTION (pfile, operator_names))
	continue;

      if (b->flags & (OPERATOR | BUILTIN))
	{
	  cpp_hashnode *hp = cpp_lookup (pfile, b->name, b->len);
	  if (b->flags & OPERATOR)
	    {
	      hp->flags |= NODE_OPERATOR;
	      hp->value.operator = b->operator;
	    }
	  else
	    {
	      hp->type = NT_MACRO;
	      hp->flags |= NODE_BUILTIN | NODE_WARN;
	      hp->value.builtin = b->builtin;
	    }
	}
      else			/* A standard macro of some kind.  */
	{
	  const char *val;
	  char *str;

	  if (b->flags & VERS)
	    {
	      /* Allocate enough space for 'name "value"\n\0'.  */
	      str = alloca (b->len + strlen (version_string) + 5);
	      sprintf (str, "%s \"%s\"\n", b->name, version_string);
	    }
	  else
	    {
	      if (b->flags & ULP)
		val = CPP_OPTION (pfile, user_label_prefix);
	      else
		val = b->value;

	      /* Allocate enough space for "name value\n\0".  */
	      str = alloca (b->len + strlen (val) + 3);
	      sprintf(str, "%s %s\n", b->name, val);
	    }

	  _cpp_define_builtin (pfile, str);
	}
    }

  if (CPP_OPTION (pfile, cplusplus))
    {
      _cpp_define_builtin (pfile, "__cplusplus 1");
      if (SUPPORTS_ONE_ONLY)
	_cpp_define_builtin (pfile, "__GXX_WEAK__ 1");
      else
	_cpp_define_builtin (pfile, "__GXX_WEAK__ 0");
    }
  if (CPP_OPTION (pfile, objc))
    _cpp_define_builtin (pfile, "__OBJC__ 1");

  if (CPP_OPTION (pfile, lang) == CLK_STDC94)
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199409L");
  else if (CPP_OPTION (pfile, c99))
    _cpp_define_builtin (pfile, "__STDC_VERSION__ 199901L");

  if (CPP_OPTION (pfile, lang) == CLK_STDC89
      || CPP_OPTION (pfile, lang) == CLK_STDC94
      || CPP_OPTION (pfile, lang) == CLK_STDC99)
    _cpp_define_builtin (pfile, "__STRICT_ANSI__ 1");
  else if (CPP_OPTION (pfile, lang) == CLK_ASM)
    _cpp_define_builtin (pfile, "__ASSEMBLER__ 1");
}
#undef BUILTIN
#undef OPERATOR
#undef VERS
#undef ULP
#undef CPLUS
#undef builtin_array_end

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

  GET_ENV_PATH_LIST (path, "CPATH");
  if (path != 0 && *path != 0)
    path_include (pfile, path, BRACKET);

  switch ((CPP_OPTION (pfile, objc) << 1) + CPP_OPTION (pfile, cplusplus))
    {
    case 0:
      GET_ENV_PATH_LIST (path, "C_INCLUDE_PATH");
      break;
    case 1:
      GET_ENV_PATH_LIST (path, "CPLUS_INCLUDE_PATH");
      break;
    case 2:
      GET_ENV_PATH_LIST (path, "OBJC_INCLUDE_PATH");
      break;
    case 3:
      GET_ENV_PATH_LIST (path, "OBJCPLUS_INCLUDE_PATH");
      break;
    }
  if (path != 0 && *path != 0)
    path_include (pfile, path, SYSTEM);

  /* Search "translated" versions of GNU directories.
     These have /usr/local/lib/gcc... replaced by specd_prefix.  */
  if (specd_prefix != 0 && cpp_GCC_INCLUDE_DIR_len)
    {
      /* Remove the `include' from /usr/local/lib/gcc.../include.
	 GCC_INCLUDE_DIR will always end in /include. */
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
	      if (!memcmp (p->fname, default_prefix, default_len))
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
	  char *str = xstrdup (update_path (p->fname, p->component));
	  append_include_chain (pfile, str, SYSTEM, p->cxx_aware);
	}
    }
}

/* Handles -imacro and -include from the command line.  */
static void
do_includes (pfile, p, scan)
     cpp_reader *pfile;
     struct pending_option *p;
     int scan;
{
  while (p)
    {
      struct pending_option *q;

      /* Don't handle if -fpreprocessed.  Later: maybe update this to
	 use the #include "" search path if cpp_read_file fails.  */
      if (CPP_OPTION (pfile, preprocessed))
	cpp_error (pfile, "-include and -imacros cannot be used with -fpreprocessed");
      else
	{
	  cpp_token header;
	  header.type = CPP_STRING;
	  header.val.str.text = (const unsigned char *) p->arg;
	  header.val.str.len = strlen (p->arg);
	  if (_cpp_execute_include (pfile, &header, IT_CMDLINE) && scan)
	    cpp_scan_buffer_nooutput (pfile, 0);
	}
      q = p->next;
      free (p);
      p = q;
    }
}

/* This is called after options have been processed.  Setup for
   processing input from the file named FNAME, or stdin if it is the
   empty string.  Return 1 on success, 0 on failure.  */
int
cpp_start_read (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct pending_option *p, *q;

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

  if (CPP_OPTION (pfile, print_deps))
    /* Set the default target (if there is none already).  */
    deps_add_default_target (pfile->deps, fname);

  /* Open the main input file.  This must be done early, so we have a
     buffer to stand on.  */
  if (!_cpp_read_file (pfile, fname))
    return 0;

  /* If already preprocessed, don't install __LINE__, etc., and ignore
     command line definitions and assertions.  Handle -U's, -D's and
     -A's in the order they were seen.  */
  if (! CPP_OPTION (pfile, preprocessed))
    init_builtins (pfile);

  p = CPP_OPTION (pfile, pending)->directive_head;
  while (p)
    {
      if (! CPP_OPTION (pfile, preprocessed))
	(*p->handler) (pfile, p->arg);
      q = p->next;
      free (p);
      p = q;
    }

  /* The -imacros files can be scanned now, but the -include files
     have to be pushed onto the buffer stack and processed later,
     otherwise cppmain.c won't see the tokens.  include_head was built
     up as a stack, and popping this stack onto the buffer stack means
     we preserve the order of the command line.  */
  do_includes (pfile, CPP_OPTION (pfile, pending)->imacros_head, 1);
  do_includes (pfile, CPP_OPTION (pfile, pending)->include_head, 0);

  free (CPP_OPTION (pfile, pending));
  CPP_OPTION (pfile, pending) = NULL;

  return 1;
}

/* Use mkdeps.c to output dependency information.  */
static void
output_deps (pfile)
     cpp_reader *pfile;
{
  /* Stream on which to print the dependency information.  */
  FILE *deps_stream = 0;
  const char *deps_mode = CPP_OPTION (pfile, print_deps_append) ? "a" : "w";

  if (CPP_OPTION (pfile, deps_file) == 0)
    deps_stream = stdout;
  else
    {
      deps_stream = fopen (CPP_OPTION (pfile, deps_file), deps_mode);
      if (deps_stream == 0)
	{
	  cpp_notice_from_errno (pfile, CPP_OPTION (pfile, deps_file));
	  return;
	}
    }

  deps_write (pfile->deps, deps_stream, 72);

  if (CPP_OPTION (pfile, deps_phony_targets))
    deps_phony_targets (pfile->deps, deps_stream);

  /* Don't close stdout.  */
  if (CPP_OPTION (pfile, deps_file))
    {
      if (ferror (deps_stream) || fclose (deps_stream) != 0)
	cpp_fatal (pfile, "I/O error on output");
    }
}

/* This is called at the end of preprocessing.  It pops the
   last buffer and writes dependency output.  It should also
   clear macro definitions, such that you could call cpp_start_read
   with a new filename to restart processing.  */
void
cpp_finish (pfile)
     cpp_reader *pfile;
{
  if (CPP_BUFFER (pfile))
    {
      cpp_ice (pfile, "buffers still stacked in cpp_finish");
      while (CPP_BUFFER (pfile))
	cpp_pop_buffer (pfile);
    }

  /* Don't write the deps file if preprocessing has failed.  */
  if (CPP_OPTION (pfile, print_deps) && pfile->errors == 0)
    output_deps (pfile);

  /* Report on headers that could use multiple include guards.  */
  if (CPP_OPTION (pfile, print_include_names))
    _cpp_report_missing_guards (pfile);
}

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
#define no_arg N_("Argument missing after %s")
#define no_ass N_("Assertion missing after %s")
#define no_dir N_("Directory name missing after %s")
#define no_fil N_("File name missing after %s")
#define no_mac N_("Macro name missing after %s")
#define no_pth N_("Path name missing after %s")
#define no_num N_("Number missing after %s")
#define no_tgt N_("Target missing after %s")

/* This is the list of all command line options, with the leading
   "-" removed.  It must be sorted in ASCII collating order.  */
#define COMMAND_LINE_OPTIONS                                                  \
  DEF_OPT("$",                        0,      OPT_dollar)                     \
  DEF_OPT("+",                        0,      OPT_plus)                       \
  DEF_OPT("-help",                    0,      OPT__help)                      \
  DEF_OPT("-target-help",             0,      OPT_target__help)               \
  DEF_OPT("-version",                 0,      OPT__version)                   \
  DEF_OPT("A",                        no_ass, OPT_A)                          \
  DEF_OPT("C",                        0,      OPT_C)                          \
  DEF_OPT("D",                        no_mac, OPT_D)                          \
  DEF_OPT("H",                        0,      OPT_H)                          \
  DEF_OPT("I",                        no_dir, OPT_I)                          \
  DEF_OPT("M",                        0,      OPT_M)                          \
  DEF_OPT("MD",                       no_fil, OPT_MD)                         \
  DEF_OPT("MF",                       no_fil, OPT_MF)                         \
  DEF_OPT("MG",                       0,      OPT_MG)                         \
  DEF_OPT("MM",                       0,      OPT_MM)                         \
  DEF_OPT("MMD",                      no_fil, OPT_MMD)                        \
  DEF_OPT("MP",                       0,      OPT_MP)                         \
  DEF_OPT("MQ",                       no_tgt, OPT_MQ)                         \
  DEF_OPT("MT",                       no_tgt, OPT_MT)                         \
  DEF_OPT("P",                        0,      OPT_P)                          \
  DEF_OPT("U",                        no_mac, OPT_U)                          \
  DEF_OPT("W",                        no_arg, OPT_W)  /* arg optional */      \
  DEF_OPT("d",                        no_arg, OPT_d)                          \
  DEF_OPT("fleading-underscore",      0,      OPT_fleading_underscore)        \
  DEF_OPT("fno-leading-underscore",   0,      OPT_fno_leading_underscore)     \
  DEF_OPT("fno-operator-names",       0,      OPT_fno_operator_names)         \
  DEF_OPT("fno-preprocessed",         0,      OPT_fno_preprocessed)           \
  DEF_OPT("fno-show-column",          0,      OPT_fno_show_column)            \
  DEF_OPT("fpreprocessed",            0,      OPT_fpreprocessed)              \
  DEF_OPT("fshow-column",             0,      OPT_fshow_column)               \
  DEF_OPT("ftabstop=",                no_num, OPT_ftabstop)                   \
  DEF_OPT("h",                        0,      OPT_h)                          \
  DEF_OPT("idirafter",                no_dir, OPT_idirafter)                  \
  DEF_OPT("imacros",                  no_fil, OPT_imacros)                    \
  DEF_OPT("include",                  no_fil, OPT_include)                    \
  DEF_OPT("iprefix",                  no_pth, OPT_iprefix)                    \
  DEF_OPT("isystem",                  no_dir, OPT_isystem)                    \
  DEF_OPT("iwithprefix",              no_dir, OPT_iwithprefix)                \
  DEF_OPT("iwithprefixbefore",        no_dir, OPT_iwithprefixbefore)          \
  DEF_OPT("lang-asm",                 0,      OPT_lang_asm)                   \
  DEF_OPT("lang-c",                   0,      OPT_lang_c)                     \
  DEF_OPT("lang-c++",                 0,      OPT_lang_cplusplus)             \
  DEF_OPT("lang-c89",                 0,      OPT_lang_c89)                   \
  DEF_OPT("lang-objc",                0,      OPT_lang_objc)                  \
  DEF_OPT("lang-objc++",              0,      OPT_lang_objcplusplus)          \
  DEF_OPT("nostdinc",                 0,      OPT_nostdinc)                   \
  DEF_OPT("nostdinc++",               0,      OPT_nostdincplusplus)           \
  DEF_OPT("o",                        no_fil, OPT_o)                          \
  DEF_OPT("pedantic",                 0,      OPT_pedantic)                   \
  DEF_OPT("pedantic-errors",          0,      OPT_pedantic_errors)            \
  DEF_OPT("remap",                    0,      OPT_remap)                      \
  DEF_OPT("std=c++98",                0,      OPT_std_cplusplus98)            \
  DEF_OPT("std=c89",                  0,      OPT_std_c89)                    \
  DEF_OPT("std=c99",                  0,      OPT_std_c99)                    \
  DEF_OPT("std=c9x",                  0,      OPT_std_c9x)                    \
  DEF_OPT("std=gnu89",                0,      OPT_std_gnu89)                  \
  DEF_OPT("std=gnu99",                0,      OPT_std_gnu99)                  \
  DEF_OPT("std=gnu9x",                0,      OPT_std_gnu9x)                  \
  DEF_OPT("std=iso9899:1990",         0,      OPT_std_iso9899_1990)           \
  DEF_OPT("std=iso9899:199409",       0,      OPT_std_iso9899_199409)         \
  DEF_OPT("std=iso9899:1999",         0,      OPT_std_iso9899_1999)           \
  DEF_OPT("std=iso9899:199x",         0,      OPT_std_iso9899_199x)           \
  DEF_OPT("trigraphs",                0,      OPT_trigraphs)                  \
  DEF_OPT("v",                        0,      OPT_v)                          \
  DEF_OPT("version",                  0,      OPT_version)                    \
  DEF_OPT("w",                        0,      OPT_w)

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
   e.g. -iwithprefix and -iwithprefixbefore.  Moreover, we need to
   accept options beginning with -W that we do not recognise, but not
   to swallow any subsequent command line argument; this is handled as
   special cases in cpp_handle_option.  */
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
      comp = memcmp (input, cl_options[md].opt_text, opt_len);

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
		  if (memcmp (input, cl_options[mn].opt_text, opt_len))
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

  /* Interpret "-" or a non-option as a file name.  */
  if (argv[i][0] != '-' || argv[i][1] == '\0')
    {
      if (CPP_OPTION (pfile, in_fname) == NULL)
	CPP_OPTION (pfile, in_fname) = argv[i];
      else if (CPP_OPTION (pfile, out_fname) == NULL)
	CPP_OPTION (pfile, out_fname) = argv[i];
      else
	cpp_fatal (pfile, "Too many filenames. Type %s --help for usage info",
		   progname);
    }
  else
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

	  /* Yuk. Special case for -W as it must not swallow
	     up any following argument.  If this becomes common, add
	     another field to the cl_options table.  */
	  if (arg[0] == '\0' && opt_code != OPT_W)
	    {
	      arg = argv[++i];
	      if (!arg)
		{
		  cpp_fatal (pfile, cl_options[opt_index].msg, argv[i - 1]);
		  return argc;
		}
	    }
	}

      switch (opt_code)
	{
	case N_OPTS: /* Shut GCC up.  */
	  break;
	case OPT_fleading_underscore:
	  CPP_OPTION (pfile, user_label_prefix) = "_";
	  break;
	case OPT_fno_leading_underscore:
	  CPP_OPTION (pfile, user_label_prefix) = "";
	  break;
	case OPT_fno_operator_names:
	  CPP_OPTION (pfile, operator_names) = 0;
	  break;
	case OPT_fpreprocessed:
	  CPP_OPTION (pfile, preprocessed) = 1;
	  break;
	case OPT_fno_preprocessed:
	  CPP_OPTION (pfile, preprocessed) = 0;
	  break;
	case OPT_fshow_column:
	  CPP_OPTION (pfile, show_column) = 1;
	  break;
	case OPT_fno_show_column:
	  CPP_OPTION (pfile, show_column) = 0;
	  break;
	case OPT_ftabstop:
	  /* Silently ignore empty string, non-longs and silly values.  */
	  if (arg[0] != '\0')
	    {
	      char *endptr;
	      long tabstop = strtol (arg, &endptr, 10);
	      if (*endptr == '\0' && tabstop >= 1 && tabstop <= 100)
		CPP_OPTION (pfile, tabstop) = tabstop;
	    }
	  break;
	case OPT_w:
	  CPP_OPTION (pfile, inhibit_warnings) = 1;
	  break;
	case OPT_h:
	case OPT__help:
	  print_help ();
	  CPP_OPTION (pfile, help_only) = 1;
	  break;
	case OPT_target__help:
          /* Print if any target specific options. cpplib has none, but
	     make sure help_only gets set.  */
	  CPP_OPTION (pfile, help_only) = 1;
          break;

	  /* --version inhibits compilation, -version doesn't. -v means
	     verbose and -version.  Historical reasons, don't ask.  */
	case OPT__version:
	  CPP_OPTION (pfile, help_only) = 1;
	  pfile->print_version = 1;
	  break;
	case OPT_v:
	  CPP_OPTION (pfile, verbose) = 1;
	  pfile->print_version = 1;
	  break;
	case OPT_version:
	  pfile->print_version = 1;
	  break;

	case OPT_C:
	  CPP_OPTION (pfile, discard_comments) = 0;
	  break;
	case OPT_P:
	  CPP_OPTION (pfile, no_line_commands) = 1;
	  break;
	case OPT_dollar:	/* Don't include $ in identifiers.  */
	  CPP_OPTION (pfile, dollars_in_ident) = 0;
	  break;
	case OPT_H:
	  CPP_OPTION (pfile, print_include_names) = 1;
	  break;
	case OPT_D:
	  new_pending_directive (pend, arg, cpp_define);
	  break;
	case OPT_pedantic_errors:
	  CPP_OPTION (pfile, pedantic_errors) = 1;
	  /* fall through */
	case OPT_pedantic:
 	  CPP_OPTION (pfile, pedantic) = 1;
	  break;
	case OPT_trigraphs:
 	  CPP_OPTION (pfile, trigraphs) = 1;
	  break;
	case OPT_plus:
	  CPP_OPTION (pfile, cplusplus) = 1;
	  CPP_OPTION (pfile, cplusplus_comments) = 1;
	  break;
	case OPT_remap:
	  CPP_OPTION (pfile, remap) = 1;
	  break;
	case OPT_iprefix:
	  CPP_OPTION (pfile, include_prefix) = arg;
	  CPP_OPTION (pfile, include_prefix_len) = strlen (arg);
	  break;
	case OPT_lang_c:
	  set_lang (pfile, CLK_GNUC89);
	  break;
	case OPT_lang_cplusplus:
	  set_lang (pfile, CLK_GNUCXX);
	  break;
	case OPT_lang_objc:
	  set_lang (pfile, CLK_OBJC);
	  break;
	case OPT_lang_objcplusplus:
	  set_lang (pfile, CLK_OBJCXX);
	  break;
	case OPT_lang_asm:
	  set_lang (pfile, CLK_ASM);
	  break;
	case OPT_std_cplusplus98:
	  set_lang (pfile, CLK_CXX98);
	  break;
	case OPT_std_gnu89:
	  set_lang (pfile, CLK_GNUC89);
	  break;
	case OPT_std_gnu9x:
	case OPT_std_gnu99:
	  set_lang (pfile, CLK_GNUC99);
	  break;
	case OPT_std_iso9899_199409:
	  set_lang (pfile, CLK_STDC94);
	  break;
	case OPT_std_iso9899_1990:
	case OPT_std_c89:
	case OPT_lang_c89:
	  set_lang (pfile, CLK_STDC89);
	  break;
	case OPT_std_iso9899_199x:
	case OPT_std_iso9899_1999:
	case OPT_std_c9x:
	case OPT_std_c99:
	  set_lang (pfile, CLK_STDC99);
	  break;
	case OPT_nostdinc:
	  /* -nostdinc causes no default include directories.
	     You must specify all include-file directories with -I.  */
	  CPP_OPTION (pfile, no_standard_includes) = 1;
	  break;
	case OPT_nostdincplusplus:
	  /* -nostdinc++ causes no default C++-specific include directories. */
	  CPP_OPTION (pfile, no_standard_cplusplus_includes) = 1;
	  break;
	case OPT_o:
	  if (CPP_OPTION (pfile, out_fname) == NULL)
	    CPP_OPTION (pfile, out_fname) = arg;
	  else
	    {
	      cpp_fatal (pfile, "Output filename specified twice");
	      return argc;
	    }
	  break;
	case OPT_d:
	  /* Args to -d specify what parts of macros to dump.
	     Silently ignore unrecognised options; they may
	     be aimed at the compiler proper.  */
 	  {
	    char c;

	    while ((c = *arg++) != '\0')
 	      switch (c)
 		{
 		case 'M':
		  CPP_OPTION (pfile, dump_macros) = dump_only;
		  CPP_OPTION (pfile, no_output) = 1;
		  break;
		case 'N':
		  CPP_OPTION (pfile, dump_macros) = dump_names;
		  break;
		case 'D':
		  CPP_OPTION (pfile, dump_macros) = dump_definitions;
		  break;
		case 'I':
		  CPP_OPTION (pfile, dump_includes) = 1;
		  break;
		}
	  }
	  break;

	case OPT_MG:
	  CPP_OPTION (pfile, print_deps_missing_files) = 1;
	  break;
	case OPT_M:
	  CPP_OPTION (pfile, print_deps) = 2;
	  break;
	case OPT_MM:
	  CPP_OPTION (pfile, print_deps) = 1;
	  break;
	case OPT_MF:
	  CPP_OPTION (pfile, deps_file) = arg;
	  break;
 	case OPT_MP:
	  CPP_OPTION (pfile, deps_phony_targets) = 1;
	  break;
	case OPT_MQ:
	case OPT_MT:
	  /* Add a target.  -MQ quotes for Make.  */
	  deps_add_target (pfile->deps, arg, opt_code == OPT_MQ);
	  break;

	  /* -MD and -MMD for cpp0 are deprecated and undocumented
	     (use -M or -MM with -MF instead), and probably should be
	     removed with the next major GCC version.  For the moment
	     we allow these for the benefit of Automake 1.4, which
	     uses these when dependency tracking is enabled.  Automake
	     1.5 will fix this.  */
	case OPT_MD:
	  CPP_OPTION (pfile, print_deps) = 2;
	  CPP_OPTION (pfile, deps_file) = arg;
	  break;
	case OPT_MMD:
	  CPP_OPTION (pfile, print_deps) = 1;
	  CPP_OPTION (pfile, deps_file) = arg;
	  break;

	case OPT_A:
	  if (arg[0] == '-')
	    {
	      /* -A with an argument beginning with '-' acts as
		 #unassert on whatever immediately follows the '-'.
		 If "-" is the whole argument, we eliminate all
		 predefined macros and assertions, including those
		 that were specified earlier on the command line.
		 That way we can get rid of any that were passed
		 automatically in from GCC.  */

	      if (arg[1] == '\0')
		{
		  struct pending_option *o1, *o2;

		  o1 = pend->directive_head;
		  while (o1)
		    {
		      o2 = o1->next;
		      free (o1);
		      o1 = o2;
		    }
		  pend->directive_head = NULL;
		  pend->directive_tail = NULL;
		}
	      else
		new_pending_directive (pend, arg + 1, cpp_unassert);
	    }
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
		  cpp_fatal (pfile, "-I- specified twice");
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
	  {
	    struct pending_option *o = (struct pending_option *)
	      xmalloc (sizeof (struct pending_option));
	    o->arg = arg;

	    /* This list has to be built in reverse order so that
	       when cpp_start_read pushes all the -include files onto
	       the buffer stack, they will be scanned in forward order.  */
	    o->next = pend->include_head;
	    pend->include_head = o;
	  }
	  break;
	case OPT_imacros:
	  {
	    struct pending_option *o = (struct pending_option *)
	      xmalloc (sizeof (struct pending_option));
	    o->arg = arg;
	    o->next = NULL;

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
	case OPT_W:
	  /* Silently ignore unrecognised options.  */
	  if (!strcmp (argv[i], "-Wall"))
	    {
	      CPP_OPTION (pfile, warn_trigraphs) = 1;
	      CPP_OPTION (pfile, warn_comments) = 1;
	    }
	  else if (!strcmp (argv[i], "-Wtraditional"))
	    CPP_OPTION (pfile, warn_traditional) = 1;
	  else if (!strcmp (argv[i], "-Wtrigraphs"))
	    CPP_OPTION (pfile, warn_trigraphs) = 1;
	  else if (!strcmp (argv[i], "-Wcomment"))
	    CPP_OPTION (pfile, warn_comments) = 1;
	  else if (!strcmp (argv[i], "-Wcomments"))
	    CPP_OPTION (pfile, warn_comments) = 1;
	  else if (!strcmp (argv[i], "-Wundef"))
	    CPP_OPTION (pfile, warn_undef) = 1;
	  else if (!strcmp (argv[i], "-Wimport"))
	    CPP_OPTION (pfile, warn_import) = 1;
	  else if (!strcmp (argv[i], "-Werror"))
	    CPP_OPTION (pfile, warnings_are_errors) = 1;
	  else if (!strcmp (argv[i], "-Wsystem-headers"))
	    CPP_OPTION (pfile, warn_system_headers) = 1;
	  else if (!strcmp (argv[i], "-Wno-traditional"))
	    CPP_OPTION (pfile, warn_traditional) = 0;
	  else if (!strcmp (argv[i], "-Wno-trigraphs"))
	    CPP_OPTION (pfile, warn_trigraphs) = 0;
	  else if (!strcmp (argv[i], "-Wno-comment"))
	    CPP_OPTION (pfile, warn_comments) = 0;
	  else if (!strcmp (argv[i], "-Wno-comments"))
	    CPP_OPTION (pfile, warn_comments) = 0;
	  else if (!strcmp (argv[i], "-Wno-undef"))
	    CPP_OPTION (pfile, warn_undef) = 0;
	  else if (!strcmp (argv[i], "-Wno-import"))
	    CPP_OPTION (pfile, warn_import) = 0;
	  else if (!strcmp (argv[i], "-Wno-error"))
	    CPP_OPTION (pfile, warnings_are_errors) = 0;
	  else if (!strcmp (argv[i], "-Wno-system-headers"))
	    CPP_OPTION (pfile, warn_system_headers) = 0;
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

/* Extra processing when all options are parsed, after all calls to
   cpp_handle_option[s].  Consistency checks etc.  */
void
cpp_post_options (pfile)
     cpp_reader *pfile;
{
  if (pfile->print_version)
    {
      fprintf (stderr, _("GNU CPP version %s (cpplib)"), version_string);
#ifdef TARGET_VERSION
      TARGET_VERSION;
#endif
      fputc ('\n', stderr);
    }

  /* Canonicalize in_fname and out_fname.  We guarantee they are not
     NULL, and that the empty string represents stdin / stdout.  */
  if (CPP_OPTION (pfile, in_fname) == NULL
      || !strcmp (CPP_OPTION (pfile, in_fname), "-"))
    CPP_OPTION (pfile, in_fname) = "";

  if (CPP_OPTION (pfile, out_fname) == NULL
      || !strcmp (CPP_OPTION (pfile, out_fname), "-"))
    CPP_OPTION (pfile, out_fname) = "";

  /* -Wtraditional is not useful in C++ mode.  */
  if (CPP_OPTION (pfile, cplusplus))
    CPP_OPTION (pfile, warn_traditional) = 0;

  /* Set this if it hasn't been set already. */
  if (CPP_OPTION (pfile, user_label_prefix) == NULL)
    CPP_OPTION (pfile, user_label_prefix) = USER_LABEL_PREFIX;

  /* Permanently disable macro expansion if we are rescanning
     preprocessed text.  */
  if (CPP_OPTION (pfile, preprocessed))
    pfile->state.prevent_expansion = 1;

  /* We need to do this after option processing and before
     cpp_start_read, as cppmain.c relies on the options->no_output to
     set its callbacks correctly before calling cpp_start_read.  */
  init_dependency_output (pfile);

  /* After checking the environment variables, check if -M or -MM has
     not been specified, but other -M options have.  */
  if (CPP_OPTION (pfile, print_deps) == 0 &&
      (CPP_OPTION (pfile, print_deps_missing_files)
       || CPP_OPTION (pfile, deps_file)
       || CPP_OPTION (pfile, deps_phony_targets)))
    cpp_fatal (pfile, "you must additionally specify either -M or -MM");
}

/* Set up dependency-file output.  */
static void
init_dependency_output (pfile)
     cpp_reader *pfile;
{
  char *spec, *s, *output_file;

  /* Either of two environment variables can specify output of deps.
     Its value is either "OUTPUT_FILE" or "OUTPUT_FILE DEPS_TARGET",
     where OUTPUT_FILE is the file to write deps info to
     and DEPS_TARGET is the target to mention in the deps.  */

  if (CPP_OPTION (pfile, print_deps) == 0)
    {
      spec = getenv ("DEPENDENCIES_OUTPUT");
      if (spec)
	CPP_OPTION (pfile, print_deps) = 1;
      else
	{
	  spec = getenv ("SUNPRO_DEPENDENCIES");
	  if (spec)
	    CPP_OPTION (pfile, print_deps) = 2;
	  else
	    return;
	}

      /* Find the space before the DEPS_TARGET, if there is one.  */
      s = strchr (spec, ' ');
      if (s)
	{
	  /* Let the caller perform MAKE quoting.  */
	  deps_add_target (pfile->deps, s + 1, 0);
	  output_file = (char *) xmalloc (s - spec + 1);
	  memcpy (output_file, spec, s - spec);
	  output_file[s - spec] = 0;
	}
      else
	output_file = spec;

      /* Command line overrides environment variables.  */
      if (CPP_OPTION (pfile, deps_file) == 0)
	CPP_OPTION (pfile, deps_file) = output_file;
      CPP_OPTION (pfile, print_deps_append) = 1;
    }

  /* If dependencies go to standard output, or -MG is used, we should
     suppress output.  The user may be requesting other stuff to
     stdout, with -dM, -v etc.  We let them shoot themselves in the
     foot.  */
  if (CPP_OPTION (pfile, deps_file) == 0
      || CPP_OPTION (pfile, print_deps_missing_files))
    CPP_OPTION (pfile, no_output) = 1;
}

static void
print_help ()
{
  fprintf (stderr, _("Usage: %s [switches] input output\n"), progname);
  /* To keep the lines from getting too long for some compilers, limit
     to about 500 characters (6 lines) per chunk. */
  fputs (_("\
Switches:\n\
  -include <file>           Include the contents of <file> before other files\n\
  -imacros <file>           Accept definition of macros in <file>\n\
  -iprefix <path>           Specify <path> as a prefix for next two options\n\
  -iwithprefix <dir>        Add <dir> to the end of the system include path\n\
  -iwithprefixbefore <dir>  Add <dir> to the end of the main include path\n\
  -isystem <dir>            Add <dir> to the start of the system include path\n\
"), stdout);
  fputs (_("\
  -idirafter <dir>          Add <dir> to the end of the system include path\n\
  -I <dir>                  Add <dir> to the end of the main include path\n\
  -I-                       Fine-grained include path control; see info docs\n\
  -nostdinc                 Do not search system include directories\n\
                             (dirs specified with -isystem will still be used)\n\
  -nostdinc++               Do not search system include directories for C++\n\
  -o <file>                 Put output into <file>\n\
"), stdout);
  fputs (_("\
  -pedantic                 Issue all warnings demanded by strict ISO C\n\
  -pedantic-errors          Issue -pedantic warnings as errors instead\n\
  -trigraphs                Support ISO C trigraphs\n\
  -lang-c                   Assume that the input sources are in C\n\
  -lang-c89                 Assume that the input sources are in C89\n\
"), stdout);
  fputs (_("\
  -lang-c++                 Assume that the input sources are in C++\n\
  -lang-objc                Assume that the input sources are in ObjectiveC\n\
  -lang-objc++              Assume that the input sources are in ObjectiveC++\n\
  -lang-asm                 Assume that the input sources are in assembler\n\
"), stdout);
  fputs (_("\
  -std=<std name>           Specify the conformance standard; one of:\n\
                            gnu89, gnu99, c89, c99, iso9899:1990,\n\
                            iso9899:199409, iso9899:1999\n\
  -+                        Allow parsing of C++ style features\n\
  -w                        Inhibit warning messages\n\
  -Wtrigraphs               Warn if trigraphs are encountered\n\
  -Wno-trigraphs            Do not warn about trigraphs\n\
  -Wcomment{s}              Warn if one comment starts inside another\n\
"), stdout);
  fputs (_("\
  -Wno-comment{s}           Do not warn about comments\n\
  -Wtraditional             Warn about features not present in traditional C\n\
  -Wno-traditional          Do not warn about traditional C\n\
  -Wundef                   Warn if an undefined macro is used by #if\n\
  -Wno-undef                Do not warn about testing undefined macros\n\
  -Wimport                  Warn about the use of the #import directive\n\
"), stdout);
  fputs (_("\
  -Wno-import               Do not warn about the use of #import\n\
  -Werror                   Treat all warnings as errors\n\
  -Wno-error                Do not treat warnings as errors\n\
  -Wsystem-headers          Do not suppress warnings from system headers\n\
  -Wno-system-headers       Suppress warnings from system headers\n\
  -Wall                     Enable all preprocessor warnings\n\
"), stdout);
  fputs (_("\
  -M                        Generate make dependencies\n\
  -MM                       As -M, but ignore system header files\n\
  -MF <file>                Write dependency output to the given file\n\
  -MG                       Treat missing header file as generated files\n\
"), stdout);
  fputs (_("\
  -MP			    Generate phony targets for all headers\n\
  -MQ <target>              Add a MAKE-quoted target\n\
  -MT <target>              Add an unquoted target\n\
"), stdout);
  fputs (_("\
  -D<macro>                 Define a <macro> with string '1' as its value\n\
  -D<macro>=<val>           Define a <macro> with <val> as its value\n\
  -A<question> (<answer>)   Assert the <answer> to <question>\n\
  -A-<question> (<answer>)  Disable the <answer> to <question>\n\
  -U<macro>                 Undefine <macro> \n\
  -v                        Display the version number\n\
"), stdout);
  fputs (_("\
  -H                        Print the name of header files as they are used\n\
  -C                        Do not discard comments\n\
  -dM                       Display a list of macro definitions active at end\n\
  -dD                       Preserve macro definitions in output\n\
  -dN                       As -dD except that only the names are preserved\n\
  -dI                       Include #include directives in the output\n\
"), stdout);
  fputs (_("\
  -fpreprocessed            Treat the input file as already preprocessed\n\
  -ftabstop=<number>        Distance between tab stops for column reporting\n\
  -P                        Do not generate #line directives\n\
  -$                        Do not allow '$' in identifiers\n\
  -remap                    Remap file names when including files.\n\
  --version                 Display version information\n\
  -h or --help              Display this information\n\
"), stdout);
}
