/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
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
#include "hashtab.h"
#include "mkdeps.h"

/* Predefined symbols, built-in macros, and the default include path. */

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME)	do { (VAR) = getenv (NAME); } while (0)
#endif

/* Windows does not natively support inodes, and neither does MSDOS.
   Cygwin's emulation can generate non-unique inodes, so don't use it.
   VMS has non-numeric inodes. */
#ifdef VMS
#define INO_T_EQ(a, b) (!memcmp (&(a), &(b), sizeof (a)))
#elif (defined _WIN32 && ! defined (_UWIN)) || defined __MSDOS__
#define INO_T_EQ(a, b) 0
#else
#define INO_T_EQ(a, b) ((a) == (b))
#endif

#ifndef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR "/usr/include"
#endif

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

/* The string value for __SIZE_TYPE__.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

/* The string value for __PTRDIFF_TYPE__.  */

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

/* The string value for __WCHAR_TYPE__.  */

#ifndef WCHAR_TYPE
#define WCHAR_TYPE "int"
#endif

/* The string value for __USER_LABEL_PREFIX__ */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#endif

/* The string value for __REGISTER_PREFIX__ */

#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX ""
#endif

/* This is the default list of directories to search for include files.
   It may be overridden by the various -I and -ixxx options.

   #include "file" looks in the same directory as the current file,
   then this list. 
   #include <file> just looks in this list.

   All these directories are treated as `system' include directories
   (they are not subject to pedantic warnings in some cases).  */

struct default_include
{
  const char *fname;		/* The name of the directory.  */
  const char *component;	/* The component containing the directory
				   (see update_path in prefix.c) */
  int cplusplus;		/* Only look here if we're compiling C++.  */
  int cxx_aware;		/* Includes in this directory don't need to
				   be wrapped in extern "C" when compiling
				   C++.  */
};

#ifndef STANDARD_INCLUDE_COMPONENT
#define STANDARD_INCLUDE_COMPONENT 0
#endif

#ifdef CROSS_COMPILE
#undef LOCAL_INCLUDE_DIR
#undef SYSTEM_INCLUDE_DIR
#undef STANDARD_INCLUDE_DIR
#else
#undef CROSS_INCLUDE_DIR
#endif

static const struct default_include include_defaults_array[]
#ifdef INCLUDE_DEFAULTS
= INCLUDE_DEFAULTS;
#else
= {
#ifdef GPLUSPLUS_INCLUDE_DIR
    /* Pick up GNU C++ specific include files.  */
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1 },
#endif
#ifdef LOCAL_INCLUDE_DIR
    /* /usr/local/include comes before the fixincluded header files.  */
    { LOCAL_INCLUDE_DIR, 0, 0, 1 },
#endif
#ifdef GCC_INCLUDE_DIR
    /* This is the dir for fixincludes and for gcc's private headers.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0 },
#endif
#ifdef CROSS_INCLUDE_DIR
    /* One place the target system's headers might be.  */
    { CROSS_INCLUDE_DIR, "GCC", 0, 0 },
#endif
#ifdef TOOL_INCLUDE_DIR
    /* Another place the target system's headers might be.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1 },
#endif
#ifdef SYSTEM_INCLUDE_DIR
    /* Some systems have an extra dir of include files.  */
    { SYSTEM_INCLUDE_DIR, 0, 0, 0 },
#endif
#ifdef STANDARD_INCLUDE_DIR
    /* /usr/include comes dead last.  */
    { STANDARD_INCLUDE_DIR, STANDARD_INCLUDE_COMPONENT, 0, 0 },
#endif
    { 0, 0, 0, 0 }
  };
#endif /* no INCLUDE_DEFAULTS */

/* Internal structures and prototypes. */

/* A `struct pending_option' remembers one -D, -A, -U, -include, or -imacros
   switch.  There are four lists: one for -D and -U, one for -A, one
   for -include, one for -imacros.  `undef' is set for -U, clear for
   -D, ignored for the others.
   (Future: add an equivalent of -U for -A) */

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
   tail pointers for quick insertion. */
struct cpp_pending
{
  struct pending_option *directive_head, *directive_tail;

  struct file_name_list *quote_head, *quote_tail;
  struct file_name_list *brack_head, *brack_tail;
  struct file_name_list *systm_head, *systm_tail;
  struct file_name_list *after_head, *after_tail;

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
						 struct cpp_pending *,
						 char *, int));
static void initialize_builtins		PARAMS ((cpp_reader *));
static void append_include_chain	PARAMS ((cpp_reader *,
						 struct cpp_pending *,
						 char *, int, int));
static void merge_include_chains	PARAMS ((struct cpp_options *));

static void dump_special_to_buffer	PARAMS ((cpp_reader *, const char *));
static void initialize_dependency_output PARAMS ((cpp_reader *));
static void initialize_standard_includes PARAMS ((cpp_reader *));
static void new_pending_directive		PARAMS ((struct cpp_options *,
						 const char *,
						 cl_directive_handler));
#ifdef HOST_EBCDIC
static int opt_comp			PARAMS ((const void *, const void *));
#endif
static int parse_option			PARAMS ((const char *));
static int handle_option		PARAMS ((cpp_reader *, int, char **));

/* Fourth argument to append_include_chain: chain to use */
enum { QUOTE = 0, BRACKET, SYSTEM, AFTER };

/* If we have designated initializers (GCC >2.7, or C99) this table
   can be initialized, constant data.  Otherwise, it has to be filled
   in at runtime.  */

#if (GCC_VERSION >= 2007) || (__STDC_VERSION__ >= 199901L)
#define init_IStable()  /* nothing */
#define ISTABLE const unsigned char _cpp_IStable[256] = {
#define END };
#define s(p, v) [p] = v,
#else
#define ISTABLE unsigned char _cpp_IStable[256] = { 0 }; \
 static void init_IStable PARAMS ((void)) { \
 unsigned char *x = _cpp_IStable;
#define END } 
#define s(p, v) x[p] = v;
#endif

#define A(x) s(x, ISidnum|ISidstart)
#define N(x) s(x, ISidnum|ISnumstart)
#define H(x) s(x, IShspace|ISspace)
#define S(x) s(x, ISspace)

ISTABLE
  A('_')

  A('a') A('b') A('c') A('d') A('e') A('f') A('g') A('h') A('i')
  A('j') A('k') A('l') A('m') A('n') A('o') A('p') A('q') A('r')
  A('s') A('t') A('u') A('v') A('w') A('x') A('y') A('z')

  A('A') A('B') A('C') A('D') A('E') A('F') A('G') A('H') A('I')
  A('J') A('K') A('L') A('M') A('N') A('O') A('P') A('Q') A('R')
  A('S') A('T') A('U') A('V') A('W') A('X') A('Y') A('Z')

  N('1') N('2') N('3') N('4') N('5') N('6') N('7') N('8') N('9') N('0')

  H(' ') H('\t') H('\v') H('\f')

  S('\n')
END

#undef A
#undef N
#undef H
#undef S
#undef s
#undef ISTABLE
#undef END

/* Given a colon-separated list of file names PATH,
   add all the names to the search path for include files.  */

static void
path_include (pfile, pend, list, path)
     cpp_reader *pfile;
     struct cpp_pending *pend;
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

      append_include_chain (pfile, pend, name, path, 0);

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
append_include_chain (pfile, pend, dir, path, cxx_aware)
     cpp_reader *pfile;
     struct cpp_pending *pend;
     char *dir;
     int path;
     int cxx_aware;
{
  struct file_name_list *new;
  struct stat st;
  unsigned int len;

  _cpp_simplify_pathname (dir);
  if (stat (dir, &st))
    {
      /* Dirs that don't exist are silently ignored. */
      if (errno != ENOENT)
	cpp_notice_from_errno (pfile, dir);
      else if (CPP_OPTIONS (pfile)->verbose)
	fprintf (stderr, _("ignoring nonexistent directory `%s'\n"), dir);
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
  
  new = (struct file_name_list *) xmalloc (sizeof (struct file_name_list));
  new->name = dir;
  new->nlen = len;
  new->ino  = st.st_ino;
  new->dev  = st.st_dev;
  if (path == SYSTEM)
    new->sysp = cxx_aware ? 1 : 2;
  else
    new->sysp = 0;
  new->name_map = NULL;
  new->next = NULL;
  new->alloc = NULL;

  switch (path)
    {
    case QUOTE:		APPEND (pend, quote, new); break;
    case BRACKET:	APPEND (pend, brack, new); break;
    case SYSTEM:	APPEND (pend, systm, new); break;
    case AFTER:		APPEND (pend, after, new); break;
    }
}

/* Merge the four include chains together in the order quote, bracket,
   system, after.  Remove duplicate dirs (as determined by
   INO_T_EQ()).  The system_include and after_include chains are never
   referred to again after this function; all access is through the
   bracket_include path.

   For the future: Check if the directory is empty (but
   how?) and possibly preload the include hash. */

static void
merge_include_chains (opts)
     struct cpp_options *opts;
{
  struct file_name_list *prev, *cur, *other;
  struct file_name_list *quote, *brack, *systm, *after;
  struct file_name_list *qtail, *btail, *stail, *atail;

  qtail = opts->pending->quote_tail;
  btail = opts->pending->brack_tail;
  stail = opts->pending->systm_tail;
  atail = opts->pending->after_tail;

  quote = opts->pending->quote_head;
  brack = opts->pending->brack_head;
  systm = opts->pending->systm_head;
  after = opts->pending->after_head;

  /* Paste together bracket, system, and after include chains. */
  if (stail)
    stail->next = after;
  else
    systm = after;
  if (btail)
    btail->next = systm;
  else
    brack = systm;

  /* This is a bit tricky.
     First we drop dupes from the quote-include list.
     Then we drop dupes from the bracket-include list.
     Finally, if qtail and brack are the same directory,
     we cut out qtail.

     We can't just merge the lists and then uniquify them because
     then we may lose directories from the <> search path that should
     be there; consider -Ifoo -Ibar -I- -Ifoo -Iquux. It is however
     safe to treat -Ibar -Ifoo -I- -Ifoo -Iquux as if written
     -Ibar -I- -Ifoo -Iquux.

     Note that this algorithm is quadratic in the number of -I switches,
     which is acceptable since there aren't usually that many of them.  */

  for (cur = quote, prev = NULL; cur; cur = cur->next)
    {
      for (other = quote; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino)
	    && cur->dev == other->dev)
          {
	    if (opts->verbose)
	      fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
		       cur->name);

	    prev->next = cur->next;
	    free (cur->name);
	    free (cur);
	    cur = prev;
	    break;
	  }
      prev = cur;
    }
  qtail = prev;

  for (cur = brack; cur; cur = cur->next)
    {
      for (other = brack; other != cur; other = other->next)
        if (INO_T_EQ (cur->ino, other->ino)
	    && cur->dev == other->dev)
          {
	    if (opts->verbose)
	      fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
		       cur->name);

	    prev->next = cur->next;
	    free (cur->name);
	    free (cur);
	    cur = prev;
	    break;
	  }
      prev = cur;
    }

  if (quote)
    {
      if (INO_T_EQ (qtail->ino, brack->ino) && qtail->dev == brack->dev)
        {
	  if (quote == qtail)
	    {
	      if (opts->verbose)
		fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
			 quote->name);

	      free (quote->name);
	      free (quote);
	      quote = brack;
	    }
	  else
	    {
	      cur = quote;
	      while (cur->next != qtail)
		  cur = cur->next;
	      cur->next = brack;
	      if (opts->verbose)
		fprintf (stderr, _("ignoring duplicate directory `%s'\n"),
			 qtail->name);

	      free (qtail->name);
	      free (qtail);
	    }
	}
      else
	  qtail->next = brack;
    }
  else
      quote = brack;

  opts->quote_include = quote;
  opts->bracket_include = brack;
}


/* Write out a #define command for the special named MACRO_NAME
   to PFILE's token_buffer.  */

static void
dump_special_to_buffer (pfile, macro_name)
     cpp_reader *pfile;
     const char *macro_name;
{
  static const char define_directive[] = "#define ";
  int macro_name_length = strlen (macro_name);
  _cpp_output_line_command (pfile, same_file);
  CPP_RESERVE (pfile, sizeof(define_directive) + macro_name_length);
  CPP_PUTS_Q (pfile, define_directive, sizeof(define_directive)-1);
  CPP_PUTS_Q (pfile, macro_name, macro_name_length);
  CPP_PUTC_Q (pfile, ' ');
  cpp_expand_to_buffer (pfile, macro_name, macro_name_length);
  CPP_PUTC (pfile, '\n');
}

/* Initialize a cpp_options structure. */
void
cpp_options_init (opts)
     cpp_options *opts;
{
  memset ((char *) opts, 0, sizeof (struct cpp_options));

  opts->dollars_in_ident = 1;
  opts->cplusplus_comments = 1;
  opts->warn_import = 1;
  opts->discard_comments = 1;

  opts->pending =
    (struct cpp_pending *) xcalloc (1, sizeof (struct cpp_pending));
}

/* Initialize a cpp_reader structure. */
void
cpp_reader_init (pfile)
     cpp_reader *pfile;
{
  memset ((char *) pfile, 0, sizeof (cpp_reader));

  pfile->token_buffer_size = 200;
  pfile->token_buffer = (U_CHAR *) xmalloc (pfile->token_buffer_size);
  CPP_SET_WRITTEN (pfile, 0);

  _cpp_init_macro_hash (pfile);
  _cpp_init_include_hash (pfile);
}

/* Free resources used by PFILE.
   This is the cpp_reader 'finalizer' or 'destructor' (in C++ terminology).  */
void
cpp_cleanup (pfile)
     cpp_reader *pfile;
{
  while (CPP_BUFFER (pfile) != NULL)
    cpp_pop_buffer (pfile);

  if (pfile->token_buffer)
    {
      free (pfile->token_buffer);
      pfile->token_buffer = NULL;
    }

  if (pfile->input_buffer)
    {
      free (pfile->input_buffer);
      free (pfile->input_speccase);
      pfile->input_buffer = pfile->input_speccase = NULL;
      pfile->input_buffer_len = 0;
    }

  if (pfile->deps)
    deps_free (pfile->deps);

  htab_delete (pfile->hashtab);
  htab_delete (pfile->all_include_files);
}


/* This structure defines one built-in macro.  A node of type TYPE will
   be entered in the macro hash table under the name NAME, with value
   VALUE (if any).  FLAGS tweaks the behavior a little:
   DUMP		write debug info for this macro
   STDC		define only if not -traditional
   ULP		value is the global user_label_prefix (which can't be
		put directly into the table).
 */

struct builtin
{
  const char *name;
  const char *value;
  unsigned short type;
  unsigned short flags;
};
#define DUMP 0x01
#define STDC 0x02
#define VERS 0x04
#define ULP  0x08

static const struct builtin builtin_array[] =
{
  { "__TIME__",			0, T_TIME,		DUMP },
  { "__DATE__",			0, T_DATE,		DUMP },
  { "__FILE__",			0, T_FILE,		0    },
  { "__BASE_FILE__",		0, T_BASE_FILE,		0    },
  { "__LINE__",			0, T_SPECLINE,		0    },
  { "__INCLUDE_LEVEL__",	0, T_INCLUDE_LEVEL,	0    },
  { "__VERSION__",		0, T_VERSION,		DUMP|VERS },
  { "__STDC__",			0, T_STDC,		DUMP|STDC },

  { "__USER_LABEL_PREFIX__",	0,		 T_CONST, ULP  },
  { "__REGISTER_PREFIX__",	REGISTER_PREFIX, T_CONST, 0    },
  { "__HAVE_BUILTIN_SETJMP__",	"1",		 T_CONST, 0    },
#ifndef NO_BUILTIN_SIZE_TYPE
  { "__SIZE_TYPE__",		SIZE_TYPE,	 T_CONST, DUMP },
#endif
#ifndef NO_BUILTIN_PTRDIFF_TYPE
  { "__PTRDIFF_TYPE__",		PTRDIFF_TYPE,	 T_CONST, DUMP },
#endif
#ifndef NO_BUILTIN_WCHAR_TYPE
  { "__WCHAR_TYPE__",		WCHAR_TYPE,	 T_CONST, DUMP },
#endif
  { 0, 0, 0, 0 }
};

/* Subroutine of cpp_start_read; reads the builtins table above and
   enters the macros into the hash table.  */
static void
initialize_builtins (pfile)
     cpp_reader *pfile;
{
  int len;
  const struct builtin *b;
  const char *val;
  HASHNODE *hp;
  for(b = builtin_array; b->name; b++)
    {
      if ((b->flags & STDC) && CPP_TRADITIONAL (pfile))
	continue;

      if (b->flags & ULP)
	val = user_label_prefix;
      else if (b->flags & VERS)
	val = version_string;
      else
	val = b->value;

      len = strlen (b->name);
      hp = _cpp_make_hashnode (b->name, len, b->type, -1);
      hp->value.cpval = val;
      *(htab_find_slot (pfile->hashtab, (void *)hp, 1)) = hp;

      if ((b->flags & DUMP) && CPP_OPTIONS (pfile)->debug_output)
	dump_special_to_buffer (pfile, b->name);
    }

}
#undef DUMP
#undef STDC
#undef VERS
#undef ULP

/* Another subroutine of cpp_start_read.  This one sets up to do
   dependency-file output. */
static void
initialize_dependency_output (pfile)
     cpp_reader *pfile;
{
  cpp_options *opts = CPP_OPTIONS (pfile);
  char *spec, *s, *output_file;
  
  /* Either of two environment variables can specify output of deps.
     Its value is either "OUTPUT_FILE" or "OUTPUT_FILE DEPS_TARGET",
     where OUTPUT_FILE is the file to write deps info to
     and DEPS_TARGET is the target to mention in the deps.  */

  if (opts->print_deps == 0)
    {
      spec = getenv ("DEPENDENCIES_OUTPUT");
      if (spec)
	opts->print_deps = 1;
      else
	{
	  spec = getenv ("SUNPRO_DEPENDENCIES");
	  if (spec)
	    opts->print_deps = 2;
	  else
	    return;
	}

      /* Find the space before the DEPS_TARGET, if there is one.  */
      s = strchr (spec, ' ');
      if (s)
	{
	  opts->deps_target = s + 1;
	  output_file = (char *) xmalloc (s - spec + 1);
	  memcpy (output_file, spec, s - spec);
	  output_file[s - spec] = 0;
	}
      else
	{
	  opts->deps_target = 0;
	  output_file = spec;
	}

      opts->deps_file = output_file;
      opts->print_deps_append = 1;
    }

  pfile->deps = deps_init ();

  /* Print the expected object file name as the target of this Make-rule.  */
  if (opts->deps_target)
    deps_add_target (pfile->deps, opts->deps_target);
  else if (*opts->in_fname == 0)
    deps_add_target (pfile->deps, "-");
  else
    deps_calc_target (pfile->deps, opts->in_fname);

  if (opts->in_fname)
    deps_add_dep (pfile->deps, opts->in_fname);
}

/* And another subroutine.  This one sets up the standard include path.  */
static void
initialize_standard_includes (pfile)
     cpp_reader *pfile;
{
  cpp_options *opts = CPP_OPTIONS (pfile);
  char *path;
  const struct default_include *p;
  const char *specd_prefix = opts->include_prefix;

  /* Several environment variables may add to the include search path.
     CPATH specifies an additional list of directories to be searched
     as if specified with -I, while C_INCLUDE_PATH, CPLUS_INCLUDE_PATH,
     etc. specify an additional list of directories to be searched as
     if specified with -isystem, for the language indicated.  */

  GET_ENV_PATH_LIST (path, "CPATH");
  if (path != 0 && *path != 0)
    path_include (pfile, opts->pending, path, BRACKET);

  switch ((opts->objc << 1) + opts->cplusplus)
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
    path_include (pfile, opts->pending, path, SYSTEM);

  /* Search "translated" versions of GNU directories.
     These have /usr/local/lib/gcc... replaced by specd_prefix.  */
  if (specd_prefix != 0)
    {
      char *default_prefix = alloca (sizeof GCC_INCLUDE_DIR - 7);
      /* Remove the `include' from /usr/local/lib/gcc.../include.
	 GCC_INCLUDE_DIR will always end in /include. */
      int default_len = sizeof GCC_INCLUDE_DIR - 8;
      int specd_len = strlen (specd_prefix);

      memcpy (default_prefix, GCC_INCLUDE_DIR, default_len);
      default_prefix[default_len] = '\0';

      for (p = include_defaults_array; p->fname; p++)
	{
	  /* Some standard dirs are only for C++.  */
	  if (!p->cplusplus
	      || (opts->cplusplus
		  && !opts->no_standard_cplusplus_includes))
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

		  append_include_chain (pfile, opts->pending,
					str, SYSTEM, p->cxx_aware);
		}
	    }
	}
    }

  /* Search ordinary names for GNU include directories.  */
  for (p = include_defaults_array; p->fname; p++)
    {
      /* Some standard dirs are only for C++.  */
      if (!p->cplusplus
	  || (opts->cplusplus
	      && !opts->no_standard_cplusplus_includes))
	{
	  /* XXX Potential memory leak! */
	  char *str = xstrdup (update_path (p->fname, p->component));
	  append_include_chain (pfile, opts->pending, str, SYSTEM,
				p->cxx_aware);
	}
    }
}

/* This is called after options have been processed.
 * Check options for consistency, and setup for processing input
 * from the file named FNAME.  (Use standard input if FNAME==NULL.)
 * Return 1 on success, 0 on failure.
 */

int
cpp_start_read (pfile, fname)
     cpp_reader *pfile;
     const char *fname;
{
  struct cpp_options *opts = CPP_OPTIONS (pfile);
  struct pending_option *p, *q;

  /* -MG doesn't select the form of output and must be specified with one of
     -M or -MM.  -MG doesn't make sense with -MD or -MMD since they don't
     inhibit compilation.  */
  if (opts->print_deps_missing_files
      && (opts->print_deps == 0 || !opts->no_output))
    {
      cpp_fatal (pfile, "-MG must be specified with one of -M or -MM");
      return 0;
    }

  /* Chill should not be used with -trigraphs. */
  if (opts->chill && opts->trigraphs)
    {
      cpp_warning (pfile, "-lang-chill and -trigraphs are mutually exclusive");
      opts->trigraphs = 0;
    }

  /* Set this if it hasn't been set already. */
  if (user_label_prefix == NULL)
    user_label_prefix = USER_LABEL_PREFIX;

  /* Don't bother trying to do macro expansion if we've already done
     preprocessing.  */
  if (opts->preprocessed)
    pfile->no_macro_expand++;

  /* Set up the IStable.  This doesn't do anything if we were compiled
     with a compiler that supports C99 designated initializers.  */
  init_IStable ();

  /* Set up the tables used by read_and_prescan.  */
  _cpp_init_input_buffer (pfile);
  
  /* Set up the include search path now.  */
  if (! opts->no_standard_includes)
    initialize_standard_includes (pfile);

  merge_include_chains (opts);

  /* With -v, print the list of dirs to search.  */
  if (opts->verbose)
    {
      struct file_name_list *l;
      fprintf (stderr, _("#include \"...\" search starts here:\n"));
      for (l = opts->quote_include; l; l = l->next)
	{
	  if (l == opts->bracket_include)
	    fprintf (stderr, _("#include <...> search starts here:\n"));
	  fprintf (stderr, " %s\n", l->name);
	}
      fprintf (stderr, _("End of search list.\n"));
    }

  initialize_dependency_output (pfile);
  
  /* Open the main input file.  This must be done before -D processing
     so we have a buffer to stand on.  */
  if (opts->in_fname == NULL || *opts->in_fname == 0)
    {
      opts->in_fname = fname;
      if (opts->in_fname == NULL)
	opts->in_fname = "";
    }

  if (!cpp_read_file (pfile, fname))
    return 0;

  /* -D and friends may produce output, which should be identified
     as line 0.  */

  CPP_BUFFER (pfile)->lineno = 0;

  /* Install __LINE__, etc.  */
  initialize_builtins (pfile);

  /* Do -U's, -D's and -A's in the order they were seen.  */
  p = opts->pending->directive_head;
  while (p)
    {
      (*p->handler) (pfile, p->arg);
      q = p->next;
      free (p);
      p = q;
    }

  opts->done_initializing = 1;
  CPP_BUFFER (pfile)->lineno = 1;

  if (opts->preprocessed)
    /* If we've already processed this code, we want to trust the #line
       directives in the input.  But we still need to update our line
       counter accordingly.  */
    pfile->lineno = CPP_BUFFER (pfile)->lineno;
  else
    _cpp_output_line_command (pfile, same_file);
  pfile->only_seen_white = 2;

  /* The -imacros files can be scanned now, but the -include files
     have to be pushed onto the include stack and processed later,
     in the main loop calling cpp_get_token.  */
  
  opts->no_output++;
  p = opts->pending->imacros_head;
  while (p)
    {
      if (cpp_read_file (pfile, p->arg))
	cpp_scan_buffer (pfile);

      q = p->next;
      free (p);
      p = q;
    }
  opts->no_output--;

  p = opts->pending->include_head;
  while (p)
    {
      if (cpp_read_file (pfile, p->arg))
	_cpp_output_line_command (pfile, enter_file);

      q = p->next;
      free (p);
      p = q;
    }

  free (opts->pending);
  opts->pending = NULL;

  return 1;
}

/* This is called at the end of preprocessing.  It pops the
   last buffer and writes dependency output.  It should also
   clear macro definitions, such that you could call cpp_start_read
   with a new filename to restart processing. */
void
cpp_finish (pfile)
     cpp_reader *pfile;
{
  struct cpp_options *opts = CPP_OPTIONS (pfile);

  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)))
    cpp_ice (pfile, "buffers still stacked in cpp_finish");
  while (CPP_BUFFER (pfile))
    cpp_pop_buffer (pfile);

  /* Don't write the deps file if preprocessing has failed.  */
  if (opts->print_deps && pfile->errors == 0)
    {
      /* Stream on which to print the dependency information.  */
      FILE *deps_stream = 0;

      const char *deps_mode = opts->print_deps_append ? "a" : "w";
      if (opts->deps_file == 0)
	deps_stream = stdout;
      else if ((deps_stream = fopen (opts->deps_file, deps_mode)) == 0)
	cpp_notice_from_errno (pfile, opts->deps_file);

      if (deps_stream)
	{
	  deps_write (pfile->deps, deps_stream, 72);
	  if (opts->deps_file)
	    {
	      if (ferror (deps_stream) || fclose (deps_stream) != 0)
		cpp_fatal (pfile, "I/O error on output");
	    }
	}
    }

  if (opts->dump_macros == dump_only)
    _cpp_dump_macro_hash (pfile);
}

static void
new_pending_directive (opts, text, handler)
     struct cpp_options *opts;
     const char *text;
     cl_directive_handler handler;
{
  struct pending_option *o = (struct pending_option *)
    xmalloc (sizeof (struct pending_option));

  o->arg = text;
  o->next = NULL;
  o->handler = handler;
  APPEND (opts->pending, directive, o);
}

enum opt_code
{
  OPT_stdin_stdout = 0, OPT_dollar, OPT_plus,
  OPT__help, OPT__version,
  OPT_A, OPT_C, OPT_D, OPT_H, OPT_I, OPT_M,
  OPT_MD, OPT_MG, OPT_MM, OPT_MMD,
  OPT_P, OPT_U, OPT_W,
  OPT_d,
  OPT_fleading_underscore, OPT_fno_leading_underscore,
  OPT_fpreprocessed, OPT_fno_preprocessed,
  OPT_g, OPT_h, 
  OPT_idirafter, OPT_imacros, OPT_include,
  OPT_iprefix, OPT_isystem, OPT_iwithprefix, OPT_iwithprefixbefore,
  OPT_lang_asm, OPT_lang_c, OPT_lang_cplusplus, OPT_lang_c89,
  OPT_lang_chill, OPT_lang_fortran, OPT_lang_objc, OPT_lang_objcplusplus,
  OPT_nostdinc, OPT_nostdincplusplus,
  OPT_o,
  OPT_pedantic, OPT_pedantic_errors, OPT_remap,
  OPT_std_c89, OPT_std_c99, OPT_std_c9x, OPT_std_gnu89, OPT_std_gnu99,
  OPT_std_gnu9x, OPT_std_iso9899_1990, OPT_std_iso9899_199409,
  OPT_std_iso9899_1999, OPT_std_iso9899_199x,
  OPT_traditional, OPT_trigraphs,
  OPT_v, OPT_w,
  N_OPTS
};

struct cl_option
{
  const char *opt_text;
  const char *msg;
  size_t opt_len;
  enum opt_code opt_code;
};

/* Irix6 "cc -n32" and OSF4 cc have problems with char foo[] = ("string");
   I.e. a const string initializer with parens around it.  That is
   what N_("string") resolves to, so we make no_* be macros instead.  */
#define no_arg N_("Argument missing after `%s' option")
#define no_ass N_("Assertion missing after `%s' option")
#define no_dir N_("Directory name missing after `%s' option")
#define no_fil N_("File name missing after `%s' option")
#define no_mac N_("Macro name missing after `%s' option")
#define no_pth N_("Path name missing after `%s' option")

/* This list must be ASCII sorted. Make enum order above match this. */
#define DEF_OPT(text, msg, code) {text, msg, sizeof(text) - 1, code}

#ifdef HOST_EBCDIC
static struct cl_option cl_options[] =
#else
static const struct cl_option cl_options[] =
#endif
{
  DEF_OPT("",                         0,      OPT_stdin_stdout),
  DEF_OPT("$",                        0,      OPT_dollar),
  DEF_OPT("+",                        0,      OPT_plus),
  DEF_OPT("-help",                    0,      OPT__help),
  DEF_OPT("-version",                 0,      OPT__version),
  DEF_OPT("A",                        no_ass, OPT_A),
  DEF_OPT("C",                        0,      OPT_C),
  DEF_OPT("D",                        no_mac, OPT_D),
  DEF_OPT("H",                        0,      OPT_H),
  DEF_OPT("I",                        no_dir, OPT_I),
  DEF_OPT("M",                        0,      OPT_M),
  DEF_OPT("MD",                       no_fil, OPT_MD),
  DEF_OPT("MG",                       0,      OPT_MG),
  DEF_OPT("MM",                       0,      OPT_MM),
  DEF_OPT("MMD",                      no_fil, OPT_MMD),
  DEF_OPT("P",                        0,      OPT_P),
  DEF_OPT("U",                        no_mac, OPT_U),
  /* NB: Immed arg only, and not reqd */
  DEF_OPT("W",                        no_arg, OPT_W),
  DEF_OPT("d",                        no_arg, OPT_d),
  DEF_OPT("fleading-underscore",      0,      OPT_fleading_underscore),
  DEF_OPT("fno-leading-underscore",   0,      OPT_fno_leading_underscore),
  DEF_OPT("fpreprocessed",            0,      OPT_fpreprocessed),
  DEF_OPT("fno-preprocessed",         0,      OPT_fno_preprocessed),
  /* NB: Immed arg only, and not reqd */
  DEF_OPT("g",                        no_arg, OPT_g),
  DEF_OPT("h",                        0,      OPT_h),
  DEF_OPT("idirafter",                no_dir, OPT_idirafter),
  DEF_OPT("imacros",                  no_fil, OPT_imacros),
  DEF_OPT("include",                  no_fil, OPT_include),
  DEF_OPT("iprefix",                  no_pth, OPT_iprefix),
  DEF_OPT("isystem",                  no_dir, OPT_isystem),
  DEF_OPT("iwithprefix",              no_dir, OPT_iwithprefix),
  DEF_OPT("iwithprefixbefore",        no_dir, OPT_iwithprefixbefore),
  DEF_OPT("lang-asm",                 0,      OPT_lang_asm),
  DEF_OPT("lang-c",                   0,      OPT_lang_c),
  DEF_OPT("lang-c++",                 0,      OPT_lang_cplusplus),
  DEF_OPT("lang-c89",                 0,      OPT_lang_c89),
  DEF_OPT("lang-chill",               0,      OPT_lang_chill),
  DEF_OPT("lang-fortran",             0,      OPT_lang_fortran),
  DEF_OPT("lang-objc",                0,      OPT_lang_objc),
  DEF_OPT("lang-objc++",              0,      OPT_lang_objcplusplus),
  DEF_OPT("nostdinc",                 0,      OPT_nostdinc),
  DEF_OPT("nostdinc++",               0,      OPT_nostdincplusplus),
  DEF_OPT("o",                        no_fil, OPT_o),
  DEF_OPT("pedantic",                 0,      OPT_pedantic),
  DEF_OPT("pedantic-errors",          0,      OPT_pedantic_errors),
  DEF_OPT("remap",                    0,      OPT_remap),
  DEF_OPT("std=c89",                  0,      OPT_std_c89),
  DEF_OPT("std=c99",                  0,      OPT_std_c99),
  DEF_OPT("std=c9x",                  0,      OPT_std_c9x),
  DEF_OPT("std=gnu89",                0,      OPT_std_gnu89),
  DEF_OPT("std=gnu99",                0,      OPT_std_gnu99),
  DEF_OPT("std=gnu9x",                0,      OPT_std_gnu9x),
  DEF_OPT("std=iso9899:1990",         0,      OPT_std_iso9899_1990),
  DEF_OPT("std=iso9899:199409",       0,      OPT_std_iso9899_199409),
  DEF_OPT("std=iso9899:1999",         0,      OPT_std_iso9899_1999),
  DEF_OPT("std=iso9899:199x",         0,      OPT_std_iso9899_199x),
  DEF_OPT("traditional",              0,      OPT_traditional),
  DEF_OPT("trigraphs",                0,      OPT_trigraphs),
  DEF_OPT("v",                        0,      OPT_v),
  DEF_OPT("w",                        0,      OPT_w)
};
#undef DEF_OPT

/* Perform a binary search to find which, if any, option the given
   command-line matches.  Returns its index in the option array,
   negative on failure.  Complications arise since some options can be
   suffixed with an argument, and multiple complete matches can occur,
   e.g. -iwithprefix and -iwithprefixbefore.  Moreover, we want to
   accept options beginning with -g and -W that we do not recognise,
   but not to swallow any subsequent command line argument; these are
   handled as special cases in cpp_handle_option */
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
	     continue the search (e.g. input="stdc++" match was "stdc") */
	  mn = md + 1;
	  if (cl_options[md].msg)
	    {
	      /* Scan forwards.  If we get an exact match, return it.
		 Otherwise, return the longest option-accepting match.
		 This loops no more than twice with current options */
	      mx = md;
	      for (; mn < N_OPTS; mn++)
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

static int
handle_option (pfile, argc, argv)
     cpp_reader *pfile;
     int argc;
     char **argv;
{
  struct cpp_options *opts = CPP_OPTIONS (pfile);
  int i = 0;

  if (argv[i][0] != '-')
    {
      if (opts->out_fname != NULL)
	{
	  print_help ();
	  cpp_fatal (pfile, "Too many arguments");
	}
      else if (opts->in_fname != NULL)
	opts->out_fname = argv[i];
      else
	opts->in_fname = argv[i];
    }
  else
    {
      enum opt_code opt_code;
      int opt_index;
      const char *arg = 0;

      /* Skip over '-' */
      opt_index = parse_option (&argv[i][1]);
      if (opt_index < 0)
	return i;

      opt_code = cl_options[opt_index].opt_code;
      if (cl_options[opt_index].msg)
	{
	  arg = &argv[i][cl_options[opt_index].opt_len + 1];

	  /* Yuk. Special case for -g and -W as they must not swallow
	     up any following argument.  If this becomes common, add
	     another field to the cl_options table */
	  if (arg[0] == '\0' && !(opt_code == OPT_g || opt_code == OPT_W))
	    {
	      arg = argv[++i];
	      if (!arg)
		{
		  cpp_fatal (pfile, _(cl_options[opt_index].msg), argv[i - 1]);
		  return argc;
		}
	    }
	}
 
      switch (opt_code)
	{
	case N_OPTS: /* shut GCC up */
	  break;
	case OPT_fleading_underscore:
	  user_label_prefix = "_";
	  break;
	case OPT_fno_leading_underscore:
	  user_label_prefix = "";
	  break;
	case OPT_fpreprocessed:
	  opts->preprocessed = 1;
	  break;
	case OPT_fno_preprocessed:
	  opts->preprocessed = 0;
	  break;
	case OPT_w:
	  opts->inhibit_warnings = 1;
	  break;
	case OPT_g:  /* Silently ignore anything but -g3 */
	  if (!strcmp(&argv[i][2], "3"))
	    opts->debug_output = 1;
	  break;
	case OPT_h:
	case OPT__help:
	  print_help ();
	  exit (0);  /* XXX */
	  break;
	case OPT__version:
	  fprintf (stderr, _("GNU CPP version %s (cpplib)\n"), version_string);
	  exit (0);  /* XXX */
	  break;
	case OPT_C:
	  opts->discard_comments = 0;
	  break;
	case OPT_P:
	  opts->no_line_commands = 1;
	  break;
	case OPT_dollar:		/* Don't include $ in identifiers.  */
	  opts->dollars_in_ident = 0;
	  break;
	case OPT_H:
	  opts->print_include_names = 1;
	  break;
	case OPT_D:
	  new_pending_directive (opts, arg, cpp_define);
	  break;
	case OPT_pedantic_errors:
	  opts->pedantic_errors = 1;
	  /* fall through */
	case OPT_pedantic:
 	  opts->pedantic = 1;
	  break;
	case OPT_traditional:
	  opts->traditional = 1;
	  opts->cplusplus_comments = 0;
	  opts->trigraphs = 0;
	  opts->warn_trigraphs = 0;
	  break;
	case OPT_trigraphs:
 	  opts->trigraphs = 1;
	  break;
	case OPT_plus:
	  opts->cplusplus = 1;
	  opts->cplusplus_comments = 1;
	  break;
	case OPT_remap:
	  opts->remap = 1;
	  break;
	case OPT_iprefix:
	  opts->include_prefix = arg;
	  opts->include_prefix_len = strlen (arg);
	  break;
	case OPT_lang_c:
	  opts->cplusplus = 0, opts->cplusplus_comments = 1;
	  opts->c89 = 0, opts->c99 = 1, opts->objc = 0;
	  break;
	case OPT_lang_c89:
	  opts->cplusplus = 0, opts->cplusplus_comments = 0;
	  opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	  opts->trigraphs = 1;
	  new_pending_directive (opts, "__STRICT_ANSI__", cpp_define);
	  break;
	case OPT_lang_cplusplus:
	  opts->cplusplus = 1, opts->cplusplus_comments = 1;
	  opts->c89 = 0, opts->c99 = 0, opts->objc = 0;
	  break;
	case OPT_lang_objc:
	case OPT_lang_objcplusplus:
	  opts->cplusplus = opt_code == OPT_lang_objcplusplus;
	  opts->cplusplus_comments = 1;
	  opts->c89 = 0, opts->c99 = 0, opts->objc = 1;
	  break;
	case OPT_lang_asm:
 	  opts->lang_asm = 1;
	  break;
	case OPT_lang_fortran:
 	  opts->lang_fortran = 1, opts->cplusplus_comments = 0;
	  break;
	case OPT_lang_chill:
	  opts->objc = 0, opts->cplusplus = 0;
	  opts->chill = 1, opts->traditional = 1;
	  break;
	case OPT_nostdinc:
	  /* -nostdinc causes no default include directories.
	     You must specify all include-file directories with -I.  */
	  opts->no_standard_includes = 1;
	  break;
	case OPT_nostdincplusplus:
	  /* -nostdinc++ causes no default C++-specific include directories. */
	  opts->no_standard_cplusplus_includes = 1;
	  break;
	case OPT_std_gnu89:
	  opts->cplusplus = 0, opts->cplusplus_comments = 1;
	  opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	  break;
	case OPT_std_gnu9x:
	case OPT_std_gnu99:
	  opts->cplusplus = 0, opts->cplusplus_comments = 1;
	  opts->c89 = 0, opts->c99 = 1, opts->objc = 0;
	  new_pending_directive (opts, "__STDC_VERSION__=199901L", cpp_define);
	  break;
	case OPT_std_iso9899_199409:
	  new_pending_directive (opts, "__STDC_VERSION__=199409L", cpp_define);
	  /* Fall through */
	case OPT_std_iso9899_1990:
	case OPT_std_c89:
	  opts->cplusplus = 0, opts->cplusplus_comments = 0;
	  opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	  opts->trigraphs = 1;
	  new_pending_directive (opts, "__STRICT_ANSI__", cpp_define);
	  break;
	case OPT_std_iso9899_199x:
	case OPT_std_iso9899_1999:
	case OPT_std_c9x:
	case OPT_std_c99:
	  opts->cplusplus = 0, opts->cplusplus_comments = 1;
	  opts->c89 = 0, opts->c99 = 1, opts->objc = 0;
	  opts->trigraphs = 1;
	  new_pending_directive (opts, "__STRICT_ANSI__", cpp_define);
	  new_pending_directive (opts, "__STDC_VERSION__=199901L", cpp_define);
	  break;
	case OPT_o:
	  if (opts->out_fname != NULL)
	    {
	      cpp_fatal (pfile, "Output filename specified twice");
	      return argc;
	    }
	  opts->out_fname = arg;
	  if (!strcmp (opts->out_fname, "-"))
	    opts->out_fname = "";
	  break;
	case OPT_v:
	  fprintf (stderr, _("GNU CPP version %s (cpplib)\n"), version_string);
#ifdef TARGET_VERSION
	  TARGET_VERSION;
#endif
	  fputc ('\n', stderr);
	  opts->verbose = 1;
	  break;
	case OPT_stdin_stdout:
	  /* JF handle '-' as file name meaning stdin or stdout */
	  if (opts->in_fname == NULL)
	    opts->in_fname = "";
	  else if (opts->out_fname == NULL)
	    opts->out_fname = "";
	  break;
	case OPT_d:
	  /* Args to -d specify what parts of macros to dump.
	     Silently ignore unrecognised options; they may
	     be aimed at the compiler proper. */
 	  {
	    char c;
 
	    while ((c = *arg++) != '\0')
 	      switch (c)
 		{
 		case 'M':
		  opts->dump_macros = dump_only;
		  opts->no_output = 1;
		  break;
		case 'N':
		  opts->dump_macros = dump_names;
		  break;
		case 'D':
		  opts->dump_macros = dump_definitions;
		  break;
		case 'I':
		  opts->dump_includes = 1;
		  break;
		}
	  }
	  break;
	  /* The style of the choices here is a bit mixed.
	     The chosen scheme is a hybrid of keeping all options in one string
	     and specifying each option in a separate argument:
	     -M|-MM|-MD file|-MMD file [-MG].  An alternative is:
	     -M|-MM|-MD file|-MMD file|-MG|-MMG; or more concisely:
	     -M[M][G][D file].  This is awkward to handle in specs, and is not
	     as extensible.  */
	  /* ??? -MG must be specified in addition to one of -M or -MM.
	     This can be relaxed in the future without breaking anything.
	     The converse isn't true.  */
       
	  /* -MG isn't valid with -MD or -MMD.  This is checked for later.  */
	case OPT_MG:
	  opts->print_deps_missing_files = 1;
	  break;
	case OPT_M:
	case OPT_MD:
	case OPT_MM:
	case OPT_MMD:
	  if (opt_code == OPT_M || opt_code == OPT_MD)
	    opts->print_deps = 2;
 	  else
	    opts->print_deps = 1;

	  /* For -MD and -MMD options, write deps on file named by next arg */
	  /* For -M and -MM, write deps on standard output
	     and suppress the usual output.  */
	  if (opt_code == OPT_MD || opt_code == OPT_MMD)
	      opts->deps_file = arg;
 	  else
	      opts->no_output = 1;
	  break;
	case OPT_A:
	  if (strcmp (arg, "-"))
	    new_pending_directive (opts, arg, cpp_assert);
	  else
	    {
	      /* -A- eliminates all predefined macros and assertions.
		 Let's include also any that were specified earlier
		 on the command line.  That way we can get rid of any
		 that were passed automatically in from GCC.  */
	      struct pending_option *o1, *o2;

	      o1 = opts->pending->directive_head;
	      while (o1)
		{
		  o2 = o1->next;
		  free (o1);
		  o1 = o2;
		}
	      opts->pending->directive_head = NULL;
	      opts->pending->directive_tail = NULL;
	    }
	  break;
	case OPT_U:
	  new_pending_directive (opts, arg, cpp_undef);
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
	      if (! opts->ignore_srcdir)
		{
		  opts->ignore_srcdir = 1;
		  opts->pending->quote_head = opts->pending->brack_head;
		  opts->pending->quote_tail = opts->pending->brack_tail;
		  opts->pending->brack_head = 0;
		  opts->pending->brack_tail = 0;
		}
	      else
		{
		  cpp_fatal (pfile, "-I- specified twice");
		  return argc;
		}
 	    }
 	  else
	    append_include_chain (pfile, opts->pending,
				  xstrdup (arg), BRACKET, 0);
	  break;
	case OPT_isystem:
	  /* Add directory to beginning of system include path, as a system
	     include directory. */
	  append_include_chain (pfile, opts->pending,
				xstrdup (arg), SYSTEM, 0);
	  break;
	case OPT_include:
	  {
	    struct pending_option *o = (struct pending_option *)
	      xmalloc (sizeof (struct pending_option));
	    o->arg = arg;

	    /* This list has to be built in reverse order so that
	       when cpp_start_read pushes all the -include files onto
	       the buffer stack, they will be scanned in forward order.  */
	    o->next = opts->pending->include_head;
	    opts->pending->include_head = o;
	  }
	  break;
	case OPT_imacros:
	  {
	    struct pending_option *o = (struct pending_option *)
	      xmalloc (sizeof (struct pending_option));
	    o->arg = arg;
	    o->next = NULL;
	    
	    APPEND (opts->pending, imacros, o);
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
 
	    if (opts->include_prefix != 0)
	      {
		fname = xmalloc (opts->include_prefix_len + len + 1);
		memcpy (fname, opts->include_prefix, opts->include_prefix_len);
		memcpy (fname + opts->include_prefix_len, arg, len + 1);
	      }
	    else
	      {
		fname = xmalloc (sizeof GCC_INCLUDE_DIR - 8 + len);
		memcpy (fname, GCC_INCLUDE_DIR, sizeof GCC_INCLUDE_DIR - 9);
		memcpy (fname + sizeof GCC_INCLUDE_DIR - 9, arg, len + 1);
	      }
	    
	    append_include_chain (pfile, opts->pending, fname, 
			  opt_code == OPT_iwithprefix ? SYSTEM: BRACKET, 0);
	  }
	  break;
	case OPT_idirafter:
	  /* Add directory to end of path for includes.  */
	  append_include_chain (pfile, opts->pending,
				xstrdup (arg), AFTER, 0);
	  break;
	case OPT_W:
	  /* Silently ignore unrecognised options */
	  if (!strcmp (argv[i], "-Wall"))
	    {
	      opts->warn_trigraphs = 1;
	      opts->warn_comments = 1;
	    }
	  else if (!strcmp (argv[i], "-Wtraditional"))
	    opts->warn_stringify = 1;
	  else if (!strcmp (argv[i], "-Wtrigraphs"))
	    opts->warn_trigraphs = 1;
	  else if (!strcmp (argv[i], "-Wcomment"))
	    opts->warn_comments = 1;
	  else if (!strcmp (argv[i], "-Wcomments"))
	    opts->warn_comments = 1;
	  else if (!strcmp (argv[i], "-Wundef"))
	    opts->warn_undef = 1;
	  else if (!strcmp (argv[i], "-Wimport"))
	    opts->warn_import = 1;
	  else if (!strcmp (argv[i], "-Werror"))
	    opts->warnings_are_errors = 1;
	  else if (!strcmp (argv[i], "-Wno-traditional"))
	    opts->warn_stringify = 0;
	  else if (!strcmp (argv[i], "-Wno-trigraphs"))
	    opts->warn_trigraphs = 0;
	  else if (!strcmp (argv[i], "-Wno-comment"))
	    opts->warn_comments = 0;
	  else if (!strcmp (argv[i], "-Wno-comments"))
	    opts->warn_comments = 0;
	  else if (!strcmp (argv[i], "-Wno-undef"))
	    opts->warn_undef = 0;
	  else if (!strcmp (argv[i], "-Wno-import"))
	    opts->warn_import = 0;
	  else if (!strcmp (argv[i], "-Wno-error"))
	    opts->warnings_are_errors = 0;
	  break;
 	}
    }
  return i + 1;
}

#ifdef HOST_EBCDIC
static int
opt_comp (const void *p1, const void *p2)
{
  return strcmp (((struct cl_option *)p1)->opt_text,
		 ((struct cl_option *)p2)->opt_text);
}
#endif

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

#ifdef HOST_EBCDIC
  static int opts_sorted = 0;

  if (!opts_sorted)
    {
      opts_sorted = 1;
      /* For non-ASCII hosts, the array needs to be sorted at runtime */
      qsort (cl_options, N_OPTS, sizeof (struct cl_option), opt_comp);
    }
#endif

  for (i = 0; i < argc; i += strings_processed)
    {
      strings_processed = handle_option (pfile, argc - i, argv + i);
      if (strings_processed == 0)
	break;
    }
  return i;
}

static void
print_help ()
{
  fprintf (stderr, _("Usage: %s [switches] input output\n"), progname);
  fputs (_("\
Switches:\n\
  -include <file>           Include the contents of <file> before other files\n\
  -imacros <file>           Accept definition of macros in <file>\n\
  -iprefix <path>           Specify <path> as a prefix for next two options\n\
  -iwithprefix <dir>        Add <dir> to the end of the system include path\n\
  -iwithprefixbefore <dir>  Add <dir> to the end of the main include path\n\
  -isystem <dir>            Add <dir> to the start of the system include path\n\
  -idirafter <dir>          Add <dir> to the end of the system include path\n\
  -I <dir>                  Add <dir> to the end of the main include path\n\
  -I-                       Fine-grained include path control; see info docs\n\
  -nostdinc                 Do not search system include directories\n\
                             (dirs specified with -isystem will still be used)\n\
  -nostdinc++               Do not search system include directories for C++\n\
  -o <file>                 Put output into <file>\n\
  -pedantic                 Issue all warnings demanded by strict ANSI C\n\
  -pedantic-errors          Issue -pedantic warnings as errors instead\n\
  -traditional              Follow K&R pre-processor behaviour\n\
  -trigraphs                Support ANSI C trigraphs\n\
  -lang-c                   Assume that the input sources are in C\n\
  -lang-c89                 Assume that the input sources are in C89\n\
  -lang-c++                 Assume that the input sources are in C++\n\
  -lang-objc                Assume that the input sources are in ObjectiveC\n\
  -lang-objc++              Assume that the input sources are in ObjectiveC++\n\
  -lang-asm                 Assume that the input sources are in assembler\n\
  -lang-fortran		    Assume that the input sources are in Fortran\n\
  -lang-chill               Assume that the input sources are in Chill\n\
  -std=<std name>           Specify the conformance standard; one of:\n\
                            gnu89, gnu99, c89, c99, iso9899:1990,\n\
                            iso9899:199409, iso9899:1999\n\
  -+                        Allow parsing of C++ style features\n\
  -w                        Inhibit warning messages\n\
  -Wtrigraphs               Warn if trigraphs are encountered\n\
  -Wno-trigraphs            Do not warn about trigraphs\n\
  -Wcomment{s}              Warn if one comment starts inside another\n\
  -Wno-comment{s}           Do not warn about comments\n\
  -Wtraditional             Warn if a macro argument is/would be turned into\n\
                             a string if -traditional is specified\n\
  -Wno-traditional          Do not warn about stringification\n\
  -Wundef                   Warn if an undefined macro is used by #if\n\
  -Wno-undef                Do not warn about testing undefined macros\n\
  -Wimport                  Warn about the use of the #import directive\n\
  -Wno-import               Do not warn about the use of #import\n\
  -Werror                   Treat all warnings as errors\n\
  -Wno-error                Do not treat warnings as errors\n\
  -Wall                     Enable all preprocessor warnings\n\
  -M                        Generate make dependencies\n\
  -MM                       As -M, but ignore system header files\n\
  -MD                       As -M, but put output in a .d file\n\
  -MMD                      As -MD, but ignore system header files\n\
  -MG                       Treat missing header file as generated files\n\
  -g3                       Include #define and #undef directives in the output\n\
  -D<macro>                 Define a <macro> with string '1' as its value\n\
  -D<macro>=<val>           Define a <macro> with <val> as its value\n\
  -A<question> (<answer>)   Assert the <answer> to <question>\n\
  -U<macro>                 Undefine <macro> \n\
  -v                        Display the version number\n\
  -H                        Print the name of header files as they are used\n\
  -C                        Do not discard comments\n\
  -dM                       Display a list of macro definitions active at end\n\
  -dD                       Preserve macro definitions in output\n\
  -dN                       As -dD except that only the names are preserved\n\
  -dI                       Include #include directives in the output\n\
  -P                        Do not generate #line directives\n\
  -$                        Do not allow '$' in identifiers\n\
  -remap                    Remap file names when including files.\n\
  --version                 Display version information\n\
  -h or --help              Display this information\n\
"), stdout);
}
