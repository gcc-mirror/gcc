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

#define FAKE_CONST
#include "cpplib.h"
#include "cpphash.h"
#include "output.h"
#include "prefix.h"
#include "intl.h"
#include "version.h"

/* Predefined symbols, built-in macros, and the default include path. */

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME)	do { (VAR) = getenv (NAME); } while (0)
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

/* Suffix for object files, and known input-file extensions. */
static const char * const known_suffixes[] =
{
  ".c",  ".C",   ".s",   ".S",   ".m",
  ".cc", ".cxx", ".cpp", ".cp",  ".c++",
  NULL
};

#ifndef OBJECT_SUFFIX
# ifdef VMS
#  define OBJECT_SUFFIX ".obj"
# else
#  define OBJECT_SUFFIX ".o"
# endif
#endif


/* This is the default list of directories to search for include files.
   It may be overridden by the various -I and -ixxx options.

   #include "file" looks in the same directory as the current file,
   then this list. 
   #include <file> just looks in this list.

   All these directories are treated as `system' include directories
   (they are not subject to pedantic warnings in some cases).  */

static struct default_include
{
  const char *fname;		/* The name of the directory.  */
  const char *component;	/* The component containing the directory
				   (see update_path in prefix.c) */
  int cplusplus;		/* Only look here if we're compiling C++.  */
  int cxx_aware;		/* Includes in this directory don't need to
				   be wrapped in extern "C" when compiling
				   C++.  */
}
include_defaults_array[]
#ifdef INCLUDE_DEFAULTS
= INCLUDE_DEFAULTS;
#else
= {
    /* Pick up GNU C++ specific include files.  */
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1 },
#ifdef CROSS_COMPILE
    /* This is the dir for fixincludes.  Put it just before
       the files that we fix.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0 },
    /* For cross-compilation, this dir name is generated
       automatically in Makefile.in.  */
    { CROSS_INCLUDE_DIR, "GCC", 0, 0 },
#ifdef TOOL_INCLUDE_DIR
    /* This is another place that the target system's headers might be.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1 },
#endif
#else /* not CROSS_COMPILE */
#ifdef LOCAL_INCLUDE_DIR
    /* This should be /usr/local/include and should come before
       the fixincludes-fixed header files.  */
    { LOCAL_INCLUDE_DIR, 0, 0, 1 },
#endif
#ifdef TOOL_INCLUDE_DIR
    /* This is here ahead of GCC_INCLUDE_DIR because assert.h goes here.
       Likewise, behind LOCAL_INCLUDE_DIR, where glibc puts its assert.h.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1 },
#endif
    /* This is the dir for fixincludes.  Put it just before
       the files that we fix.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0 },
    /* Some systems have an extra dir of include files.  */
#ifdef SYSTEM_INCLUDE_DIR
    { SYSTEM_INCLUDE_DIR, 0, 0, 0 },
#endif
#ifndef STANDARD_INCLUDE_COMPONENT
#define STANDARD_INCLUDE_COMPONENT 0
#endif
    { STANDARD_INCLUDE_DIR, STANDARD_INCLUDE_COMPONENT, 0, 0 },
#endif /* not CROSS_COMPILE */
    { 0, 0, 0, 0 }
  };
#endif /* no INCLUDE_DEFAULTS */

/* Internal structures and prototypes. */

/* A `struct pending_option' remembers one -D, -A, -U, -include, or -imacros
   switch.  There are four lists: one for -D and -U, one for -A, one
   for -include, one for -imacros.  `undef' is set for -U, clear for
   -D, ignored for the others.
   (Future: add an equivalent of -U for -A) */
struct pending_option
{
  struct pending_option *next;
  char *arg;
  int undef;
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
static char *base_name			PARAMS ((const char *));
static void dump_special_to_buffer	PARAMS ((cpp_reader *, const char *));
static void initialize_dependency_output PARAMS ((cpp_reader *));
static void initialize_standard_includes PARAMS ((cpp_reader *));
static void new_pending_define		PARAMS ((struct cpp_options *,
						 const char *));

/* Fourth argument to append_include_chain: chain to use */
enum { QUOTE = 0, BRACKET, SYSTEM, AFTER };

/* If gcc is in use (stage2/stage3) we can make this table initialized data. */
#ifdef __STDC__
#define CAT(a, b) a##b
#else
#define CAT(a, b) a/**/b
#endif

#if (GCC_VERSION >= 2007)
#define TABLE(id) static inline void CAT(init_, id) PARAMS ((void)) {} \
unsigned char id[256] = {
#define s(p, v) [p] = v,
#define END };
#else
#define TABLE(id) unsigned char id[256] = { 0 }; \
static void CAT(init_,id) PARAMS ((void)) { \
unsigned char *x = id;
#define s(p, v) x[p] = v;
#define END } 
#endif

#define A(x) s(x, ISidnum|ISidstart)
#define N(x) s(x, ISidnum|ISnumstart)
#define H(x) s(x, IShspace|ISspace)
#define S(x) s(x, ISspace)

TABLE (IStable)
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
#undef TABLE
#undef END
#undef s
#undef CAT

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

/* Find the base name of a (partial) pathname FNAME.
   Returns a pointer into the string passed in.
   Accepts Unix (/-separated) paths on all systems,
   DOS and VMS paths on those systems.  */
static char *
base_name (fname)
     const char *fname;
{
  char *s = (char *)fname;
  char *p;
#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  if (ISALPHA (s[0]) && s[1] == ':') s += 2;
  if ((p = rindex (s, '\\'))) s = p + 1;
#elif defined VMS
  if ((p = rindex (s, ':'))) s = p + 1; /* Skip device.  */
  if ((p = rindex (s, ']'))) s = p + 1; /* Skip directory.  */
  if ((p = rindex (s, '>'))) s = p + 1; /* Skip alternate (int'n'l) dir.  */
#endif
  if ((p = rindex (s, '/'))) s = p + 1;
  return s;
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

  simplify_pathname (dir);
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
  
  new = (struct file_name_list *)xmalloc (sizeof (struct file_name_list));
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


/* Write out a #define command for the special named MACRO_NAME
   to PFILE's token_buffer.  */

static void
dump_special_to_buffer (pfile, macro_name)
     cpp_reader *pfile;
     const char *macro_name;
{
  static const char define_directive[] = "#define ";
  int macro_name_length = strlen (macro_name);
  output_line_command (pfile, same_file);
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
  bzero ((char *) opts, sizeof (struct cpp_options));

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
  bzero ((char *) pfile, sizeof (cpp_reader));

  pfile->token_buffer_size = 200;
  pfile->token_buffer = (U_CHAR *) xmalloc (pfile->token_buffer_size);
  CPP_SET_WRITTEN (pfile, 0);

  pfile->hashtab = (HASHNODE **) xcalloc (HASHSIZE, sizeof (HASHNODE *));
}

/* Free resources used by PFILE.
   This is the cpp_reader 'finalizer' or 'destructor' (in C++ terminology).  */
void
cpp_cleanup (pfile)
     cpp_reader *pfile;
{
  int i;
  while (CPP_BUFFER (pfile) != CPP_NULL_BUFFER (pfile))
    cpp_pop_buffer (pfile);

  if (pfile->token_buffer)
    {
      free (pfile->token_buffer);
      pfile->token_buffer = NULL;
    }

  if (pfile->deps_buffer)
    {
      free (pfile->deps_buffer);
      pfile->deps_buffer = NULL;
      pfile->deps_allocated_size = 0;
    }

  if (pfile->input_buffer)
    {
      free (pfile->input_buffer);
      free (pfile->input_speccase);
      pfile->input_buffer = pfile->input_speccase = NULL;
      pfile->input_buffer_len = 0;
    }

  while (pfile->if_stack)
    {
      IF_STACK_FRAME *temp = pfile->if_stack;
      pfile->if_stack = temp->next;
      free (temp);
    }

  for (i = ALL_INCLUDE_HASHSIZE; --i >= 0; )
    {
      struct include_hash *imp = pfile->all_include_files[i];
      while (imp)
	{
	  struct include_hash *next = imp->next;
#if 0
	  /* This gets freed elsewhere - I think. */
	  free (imp->name);
#endif
	  free (imp);
	  imp = next;
	}
      pfile->all_include_files[i] = 0;
    }

  for (i = HASHSIZE; --i >= 0;)
    {
      while (pfile->hashtab[i])
	delete_macro (pfile->hashtab[i]);
    }
  free (pfile->hashtab);
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
#define ULP  0x10

static const struct builtin builtin_array[] =
{
  { "__TIME__",			0, T_TIME,		DUMP },
  { "__DATE__",			0, T_DATE,		DUMP },
  { "__FILE__",			0, T_FILE,		0    },
  { "__BASE_FILE__",		0, T_BASE_FILE,		0    },
  { "__LINE__",			0, T_SPECLINE,		0    },
  { "__INCLUDE_LEVEL__",	0, T_INCLUDE_LEVEL,	0    },
  { "__VERSION__",		0, T_VERSION,		DUMP },
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
  for(b = builtin_array; b->name; b++)
    {
      if ((b->flags & STDC) && CPP_TRADITIONAL (pfile))
	continue;

      val = (b->flags & ULP) ? user_label_prefix : b->value;
      len = strlen (b->name);

      cpp_install (pfile, b->name, len, b->type, val);
      if ((b->flags & DUMP) && CPP_OPTIONS (pfile)->debug_output)
	dump_special_to_buffer (pfile, b->name);
    }

}
#undef DUMP
#undef STDC
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

  /* Print the expected object file name as the target of this Make-rule.  */
  pfile->deps_allocated_size = 200;
  pfile->deps_buffer = (char *) xmalloc (pfile->deps_allocated_size);
  pfile->deps_buffer[0] = 0;
  pfile->deps_size = 0;
  pfile->deps_column = 0;

  if (opts->deps_target)
    deps_output (pfile, opts->deps_target, ':');
  else if (*opts->in_fname == 0)
    deps_output (pfile, "-", ':');
  else
    {
      char *p, *q, *r;
      int len, x;

      /* Discard all directory prefixes from filename.  */
      q = base_name (opts->in_fname);

      /* Copy remainder to mungable area.  */
      len = strlen (q);
      p = (char *) alloca (len + 8);
      strcpy (p, q);

      /* Output P, but remove known suffixes.  */
      q = p + len;
      /* Point to the filename suffix.  */
      r = strrchr (p, '.');
      if (r)
	/* Compare against the known suffixes.  */
	for (x = 0; known_suffixes[x]; x++)
	  if (strncmp (known_suffixes[x], r, q - r) == 0)
	    {
	      /* Make q point to the bit we're going to overwrite
		 with an object suffix.  */
	      q = r;
	      break;
	    }

      /* Supply our own suffix.  */
      strcpy (q, OBJECT_SUFFIX);

      deps_output (pfile, p, ':');
      deps_output (pfile, opts->in_fname, ' ');
    }
}

/* And another subroutine.  This one sets up the standard include path.  */
static void
initialize_standard_includes (pfile)
     cpp_reader *pfile;
{
  cpp_options *opts = CPP_OPTIONS (pfile);
  char *path;
  struct default_include *p = include_defaults_array;
  char *specd_prefix = opts->include_prefix;

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
     char *fname;
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
  
  /* Now that we know dollars_in_ident, we can initialize the syntax
     tables. */
  init_IStable ();
  /* XXX Get rid of code that depends on this, then IStable can
     be truly const.  */
  if (opts->dollars_in_ident)
    IStable['$'] = ISidstart|ISidnum;

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
  p = opts->pending->define_head;
  while (p)
    {
      if (p->undef)
	cpp_undef (pfile, p->arg);
      else
	cpp_define (pfile, p->arg);

      q = p->next;
      free (p);
      p = q;
    }

  p = opts->pending->assert_head;
  while (p)
    {
      if (p->undef)
	cpp_unassert (pfile, p->arg);
      else
	cpp_assert (pfile, p->arg);

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
    output_line_command (pfile, same_file);
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
	output_line_command (pfile, enter_file);

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

  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) != CPP_NULL_BUFFER (pfile))
    cpp_ice (pfile, "buffers still stacked in cpp_finish");
  cpp_pop_buffer (pfile);

  if (opts->print_deps)
    {
      /* Stream on which to print the dependency information.  */
      FILE *deps_stream = 0;

      /* Don't actually write the deps file if compilation has failed.  */
      if (pfile->errors == 0)
	{
	  const char *deps_mode = opts->print_deps_append ? "a" : "w";
	  if (opts->deps_file == 0)
	    deps_stream = stdout;
	  else if ((deps_stream = fopen (opts->deps_file, deps_mode)) == 0)
	    cpp_notice_from_errno (pfile, opts->deps_file);

	  if (deps_stream)
	    {
	      fputs (pfile->deps_buffer, deps_stream);
	      putc ('\n', deps_stream);
	      if (opts->deps_file)
		{
		  if (ferror (deps_stream) || fclose (deps_stream) != 0)
		    cpp_fatal (pfile, "I/O error on output");
		}
	    }
	}
    }

  if (opts->dump_macros == dump_only)
    {
      int i;
      HASHNODE *h;
      for (i = HASHSIZE; --i >= 0;)
	{
	  for (h = pfile->hashtab[i]; h; h = h->next)
	    if (h->type == T_MACRO)
	      {
		dump_definition (pfile, h->name, h->length, h->value.defn);
		CPP_PUTC (pfile, '\n');
	      }
	}
    }
}

static void
new_pending_define (opts, text)
     struct cpp_options *opts;
     const char *text;
{
  struct pending_option *o = (struct pending_option *)
    xmalloc (sizeof (struct pending_option));

  o->arg = (char *) text;
  o->next = NULL;
  o->undef = 0;
  APPEND (opts->pending, define, o);
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
    switch (argv[i][1])
      {
      case 'f':
	if (!strcmp (argv[i], "-fleading-underscore"))
	  user_label_prefix = "_";
	else if (!strcmp (argv[i], "-fno-leading-underscore"))
	  user_label_prefix = "";
	else if (!strcmp (argv[i], "-fpreprocessed"))
	  opts->preprocessed = 1;
	else if (!strcmp (argv[i], "-fno-preprocessed"))
	  opts->preprocessed = 0;
	else
	  {
	    return i;
	  }
	break;

      case 'I':			/* Add directory to path for includes.  */
	if (!strcmp (argv[i] + 2, "-"))
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
	  {
	    char *fname;
	    if (argv[i][2] != 0)
	      fname = argv[i] + 2;
	    else if (i + 1 == argc)
	      goto missing_dirname;
	    else
	      fname = argv[++i];
	    append_include_chain (pfile, opts->pending,
				  xstrdup (fname), BRACKET, 0);
	  }
	break;

      case 'i':
	/* Add directory to beginning of system include path, as a system
	   include directory. */
	if (!strcmp (argv[i], "-isystem"))
	  {
	    if (i + 1 == argc)
	      goto missing_filename;
	    append_include_chain (pfile, opts->pending,
				  xstrdup (argv[++i]), SYSTEM, 0);
	  }
	else if (!strcmp (argv[i], "-include"))
	  {
	    if (i + 1 == argc)
	      goto missing_filename;
	    else
	      {
		struct pending_option *o = (struct pending_option *)
		  xmalloc (sizeof (struct pending_option));
		o->arg = argv[++i];

		/* This list has to be built in reverse order so that
		   when cpp_start_read pushes all the -include files onto
		   the buffer stack, they will be scanned in forward order.  */
		o->next = opts->pending->include_head;
		opts->pending->include_head = o;
	      }
	  }
	else if (!strcmp (argv[i], "-imacros"))
	  {
	    if (i + 1 == argc)
	      goto missing_filename;
	    else
	      {
		struct pending_option *o = (struct pending_option *)
		  xmalloc (sizeof (struct pending_option));
		o->arg = argv[++i];
		o->next = NULL;

		APPEND (opts->pending, imacros, o);
	      }
	  }
	/* Add directory to end of path for includes,
	   with the default prefix at the front of its name.  */
	else if (!strcmp (argv[i], "-iwithprefix"))
	  {
	    char *fname;
	    int len;
	    if (i + 1 == argc)
	      goto missing_dirname;
	    ++i;
	    len = strlen (argv[i]);

	    if (opts->include_prefix != 0)
	      {
		fname = xmalloc (opts->include_prefix_len + len + 1);
		memcpy (fname, opts->include_prefix, opts->include_prefix_len);
		memcpy (fname + opts->include_prefix_len, argv[i], len + 1);
	      }
	    else
	      {
		fname = xmalloc (sizeof GCC_INCLUDE_DIR - 8 + len);
		memcpy (fname, GCC_INCLUDE_DIR, sizeof GCC_INCLUDE_DIR - 9);
		memcpy (fname + sizeof GCC_INCLUDE_DIR - 9, argv[i], len + 1);
	      }
	  
	    append_include_chain (pfile, opts->pending, fname, SYSTEM, 0);
	  }
	/* Add directory to main path for includes,
	   with the default prefix at the front of its name.  */
	else if (!strcmp (argv[i], "-iwithprefixbefore"))
	  {
	    char *fname;
	    int len;
	    if (i + 1 == argc)
	      goto missing_dirname;
	    ++i;
	    len = strlen (argv[i]);

	    if (opts->include_prefix != 0)
	      {
		fname = xmalloc (opts->include_prefix_len + len + 1);
		memcpy (fname, opts->include_prefix, opts->include_prefix_len);
		memcpy (fname + opts->include_prefix_len, argv[i], len + 1);
	      }
	    else
	      {
		fname = xmalloc (sizeof GCC_INCLUDE_DIR - 8 + len);
		memcpy (fname, GCC_INCLUDE_DIR, sizeof GCC_INCLUDE_DIR - 9);
		memcpy (fname + sizeof GCC_INCLUDE_DIR - 9, argv[i], len + 1);
	      }
	  
	    append_include_chain (pfile, opts->pending, fname, BRACKET, 0);
	  }
	/* Add directory to end of path for includes.  */
	else if (!strcmp (argv[i], "-idirafter"))
	  {
	    if (i + 1 == argc)
	      goto missing_dirname;
	    append_include_chain (pfile, opts->pending,
				  xstrdup (argv[++i]), AFTER, 0);
	  }
	else if (!strcmp (argv[i], "-iprefix"))
	  {
	    if (i + 1 == argc)
	      goto missing_filename;
	    else
	      {
		opts->include_prefix = argv[++i];
		opts->include_prefix_len = strlen (argv[i]);
	      }
	  }
	break;
      
      case 'o':
	if (opts->out_fname != NULL)
	  {
	    cpp_fatal (pfile, "Output filename specified twice");
	    return argc;
	  }
	if (i + 1 == argc)
	  goto missing_filename;
	opts->out_fname = argv[++i];
	if (!strcmp (opts->out_fname, "-"))
	  opts->out_fname = "";
	break;
      
      case 'p':
	if (!strcmp (argv[i], "-pedantic"))
	  SET_CPP_PEDANTIC (pfile);
	else if (!strcmp (argv[i], "-pedantic-errors"))
	  {
	    SET_CPP_PEDANTIC (pfile);
	    opts->pedantic_errors = 1;
	  }
	break;
      
      case 't':
	if (!strcmp (argv[i], "-traditional"))
	  {
	    opts->traditional = 1;
	    opts->cplusplus_comments = 0;
	    opts->trigraphs = 0;
	    opts->warn_trigraphs = 0;
	  }
	else if (!strcmp (argv[i], "-trigraphs"))
	  opts->trigraphs = 1;
	break;
      
      case 'l':
	if (! strcmp (argv[i], "-lang-c"))
	  opts->cplusplus = 0, opts->cplusplus_comments = 1, opts->c89 = 0,
	    opts->c99 = 1, opts->objc = 0;
	if (! strcmp (argv[i], "-lang-c89"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 0;
	    opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	    opts->trigraphs = 1;
	    new_pending_define (opts, "__STRICT_ANSI__");
	  }
	if (! strcmp (argv[i], "-lang-c++"))
	  opts->cplusplus = 1, opts->cplusplus_comments = 1, opts->c89 = 0,
	    opts->c99 = 0, opts->objc = 0;
	if (! strcmp (argv[i], "-lang-objc"))
	  opts->cplusplus = 0, opts->cplusplus_comments = 1, opts->c89 = 0,
	    opts->c99 = 0, opts->objc = 1;
	if (! strcmp (argv[i], "-lang-objc++"))
	  opts->cplusplus = 1, opts->cplusplus_comments = 1, opts->c89 = 0,
	    opts->c99 = 0, opts->objc = 1;
	if (! strcmp (argv[i], "-lang-asm"))
	  opts->lang_asm = 1;
	if (! strcmp (argv[i], "-lang-fortran"))
	  opts->lang_fortran = 1, opts->cplusplus_comments = 0;
	if (! strcmp (argv[i], "-lang-chill"))
	  opts->objc = 0, opts->cplusplus = 0, opts->chill = 1,
	    opts->traditional = 1;
	break;
      
      case '+':
	opts->cplusplus = 1, opts->cplusplus_comments = 1;
	break;

      case 's':
	if (!strcmp (argv[i], "-std=gnu89"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 1;
	    opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	  }
	else if (!strcmp (argv[i], "-std=gnu9x")
		 || !strcmp (argv[i], "-std=gnu99"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 1;
	    opts->c89 = 0, opts->c99 = 1, opts->objc = 0;
	    new_pending_define (opts, "__STDC_VERSION__=199901L");
	  }
	else if (!strcmp (argv[i], "-std=iso9899:1990")
		 || !strcmp (argv[i], "-std=c89"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 0;
	    opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	    opts->trigraphs = 1;
	    new_pending_define (opts, "__STRICT_ANSI__");
	  }
	else if (!strcmp (argv[i], "-std=iso9899:199409"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 0;
	    opts->c89 = 1, opts->c99 = 0, opts->objc = 0;
	    opts->trigraphs = 1;
	    new_pending_define (opts, "__STRICT_ANSI__");
	    new_pending_define (opts, "__STDC_VERSION__=199409L");
	  }
	else if (!strcmp (argv[i], "-std=iso9899:199x")
		 || !strcmp (argv[i], "-std=iso9899:1999")
		 || !strcmp (argv[i], "-std=c9x")
		 || !strcmp (argv[i], "-std=c99"))
	  {
	    opts->cplusplus = 0, opts->cplusplus_comments = 1;
	    opts->c89 = 0, opts->c99 = 1, opts->objc = 0;
	    opts->trigraphs = 1;
	    new_pending_define (opts, "__STRICT_ANSI__");
	    new_pending_define (opts, "__STDC_VERSION__=199901L");
	  }
	break;

      case 'w':
	opts->inhibit_warnings = 1;
	break;
      
      case 'W':
	if (!strcmp (argv[i], "-Wtrigraphs"))
	  opts->warn_trigraphs = 1;
	else if (!strcmp (argv[i], "-Wno-trigraphs"))
	  opts->warn_trigraphs = 0;
	else if (!strcmp (argv[i], "-Wcomment"))
	  opts->warn_comments = 1;
	else if (!strcmp (argv[i], "-Wno-comment"))
	  opts->warn_comments = 0;
	else if (!strcmp (argv[i], "-Wcomments"))
	  opts->warn_comments = 1;
	else if (!strcmp (argv[i], "-Wno-comments"))
	  opts->warn_comments = 0;
	else if (!strcmp (argv[i], "-Wtraditional"))
	  opts->warn_stringify = 1;
	else if (!strcmp (argv[i], "-Wno-traditional"))
	  opts->warn_stringify = 0;
	else if (!strcmp (argv[i], "-Wundef"))
	  opts->warn_undef = 1;
	else if (!strcmp (argv[i], "-Wno-undef"))
	  opts->warn_undef = 0;
	else if (!strcmp (argv[i], "-Wimport"))
	  opts->warn_import = 1;
	else if (!strcmp (argv[i], "-Wno-import"))
	  opts->warn_import = 0;
	else if (!strcmp (argv[i], "-Werror"))
	  opts->warnings_are_errors = 1;
	else if (!strcmp (argv[i], "-Wno-error"))
	  opts->warnings_are_errors = 0;
	else if (!strcmp (argv[i], "-Wall"))
	  {
	    opts->warn_trigraphs = 1;
	    opts->warn_comments = 1;
	  }
	break;
      
      case 'M':
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
	if (!strcmp (argv[i], "-MG"))
	  {
	    opts->print_deps_missing_files = 1;
	    break;
	  }
	if (!strcmp (argv[i], "-M"))
	  opts->print_deps = 2;
	else if (!strcmp (argv[i], "-MM"))
	  opts->print_deps = 1;
	else if (!strcmp (argv[i], "-MD"))
	  opts->print_deps = 2;
	else if (!strcmp (argv[i], "-MMD"))
	  opts->print_deps = 1;
	/* For -MD and -MMD options, write deps on file named by next arg.  */
	if (!strcmp (argv[i], "-MD") || !strcmp (argv[i], "-MMD"))
	  {
	    if (i+1 == argc)
	      goto missing_filename;
	    opts->deps_file = argv[++i];
	  }
	else
	  {
	    /* For -M and -MM, write deps on standard output
	       and suppress the usual output.  */
	    opts->no_output = 1;
	  }	  
	break;
      
      case 'd':
	{
	  char *p = argv[i] + 2;
	  char c;
	  while ((c = *p++) != 0)
	    {
	      /* Arg to -d specifies what parts of macros to dump */
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
	}
	break;
    
      case 'g':
	if (argv[i][2] == '3')
	  opts->debug_output = 1;
	break;
      
      case '-':
	if (!strcmp (argv[i], "--help"))
	  print_help ();
	else if (!strcmp (argv[i], "--version"))
	  fprintf (stderr, _("GNU CPP version %s (cpplib)\n"), version_string);
	exit (0);  /* XXX */
	break;
	
      case 'v':
	fprintf (stderr, _("GNU CPP version %s (cpplib)\n"), version_string);
#ifdef TARGET_VERSION
	TARGET_VERSION;
#endif
	fputc ('\n', stderr);
	opts->verbose = 1;
	break;
      
      case 'H':
	opts->print_include_names = 1;
	break;
      
      case 'D':
	{
	  const char *text;
	  if (argv[i][2] != 0)
	    text = argv[i] + 2;
	  else if (i + 1 == argc)
	    {
	      cpp_fatal (pfile, "Macro name missing after -D option");
	      return argc;
	    }
	  else
	    text = argv[++i];
	  new_pending_define (opts, text);
	}
	break;
      
      case 'A':
	{
	  char *p;
	
	  if (argv[i][2] != 0)
	    p = argv[i] + 2;
	  else if (i + 1 == argc)
	    {
	      cpp_fatal (pfile, "Assertion missing after -A option");
	      return argc;
	    }
	  else
	    p = argv[++i];
	
	  if (strcmp (p, "-"))
	    {
	      struct pending_option *o = (struct pending_option *)
		xmalloc (sizeof (struct pending_option));

	      o->arg = p;
	      o->next = NULL;
	      o->undef = 0;
	      APPEND (opts->pending, assert, o);
	    }
	  else
	    {
	      /* -A- eliminates all predefined macros and assertions.
		 Let's include also any that were specified earlier
		 on the command line.  That way we can get rid of any
		 that were passed automatically in from GCC.  */
	      struct pending_option *o1, *o2;

	      o1 = opts->pending->define_head;
	      while (o1)
		{
		  o2 = o1->next;
		  free (o1);
		  o1 = o2;
		}
	      o1 = opts->pending->assert_head;
	      while (o1)
		{
		  o2 = o1->next;
		  free (o1);
		  o1 = o2;
		}
	      opts->pending->assert_head = NULL;
	      opts->pending->assert_tail = NULL;
	      opts->pending->define_head = NULL;
	      opts->pending->define_tail = NULL;
	    }
	}
	break;
    
      case 'U':
	{
	  struct pending_option *o = (struct pending_option *)
	    xmalloc (sizeof (struct pending_option));
	  
	  if (argv[i][2] != 0)
	    o->arg = argv[i] + 2;
	  else if (i + 1 == argc)
	    {
	      cpp_fatal (pfile, "Macro name missing after -U option");
	      return argc;
	    }
	  else
	    o->arg = argv[++i];

	  o->next = NULL;
	  o->undef = 1;
	  APPEND (opts->pending, define, o);
	}
	break;
      
      case 'C':
	opts->discard_comments = 0;
	break;
      
      case 'E':			/* -E comes from cc -E; ignore it.  */
	break;
      
      case 'P':
	opts->no_line_commands = 1;
	break;
      
      case '$':			/* Don't include $ in identifiers.  */
	opts->dollars_in_ident = 0;
	break;
      
      case 'n':
	if (!strcmp (argv[i], "-nostdinc"))
	  /* -nostdinc causes no default include directories.
	     You must specify all include-file directories with -I.  */
	  opts->no_standard_includes = 1;
	else if (!strcmp (argv[i], "-nostdinc++"))
	  /* -nostdinc++ causes no default C++-specific include directories. */
	  opts->no_standard_cplusplus_includes = 1;
	break;
      
      case 'r':
	if (!strcmp (argv[i], "-remap"))
	  opts->remap = 1;
	break;
      
      case '\0': /* JF handle '-' as file name meaning stdin or stdout */
	if (opts->in_fname == NULL)
	  opts->in_fname = "";
	else if (opts->out_fname == NULL)
	  opts->out_fname = "";
	else
	  return i;  /* error */
	break;

      default:
	return i;
      }

  return i + 1;

 missing_filename:
  cpp_fatal (pfile, "Filename missing after `%s' option", argv[i]);
  return argc;
 missing_dirname:
  cpp_fatal (pfile, "Directory name missing after `%s' option", argv[i]);
  return argc;
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
  -nostdinc                 Do not search system include directories\n\
                             (dirs specified with -isystem will still be used)\n\
  -nostdinc++               Do not search system include directories for C++\n\
  -o <file>                 Put output into <file>\n\
  -pedantic                 Issue all warnings demanded by strict ANSI C\n\
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
  -g                        Include #define and #undef directives in the output\n\
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
  -h or --help              Display this information\n\
"), stdout);
}
