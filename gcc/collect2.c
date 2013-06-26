/* Collect static initialization info into data structures that can be
   traversed by C++ initialization and finalization routines.
   Copyright (C) 1992-2013 Free Software Foundation, Inc.
   Contributed by Chris Smith (csmith@convex.com).
   Heavily modified by Michael Meissner (meissner@cygnus.com),
   Per Bothner (bothner@cygnus.com), and John Gilmore (gnu@cygnus.com).

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


/* Build tables of static constructors and destructors and run ld.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "filenames.h"
#include "file-find.h"

/* TARGET_64BIT may be defined to use driver specific functionality. */
#undef TARGET_64BIT
#define TARGET_64BIT TARGET_64BIT_DEFAULT

#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

#define COLLECT

#include "collect2.h"
#include "collect2-aix.h"
#include "diagnostic.h"
#include "demangle.h"
#include "obstack.h"
#include "intl.h"
#include "version.h"

/* On certain systems, we have code that works by scanning the object file
   directly.  But this code uses system-specific header files and library
   functions, so turn it off in a cross-compiler.  Likewise, the names of
   the utilities are not correct for a cross-compiler; we have to hope that
   cross-versions are in the proper directories.  */

#ifdef CROSS_DIRECTORY_STRUCTURE
#ifndef CROSS_AIX_SUPPORT
#undef OBJECT_FORMAT_COFF
#endif
#undef MD_EXEC_PREFIX
#undef REAL_LD_FILE_NAME
#undef REAL_NM_FILE_NAME
#undef REAL_STRIP_FILE_NAME
#endif

/* If we cannot use a special method, use the ordinary one:
   run nm to find what symbols are present.
   In a cross-compiler, this means you need a cross nm,
   but that is not quite as unpleasant as special headers.  */

#if !defined (OBJECT_FORMAT_COFF)
#define OBJECT_FORMAT_NONE
#endif

#ifdef OBJECT_FORMAT_COFF

#ifndef CROSS_DIRECTORY_STRUCTURE
#include <a.out.h>
#include <ar.h>

#ifdef UMAX
#include <sgs.h>
#endif

/* Many versions of ldfcn.h define these.  */
#ifdef FREAD
#undef FREAD
#undef FWRITE
#endif

#include <ldfcn.h>
#endif

/* Some systems have an ISCOFF macro, but others do not.  In some cases
   the macro may be wrong.  MY_ISCOFF is defined in tm.h files for machines
   that either do not have an ISCOFF macro in /usr/include or for those
   where it is wrong.  */

#ifndef MY_ISCOFF
#define MY_ISCOFF(X) ISCOFF (X)
#endif

#endif /* OBJECT_FORMAT_COFF */

#ifdef OBJECT_FORMAT_NONE

/* Default flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-n"
#endif

#endif /* OBJECT_FORMAT_NONE */

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#endif

/* This must match tree.h.  */
#define DEFAULT_INIT_PRIORITY 65535

#ifndef COLLECT_SHARED_INIT_FUNC
#define COLLECT_SHARED_INIT_FUNC(STREAM, FUNC) \
  fprintf ((STREAM), "void _GLOBAL__DI() {\n\t%s();\n}\n", (FUNC))
#endif
#ifndef COLLECT_SHARED_FINI_FUNC
#define COLLECT_SHARED_FINI_FUNC(STREAM, FUNC) \
  fprintf ((STREAM), "void _GLOBAL__DD() {\n\t%s();\n}\n", (FUNC))
#endif

#ifdef LDD_SUFFIX
#define SCAN_LIBRARIES
#endif

#ifndef SHLIB_SUFFIX
#define SHLIB_SUFFIX ".so"
#endif

#ifdef USE_COLLECT2
int do_collecting = 1;
#else
int do_collecting = 0;
#endif

/* Cook up an always defined indication of whether we proceed the
   "EXPORT_LIST" way.  */

#ifdef COLLECT_EXPORT_LIST
#define DO_COLLECT_EXPORT_LIST 1
#else
#define DO_COLLECT_EXPORT_LIST 0
#endif

/* Nonzero if we should suppress the automatic demangling of identifiers
   in linker error messages.  Set from COLLECT_NO_DEMANGLE.  */
int no_demangle;

/* Linked lists of constructor and destructor names.  */

struct id
{
  struct id *next;
  int sequence;
  char name[1];
};

struct head
{
  struct id *first;
  struct id *last;
  int number;
};

bool vflag;				/* true if -v or --version */ 
static int rflag;			/* true if -r */
static int strip_flag;			/* true if -s */
#ifdef COLLECT_EXPORT_LIST
static int export_flag;                 /* true if -bE */
static int aix64_flag;			/* true if -b64 */
static int aixrtl_flag;			/* true if -brtl */
#endif

enum lto_mode_d {
  LTO_MODE_NONE,			/* Not doing LTO.  */
  LTO_MODE_LTO,				/* Normal LTO.  */
  LTO_MODE_WHOPR			/* WHOPR.  */
};

/* Current LTO mode.  */
static enum lto_mode_d lto_mode = LTO_MODE_NONE;

bool debug;				/* true if -debug */
bool helpflag;			/* true if --help */

static int shared_obj;			/* true if -shared */

static const char *c_file;		/* <xxx>.c for constructor/destructor list.  */
static const char *o_file;		/* <xxx>.o for constructor/destructor list.  */
#ifdef COLLECT_EXPORT_LIST
static const char *export_file;		/* <xxx>.x for AIX export list.  */
#endif
static char **lto_o_files;		/* Output files for LTO.  */
const char *ldout;			/* File for ld stdout.  */
const char *lderrout;			/* File for ld stderr.  */
static const char *output_file;		/* Output file for ld.  */
static const char *nm_file_name;	/* pathname of nm */
#ifdef LDD_SUFFIX
static const char *ldd_file_name;	/* pathname of ldd (or equivalent) */
#endif
static const char *strip_file_name;		/* pathname of strip */
const char *c_file_name;		/* pathname of gcc */
static char *initname, *fininame;	/* names of init and fini funcs */

static struct head constructors;	/* list of constructors found */
static struct head destructors;		/* list of destructors found */
#ifdef COLLECT_EXPORT_LIST
static struct head exports;		/* list of exported symbols */
#endif
static struct head frame_tables;	/* list of frame unwind info tables */

static bool at_file_supplied;		/* Whether to use @file arguments */
static char *response_file;		/* Name of any current response file */

struct obstack temporary_obstack;
char * temporary_firstobj;

/* A string that must be prepended to a target OS path in order to find
   it on the host system.  */
#ifdef TARGET_SYSTEM_ROOT
static const char *target_system_root = TARGET_SYSTEM_ROOT;
#else
static const char *target_system_root = "";
#endif

/* Whether we may unlink the output file, which should be set as soon as we
   know we have successfully produced it.  This is typically useful to prevent
   blindly attempting to unlink a read-only output that the target linker
   would leave untouched.  */
bool may_unlink_output_file = false;

#ifdef COLLECT_EXPORT_LIST
/* Lists to keep libraries to be scanned for global constructors/destructors.  */
static struct head libs;                    /* list of libraries */
static struct path_prefix cmdline_lib_dirs; /* directories specified with -L */
static struct path_prefix libpath_lib_dirs; /* directories in LIBPATH */
static struct path_prefix *libpaths[3] = {&cmdline_lib_dirs,
					  &libpath_lib_dirs, NULL};
#endif

/* List of names of object files containing LTO information.
   These are a subset of the object file names appearing on the
   command line, and must be identical, in the sense of pointer
   equality, with the names passed to maybe_run_lto_and_relink().  */

struct lto_object
{
  const char *name;		/* Name of object file.  */
  struct lto_object *next;	/* Next in linked list.  */
};

struct lto_object_list
{
  struct lto_object *first;	/* First list element.  */
  struct lto_object *last;	/* Last list element.  */
};

static struct lto_object_list lto_objects;

/* Special kinds of symbols that a name may denote.  */

typedef enum {
  SYM_REGULAR = 0,  /* nothing special  */

  SYM_CTOR = 1,  /* constructor */
  SYM_DTOR = 2,  /* destructor  */
  SYM_INIT = 3,  /* shared object routine that calls all the ctors  */
  SYM_FINI = 4,  /* shared object routine that calls all the dtors  */
  SYM_DWEH = 5   /* DWARF exception handling table  */
} symkind;

static symkind is_ctor_dtor (const char *);

static void handler (int);
static void do_wait (const char *, struct pex_obj *);
static void fork_execute (const char *, char **);
static void maybe_unlink (const char *);
static void maybe_unlink_list (char **);
static void add_to_list (struct head *, const char *);
static int extract_init_priority (const char *);
static void sort_ids (struct head *);
static void write_list (FILE *, const char *, struct id *);
#ifdef COLLECT_EXPORT_LIST
static void dump_list (FILE *, const char *, struct id *);
#endif
#if 0
static void dump_prefix_list (FILE *, const char *, struct prefix_list *);
#endif
static void write_list_with_asm (FILE *, const char *, struct id *);
static void write_c_file (FILE *, const char *);
static void write_c_file_stat (FILE *, const char *);
#ifndef LD_INIT_SWITCH
static void write_c_file_glob (FILE *, const char *);
#endif
#ifdef SCAN_LIBRARIES
static void scan_libraries (const char *);
#endif
#ifdef COLLECT_EXPORT_LIST
#if 0
static int is_in_list (const char *, struct id *);
#endif
static void write_aix_file (FILE *, struct id *);
static char *resolve_lib_name (const char *);
#endif
static char *extract_string (const char **);
static void post_ld_pass (bool);
static void process_args (int *argcp, char **argv);

/* Enumerations describing which pass this is for scanning the
   program file ...  */

typedef enum {
  PASS_FIRST,				/* without constructors */
  PASS_OBJ,				/* individual objects */
  PASS_LIB,				/* looking for shared libraries */
  PASS_SECOND,				/* with constructors linked in */
  PASS_LTOINFO				/* looking for objects with LTO info */
} scanpass;

/* ... and which kinds of symbols are to be considered.  */

enum scanfilter_masks {
  SCAN_NOTHING = 0,

  SCAN_CTOR = 1 << SYM_CTOR,
  SCAN_DTOR = 1 << SYM_DTOR,
  SCAN_INIT = 1 << SYM_INIT,
  SCAN_FINI = 1 << SYM_FINI,
  SCAN_DWEH = 1 << SYM_DWEH,
  SCAN_ALL  = ~0
};

/* This type is used for parameters and variables which hold
   combinations of the flags in enum scanfilter_masks.  */
typedef int scanfilter;

/* Scan the name list of the loaded program for the symbols g++ uses for
   static constructors and destructors.

   The SCANPASS argument tells which collect processing pass this is for and
   the SCANFILTER argument tells which kinds of symbols to consider in this
   pass.  Symbols of a special kind not in the filter mask are considered as
   regular ones.

   The constructor table begins at __CTOR_LIST__ and contains a count of the
   number of pointers (or -1 if the constructors are built in a separate
   section by the linker), followed by the pointers to the constructor
   functions, terminated with a null pointer.  The destructor table has the
   same format, and begins at __DTOR_LIST__.  */

static void scan_prog_file (const char *, scanpass, scanfilter);


/* Delete tempfiles and exit function.  */

static void
collect_atexit (void)
{
  if (c_file != 0 && c_file[0])
    maybe_unlink (c_file);

  if (o_file != 0 && o_file[0])
    maybe_unlink (o_file);

#ifdef COLLECT_EXPORT_LIST
  if (export_file != 0 && export_file[0])
    maybe_unlink (export_file);
#endif

  if (lto_o_files)
    maybe_unlink_list (lto_o_files);

  if (ldout != 0 && ldout[0])
    {
      dump_ld_file (ldout, stdout);
      maybe_unlink (ldout);
    }

  if (lderrout != 0 && lderrout[0])
    {
      dump_ld_file (lderrout, stderr);
      maybe_unlink (lderrout);
    }

  if (response_file)
    maybe_unlink (response_file);
}


/* Notify user of a non-error.  */
void
notice (const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
}

/* Notify user of a non-error, without translating the format string.  */
void
notice_translated (const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (stderr, cmsgid, ap);
  va_end (ap);
}

static void
handler (int signo)
{
  if (c_file != 0 && c_file[0])
    maybe_unlink (c_file);

  if (o_file != 0 && o_file[0])
    maybe_unlink (o_file);

  if (ldout != 0 && ldout[0])
    maybe_unlink (ldout);

  if (lderrout != 0 && lderrout[0])
    maybe_unlink (lderrout);

#ifdef COLLECT_EXPORT_LIST
  if (export_file != 0 && export_file[0])
    maybe_unlink (export_file);
#endif

  if (lto_o_files)
    maybe_unlink_list (lto_o_files);

  if (response_file)
    maybe_unlink (response_file);

  signal (signo, SIG_DFL);
  raise (signo);
}


int
file_exists (const char *name)
{
  return access (name, R_OK) == 0;
}

/* Parse a reasonable subset of shell quoting syntax.  */

static char *
extract_string (const char **pp)
{
  const char *p = *pp;
  int backquote = 0;
  int inside = 0;

  for (;;)
    {
      char c = *p;
      if (c == '\0')
	break;
      ++p;
      if (backquote)
	obstack_1grow (&temporary_obstack, c);
      else if (! inside && c == ' ')
	break;
      else if (! inside && c == '\\')
	backquote = 1;
      else if (c == '\'')
	inside = !inside;
      else
	obstack_1grow (&temporary_obstack, c);
    }

  obstack_1grow (&temporary_obstack, '\0');
  *pp = p;
  return XOBFINISH (&temporary_obstack, char *);
}

void
dump_ld_file (const char *name, FILE *to)
{
  FILE *stream = fopen (name, "r");

  if (stream == 0)
    return;
  while (1)
    {
      int c;
      while (c = getc (stream),
	     c != EOF && (ISIDNUM (c) || c == '$' || c == '.'))
	obstack_1grow (&temporary_obstack, c);
      if (obstack_object_size (&temporary_obstack) > 0)
	{
	  const char *word, *p;
	  char *result;
	  obstack_1grow (&temporary_obstack, '\0');
	  word = XOBFINISH (&temporary_obstack, const char *);

	  if (*word == '.')
	    ++word, putc ('.', to);
	  p = word;
	  if (!strncmp (p, USER_LABEL_PREFIX, strlen (USER_LABEL_PREFIX)))
	    p += strlen (USER_LABEL_PREFIX);

#ifdef HAVE_LD_DEMANGLE
	  result = 0;
#else
	  if (no_demangle)
	    result = 0;
	  else
	    result = cplus_demangle (p, DMGL_PARAMS | DMGL_ANSI | DMGL_VERBOSE);
#endif

	  if (result)
	    {
	      int diff;
	      fputs (result, to);

	      diff = strlen (word) - strlen (result);
	      while (diff > 0 && c == ' ')
		--diff, putc (' ', to);
	      if (diff < 0 && c == ' ')
		{
		  while (diff < 0 && c == ' ')
		    ++diff, c = getc (stream);
		  if (!ISSPACE (c))
		    {
		      /* Make sure we output at least one space, or
			 the demangled symbol name will run into
			 whatever text follows.  */
		      putc (' ', to);
		    }
		}

	      free (result);
	    }
	  else
	    fputs (word, to);

	  fflush (to);
	  obstack_free (&temporary_obstack, temporary_firstobj);
	}
      if (c == EOF)
	break;
      putc (c, to);
    }
  fclose (stream);
}

/* Return the kind of symbol denoted by name S.  */

static symkind
is_ctor_dtor (const char *s)
{
  struct names { const char *const name; const int len; symkind ret;
    const int two_underscores; };

  const struct names *p;
  int ch;
  const char *orig_s = s;

  static const struct names special[] = {
#ifndef NO_DOLLAR_IN_LABEL
    { "GLOBAL__I$", sizeof ("GLOBAL__I$")-1, SYM_CTOR, 0 },
    { "GLOBAL__D$", sizeof ("GLOBAL__D$")-1, SYM_DTOR, 0 },
#else
#ifndef NO_DOT_IN_LABEL
    { "GLOBAL__I.", sizeof ("GLOBAL__I.")-1, SYM_CTOR, 0 },
    { "GLOBAL__D.", sizeof ("GLOBAL__D.")-1, SYM_DTOR, 0 },
#endif /* NO_DOT_IN_LABEL */
#endif /* NO_DOLLAR_IN_LABEL */
    { "GLOBAL__I_", sizeof ("GLOBAL__I_")-1, SYM_CTOR, 0 },
    { "GLOBAL__D_", sizeof ("GLOBAL__D_")-1, SYM_DTOR, 0 },
    { "GLOBAL__F_", sizeof ("GLOBAL__F_")-1, SYM_DWEH, 0 },
    { "GLOBAL__FI_", sizeof ("GLOBAL__FI_")-1, SYM_INIT, 0 },
    { "GLOBAL__FD_", sizeof ("GLOBAL__FD_")-1, SYM_FINI, 0 },
    { NULL, 0, SYM_REGULAR, 0 }
  };

  while ((ch = *s) == '_')
    ++s;

  if (s == orig_s)
    return SYM_REGULAR;

  for (p = &special[0]; p->len > 0; p++)
    {
      if (ch == p->name[0]
	  && (!p->two_underscores || ((s - orig_s) >= 2))
	  && strncmp(s, p->name, p->len) == 0)
	{
	  return p->ret;
	}
    }
  return SYM_REGULAR;
}

/* We maintain two prefix lists: one from COMPILER_PATH environment variable
   and one from the PATH variable.  */

static struct path_prefix cpath, path;

#ifdef CROSS_DIRECTORY_STRUCTURE
/* This is the name of the target machine.  We use it to form the name
   of the files to execute.  */

static const char *const target_machine = TARGET_MACHINE;
#endif

/* Search for NAME using prefix list PPREFIX.  We only look for executable
   files.

   Return 0 if not found, otherwise return its name, allocated with malloc.  */

#ifdef OBJECT_FORMAT_NONE

/* Add an entry for the object file NAME to object file list LIST.
   New entries are added at the end of the list. The original pointer
   value of NAME is preserved, i.e., no string copy is performed.  */

static void
add_lto_object (struct lto_object_list *list, const char *name)
{
  struct lto_object *n = XNEW (struct lto_object);
  n->name = name;
  n->next = NULL;

  if (list->last)
    list->last->next = n;
  else
    list->first = n;

  list->last = n;
}
#endif /* OBJECT_FORMAT_NONE */


/* Perform a link-time recompilation and relink if any of the object
   files contain LTO info.  The linker command line LTO_LD_ARGV
   represents the linker command that would produce a final executable
   without the use of LTO. OBJECT_LST is a vector of object file names
   appearing in LTO_LD_ARGV that are to be considered for link-time
   recompilation, where OBJECT is a pointer to the last valid element.
   (This awkward convention avoids an impedance mismatch with the
   usage of similarly-named variables in main().)  The elements of
   OBJECT_LST must be identical, i.e., pointer equal, to the
   corresponding arguments in LTO_LD_ARGV.

   Upon entry, at least one linker run has been performed without the
   use of any LTO info that might be present.  Any recompilations
   necessary for template instantiations have been performed, and
   initializer/finalizer tables have been created if needed and
   included in the linker command line LTO_LD_ARGV. If any of the
   object files contain LTO info, we run the LTO back end on all such
   files, and perform the final link with the LTO back end output
   substituted for the LTO-optimized files.  In some cases, a final
   link with all link-time generated code has already been performed,
   so there is no need to relink if no LTO info is found.  In other
   cases, our caller has not produced the final executable, and is
   relying on us to perform the required link whether LTO info is
   present or not.  In that case, the FORCE argument should be true.
   Note that the linker command line argument LTO_LD_ARGV passed into
   this function may be modified in place.  */

static void
maybe_run_lto_and_relink (char **lto_ld_argv, char **object_lst,
			  const char **object, bool force)
{
  const char **object_file = CONST_CAST2 (const char **, char **, object_lst);

  int num_lto_c_args = 1;    /* Allow space for the terminating NULL.  */

  while (object_file < object)
  {
    /* If file contains LTO info, add it to the list of LTO objects.  */
    scan_prog_file (*object_file++, PASS_LTOINFO, SCAN_ALL);

    /* Increment the argument count by the number of object file arguments
       we will add.  An upper bound suffices, so just count all of the
       object files regardless of whether they contain LTO info.  */
    num_lto_c_args++;
  }

  if (lto_objects.first)
    {
      char **lto_c_argv;
      const char **lto_c_ptr;
      char **p;
      char **lto_o_ptr;
      struct lto_object *list;
      char *lto_wrapper = getenv ("COLLECT_LTO_WRAPPER");
      struct pex_obj *pex;
      const char *prog = "lto-wrapper";
      int lto_ld_argv_size = 0;
      char **out_lto_ld_argv;
      int out_lto_ld_argv_size;
      size_t num_files;

      if (!lto_wrapper)
	fatal_error ("COLLECT_LTO_WRAPPER must be set");

      num_lto_c_args++;

      /* There is at least one object file containing LTO info,
         so we need to run the LTO back end and relink.

	 To do so we build updated ld arguments with first
	 LTO object replaced by all partitions and other LTO
	 objects removed.  */

      lto_c_argv = (char **) xcalloc (sizeof (char *), num_lto_c_args);
      lto_c_ptr = CONST_CAST2 (const char **, char **, lto_c_argv);

      *lto_c_ptr++ = lto_wrapper;

      /* Add LTO objects to the wrapper command line.  */
      for (list = lto_objects.first; list; list = list->next)
	*lto_c_ptr++ = list->name;

      *lto_c_ptr = NULL;

      /* Run the LTO back end.  */
      pex = collect_execute (prog, lto_c_argv, NULL, NULL, PEX_SEARCH);
      {
	int c;
	FILE *stream;
	size_t i;
	char *start, *end;

	stream = pex_read_output (pex, 0);
	gcc_assert (stream);

	num_files = 0;
	while ((c = getc (stream)) != EOF)
	  {
	    obstack_1grow (&temporary_obstack, c);
	    if (c == '\n')
	      ++num_files;
	  }

	lto_o_files = XNEWVEC (char *, num_files + 1);
	lto_o_files[num_files] = NULL;
	start = XOBFINISH (&temporary_obstack, char *);
	for (i = 0; i < num_files; ++i)
	  {
	    end = start;
	    while (*end != '\n')
	      ++end;
	    *end = '\0';

	    lto_o_files[i] = xstrdup (start);

	    start = end + 1;
	  }

	obstack_free (&temporary_obstack, temporary_firstobj);
      }
      do_wait (prog, pex);
      pex = NULL;

      /* Compute memory needed for new LD arguments.  At most number of original arguemtns
	 plus number of partitions.  */
      for (lto_ld_argv_size = 0; lto_ld_argv[lto_ld_argv_size]; lto_ld_argv_size++)
	;
      out_lto_ld_argv = XCNEWVEC(char *, num_files + lto_ld_argv_size + 1);
      out_lto_ld_argv_size = 0;

      /* After running the LTO back end, we will relink, substituting
	 the LTO output for the object files that we submitted to the
	 LTO. Here, we modify the linker command line for the relink.  */

      /* Copy all arguments until we find first LTO file.  */
      p = lto_ld_argv;
      while (*p != NULL)
        {
          for (list = lto_objects.first; list; list = list->next)
            if (*p == list->name) /* Note test for pointer equality!  */
	      break;
	  if (list)
	    break;
	  out_lto_ld_argv[out_lto_ld_argv_size++] = *p++;
        }

      /* Now insert all LTO partitions.  */
      lto_o_ptr = lto_o_files;
      while (*lto_o_ptr)
	out_lto_ld_argv[out_lto_ld_argv_size++] = *lto_o_ptr++;

      /* ... and copy the rest.  */
      while (*p != NULL)
        {
          for (list = lto_objects.first; list; list = list->next)
            if (*p == list->name) /* Note test for pointer equality!  */
	      break;
	  if (!list)
	    out_lto_ld_argv[out_lto_ld_argv_size++] = *p;
	  p++;
        }
      out_lto_ld_argv[out_lto_ld_argv_size++] = 0;

      /* Run the linker again, this time replacing the object files
         optimized by the LTO with the temporary file generated by the LTO.  */
      fork_execute ("ld", out_lto_ld_argv);
      post_ld_pass (true);
      free (lto_ld_argv);

      maybe_unlink_list (lto_o_files);
    }
  else if (force)
    {
      /* Our caller is relying on us to do the link
         even though there is no LTO back end work to be done.  */
      fork_execute ("ld", lto_ld_argv);
      post_ld_pass (false);
    }
}

/* Main program.  */

int
main (int argc, char **argv)
{
  enum linker_select
    {
      USE_DEFAULT_LD,
      USE_PLUGIN_LD,
      USE_GOLD_LD,
      USE_BFD_LD,
      USE_LD_MAX
    } selected_linker = USE_DEFAULT_LD;
  static const char *const ld_suffixes[USE_LD_MAX] =
    {
      "ld",
      PLUGIN_LD_SUFFIX,
      "ld.gold",
      "ld.bfd"
    };
  static const char *const real_ld_suffix = "real-ld";
  static const char *const collect_ld_suffix = "collect-ld";
  static const char *const nm_suffix	= "nm";
  static const char *const gnm_suffix	= "gnm";
#ifdef LDD_SUFFIX
  static const char *const ldd_suffix	= LDD_SUFFIX;
#endif
  static const char *const strip_suffix = "strip";
  static const char *const gstrip_suffix = "gstrip";

  const char *full_ld_suffixes[USE_LD_MAX];
#ifdef CROSS_DIRECTORY_STRUCTURE
  /* If we look for a program in the compiler directories, we just use
     the short name, since these directories are already system-specific.
     But it we look for a program in the system directories, we need to
     qualify the program name with the target machine.  */

  const char *const full_nm_suffix =
    concat (target_machine, "-", nm_suffix, NULL);
  const char *const full_gnm_suffix =
    concat (target_machine, "-", gnm_suffix, NULL);
#ifdef LDD_SUFFIX
  const char *const full_ldd_suffix =
    concat (target_machine, "-", ldd_suffix, NULL);
#endif
  const char *const full_strip_suffix =
    concat (target_machine, "-", strip_suffix, NULL);
  const char *const full_gstrip_suffix =
    concat (target_machine, "-", gstrip_suffix, NULL);
#else
#ifdef LDD_SUFFIX
  const char *const full_ldd_suffix	= ldd_suffix;
#endif
  const char *const full_nm_suffix	= nm_suffix;
  const char *const full_gnm_suffix	= gnm_suffix;
  const char *const full_strip_suffix	= strip_suffix;
  const char *const full_gstrip_suffix	= gstrip_suffix;
#endif /* CROSS_DIRECTORY_STRUCTURE */

  const char *arg;
  FILE *outf;
#ifdef COLLECT_EXPORT_LIST
  FILE *exportf;
#endif
  const char *ld_file_name;
  const char *p;
  char **c_argv;
  const char **c_ptr;
  char **ld1_argv;
  const char **ld1;
  bool use_plugin = false;
  bool use_collect_ld = false;

  /* The kinds of symbols we will have to consider when scanning the
     outcome of a first pass link.  This is ALL to start with, then might
     be adjusted before getting to the first pass link per se, typically on
     AIX where we perform an early scan of objects and libraries to fetch
     the list of global ctors/dtors and make sure they are not garbage
     collected.  */
  scanfilter ld1_filter = SCAN_ALL;

  char **ld2_argv;
  const char **ld2;
  char **object_lst;
  const char **object;
#ifdef TARGET_AIX_VERSION
  int object_nbr = argc;
#endif
  int first_file;
  int num_c_args;
  char **old_argv;
  int i;

  for (i = 0; i < USE_LD_MAX; i++)
    full_ld_suffixes[i]
#ifdef CROSS_DIRECTORY_STRUCTURE
      = concat (target_machine, "-", ld_suffixes[i], NULL);
#else
      = ld_suffixes[i];
#endif

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  old_argv = argv;
  expandargv (&argc, &argv);
  if (argv != old_argv)
    at_file_supplied = 1;

  process_args (&argc, argv);

  num_c_args = argc + 9;

#ifndef HAVE_LD_DEMANGLE
  no_demangle = !! getenv ("COLLECT_NO_DEMANGLE");

  /* Suppress demangling by the real linker, which may be broken.  */
  putenv (xstrdup ("COLLECT_NO_DEMANGLE=1"));
#endif

#if defined (COLLECT2_HOST_INITIALIZATION)
  /* Perform system dependent initialization, if necessary.  */
  COLLECT2_HOST_INITIALIZATION;
#endif

#ifdef SIGCHLD
  /* We *MUST* set SIGCHLD to SIG_DFL so that the wait4() call will
     receive the signal.  A different setting is inheritable */
  signal (SIGCHLD, SIG_DFL);
#endif

  if (atexit (collect_atexit) != 0)
    fatal_error ("atexit failed");

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

  /* Do not invoke xcalloc before this point, since locale needs to be
     set first, in case a diagnostic is issued.  */

  ld1_argv = XCNEWVEC (char *, argc + 4);
  ld1 = CONST_CAST2 (const char **, char **, ld1_argv);
  ld2_argv = XCNEWVEC (char *, argc + 11);
  ld2 = CONST_CAST2 (const char **, char **, ld2_argv);
  object_lst = XCNEWVEC (char *, argc);
  object = CONST_CAST2 (const char **, char **, object_lst);

#ifdef DEBUG
  debug = 1;
#endif

  /* Parse command line early for instances of -debug.  This allows
     the debug flag to be set before functions like find_a_file()
     are called.  We also look for the -flto or -flto-partition=none flag to know
     what LTO mode we are in.  */
  {
    bool no_partition = false;

    for (i = 1; argv[i] != NULL; i ++)
      {
	if (! strcmp (argv[i], "-debug"))
	  debug = true;
        else if (! strcmp (argv[i], "-flto-partition=none"))
	  no_partition = true;
        else if ((! strncmp (argv[i], "-flto=", 6)
		  || ! strcmp (argv[i], "-flto")) && ! use_plugin)
	  lto_mode = LTO_MODE_WHOPR;
	else if (!strncmp (argv[i], "-fno-lto", 8))
	  lto_mode = LTO_MODE_NONE;
        else if (! strcmp (argv[i], "-plugin"))
	  {
	    use_plugin = true;
	    lto_mode = LTO_MODE_NONE;
	    if (selected_linker == USE_DEFAULT_LD)
	      selected_linker = USE_PLUGIN_LD;
	  }
	else if (strcmp (argv[i], "-fuse-ld=bfd") == 0)
	  selected_linker = USE_BFD_LD;
	else if (strcmp (argv[i], "-fuse-ld=gold") == 0)
	  selected_linker = USE_GOLD_LD;

#ifdef COLLECT_EXPORT_LIST
	/* These flags are position independent, although their order
	   is important - subsequent flags override earlier ones. */
	else if (strcmp (argv[i], "-b64") == 0)
	    aix64_flag = 1;
	/* -bexport:filename always needs the :filename */
	else if (strncmp (argv[i], "-bE:", 4) == 0
	      || strncmp (argv[i], "-bexport:", 9) == 0)
	    export_flag = 1;
	else if (strcmp (argv[i], "-brtl") == 0
	      || strcmp (argv[i], "-bsvr4") == 0
	      || strcmp (argv[i], "-G") == 0)
	    aixrtl_flag = 1;
	else if (strcmp (argv[i], "-bnortl") == 0)
	    aixrtl_flag = 0;
#endif
      }
    vflag = debug;
    find_file_set_debug (debug);
    if (no_partition && lto_mode == LTO_MODE_WHOPR)
      lto_mode = LTO_MODE_LTO;
  }

#ifndef DEFAULT_A_OUT_NAME
  output_file = "a.out";
#else
  output_file = DEFAULT_A_OUT_NAME;
#endif

  obstack_begin (&temporary_obstack, 0);
  temporary_firstobj = (char *) obstack_alloc (&temporary_obstack, 0);

#ifndef HAVE_LD_DEMANGLE
  current_demangling_style = auto_demangling;
#endif
  p = getenv ("COLLECT_GCC_OPTIONS");
  while (p && *p)
    {
      const char *q = extract_string (&p);
      if (*q == '-' && (q[1] == 'm' || q[1] == 'f'))
	num_c_args++;
    }
  obstack_free (&temporary_obstack, temporary_firstobj);

  /* -fno-profile-arcs -fno-test-coverage -fno-branch-probabilities
     -fno-exceptions -w -fno-whole-program */
  num_c_args += 6;

  c_argv = XCNEWVEC (char *, num_c_args);
  c_ptr = CONST_CAST2 (const char **, char **, c_argv);

  if (argc < 2)
    fatal_error ("no arguments");

#ifdef SIGQUIT
  if (signal (SIGQUIT, SIG_IGN) != SIG_IGN)
    signal (SIGQUIT, handler);
#endif
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, handler);
#ifdef SIGALRM
  if (signal (SIGALRM, SIG_IGN) != SIG_IGN)
    signal (SIGALRM, handler);
#endif
#ifdef SIGHUP
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, handler);
#endif
  if (signal (SIGSEGV, SIG_IGN) != SIG_IGN)
    signal (SIGSEGV, handler);
#ifdef SIGBUS
  if (signal (SIGBUS, SIG_IGN) != SIG_IGN)
    signal (SIGBUS, handler);
#endif

  /* Extract COMPILER_PATH and PATH into our prefix list.  */
  prefix_from_env ("COMPILER_PATH", &cpath);
  prefix_from_env ("PATH", &path);

  /* Try to discover a valid linker/nm/strip to use.  */

  /* Maybe we know the right file to use (if not cross).  */
  ld_file_name = 0;
#ifdef DEFAULT_LINKER
  if (access (DEFAULT_LINKER, X_OK) == 0)
    ld_file_name = DEFAULT_LINKER;
  if (ld_file_name == 0)
#endif
#ifdef REAL_LD_FILE_NAME
  ld_file_name = find_a_file (&path, REAL_LD_FILE_NAME, X_OK);
  if (ld_file_name == 0)
#endif
  /* Search the (target-specific) compiler dirs for ld'.  */
  ld_file_name = find_a_file (&cpath, real_ld_suffix, X_OK);
  /* Likewise for `collect-ld'.  */
  if (ld_file_name == 0)
    {
      ld_file_name = find_a_file (&cpath, collect_ld_suffix, X_OK);
      use_collect_ld = ld_file_name != 0;
    }
  /* Search the compiler directories for `ld'.  We have protection against
     recursive calls in find_a_file.  */
  if (ld_file_name == 0)
    ld_file_name = find_a_file (&cpath, ld_suffixes[selected_linker], X_OK);
  /* Search the ordinary system bin directories
     for `ld' (if native linking) or `TARGET-ld' (if cross).  */
  if (ld_file_name == 0)
    ld_file_name = find_a_file (&path, full_ld_suffixes[selected_linker], X_OK);

#ifdef REAL_NM_FILE_NAME
  nm_file_name = find_a_file (&path, REAL_NM_FILE_NAME, X_OK);
  if (nm_file_name == 0)
#endif
  nm_file_name = find_a_file (&cpath, gnm_suffix, X_OK);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&path, full_gnm_suffix, X_OK);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&cpath, nm_suffix, X_OK);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&path, full_nm_suffix, X_OK);

#ifdef LDD_SUFFIX
  ldd_file_name = find_a_file (&cpath, ldd_suffix, X_OK);
  if (ldd_file_name == 0)
    ldd_file_name = find_a_file (&path, full_ldd_suffix, X_OK);
#endif

#ifdef REAL_STRIP_FILE_NAME
  strip_file_name = find_a_file (&path, REAL_STRIP_FILE_NAME, X_OK);
  if (strip_file_name == 0)
#endif
  strip_file_name = find_a_file (&cpath, gstrip_suffix, X_OK);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&path, full_gstrip_suffix, X_OK);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&cpath, strip_suffix, X_OK);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&path, full_strip_suffix, X_OK);

  /* Determine the full path name of the C compiler to use.  */
  c_file_name = getenv ("COLLECT_GCC");
  if (c_file_name == 0)
    {
#ifdef CROSS_DIRECTORY_STRUCTURE
      c_file_name = concat (target_machine, "-gcc", NULL);
#else
      c_file_name = "gcc";
#endif
    }

  p = find_a_file (&cpath, c_file_name, X_OK);

  /* Here it should be safe to use the system search path since we should have
     already qualified the name of the compiler when it is needed.  */
  if (p == 0)
    p = find_a_file (&path, c_file_name, X_OK);

  if (p)
    c_file_name = p;

  *ld1++ = *ld2++ = ld_file_name;

  /* Make temp file names.  */
  c_file = make_temp_file (".c");
  o_file = make_temp_file (".o");
#ifdef COLLECT_EXPORT_LIST
  export_file = make_temp_file (".x");
#endif
  if (!debug)
    {
      ldout = make_temp_file (".ld");
      lderrout = make_temp_file (".le");
    }
  *c_ptr++ = c_file_name;
  *c_ptr++ = "-x";
  *c_ptr++ = "c";
  *c_ptr++ = "-c";
  *c_ptr++ = "-o";
  *c_ptr++ = o_file;

#ifdef COLLECT_EXPORT_LIST
  /* Generate a list of directories from LIBPATH.  */
  prefix_from_env ("LIBPATH", &libpath_lib_dirs);
  /* Add to this list also two standard directories where
     AIX loader always searches for libraries.  */
  add_prefix (&libpath_lib_dirs, "/lib");
  add_prefix (&libpath_lib_dirs, "/usr/lib");
#endif

  /* Get any options that the upper GCC wants to pass to the sub-GCC.

     AIX support needs to know if -shared has been specified before
     parsing commandline arguments.  */

  p = getenv ("COLLECT_GCC_OPTIONS");
  while (p && *p)
    {
      const char *q = extract_string (&p);
      if (*q == '-' && (q[1] == 'm' || q[1] == 'f'))
	*c_ptr++ = xstrdup (q);
      if (strcmp (q, "-EL") == 0 || strcmp (q, "-EB") == 0)
	*c_ptr++ = xstrdup (q);
      if (strcmp (q, "-shared") == 0)
	shared_obj = 1;
      if (*q == '-' && q[1] == 'B')
	{
	  *c_ptr++ = xstrdup (q);
	  if (q[2] == 0)
	    {
	      q = extract_string (&p);
	      *c_ptr++ = xstrdup (q);
	    }
	}
    }
  obstack_free (&temporary_obstack, temporary_firstobj);
  *c_ptr++ = "-fno-profile-arcs";
  *c_ptr++ = "-fno-test-coverage";
  *c_ptr++ = "-fno-branch-probabilities";
  *c_ptr++ = "-fno-exceptions";
  *c_ptr++ = "-w";
  *c_ptr++ = "-fno-whole-program";

  /* !!! When GCC calls collect2,
     it does not know whether it is calling collect2 or ld.
     So collect2 cannot meaningfully understand any options
     except those ld understands.
     If you propose to make GCC pass some other option,
     just imagine what will happen if ld is really ld!!!  */

  /* Parse arguments.  Remember output file spec, pass the rest to ld.  */
  /* After the first file, put in the c++ rt0.  */

  first_file = 1;
  while ((arg = *++argv) != (char *) 0)
    {
      *ld1++ = *ld2++ = arg;

      if (arg[0] == '-')
	{
	  switch (arg[1])
	    {
	    case 'd':
	      if (!strcmp (arg, "-debug"))
		{
		  /* Already parsed.  */
		  ld1--;
		  ld2--;
		}
	      if (!strcmp (arg, "-dynamic-linker") && argv[1])
		{
		  ++argv;
		  *ld1++ = *ld2++ = *argv;
		}
	      break;

            case 'f':
	      if (strncmp (arg, "-flto", 5) == 0)
		{
#ifdef ENABLE_LTO
		  /* Do not pass LTO flag to the linker. */
		  ld1--;
		  ld2--;
#else
		  error ("LTO support has not been enabled in this "
			 "configuration");
#endif
		}
	      else if (!use_collect_ld
		       && strncmp (arg, "-fuse-ld=", 9) == 0)
		{
		  /* Do not pass -fuse-ld={bfd|gold} to the linker. */
		  ld1--;
		  ld2--;
		}
#ifdef TARGET_AIX_VERSION
	      else
		{
		  /* File containing a list of input files to process.  */

		  FILE *stream;
                  char buf[MAXPATHLEN + 2];
		  /* Number of additionnal object files.  */
		  int add_nbr = 0;
		  /* Maximum of additionnal object files before vector
		     expansion.  */
		  int add_max = 0;
		  const char *list_filename = arg + 2;

		  /* Accept -fFILENAME and -f FILENAME.  */
		  if (*list_filename == '\0' && argv[1])
		    {
		      ++argv;
		      list_filename = *argv;
		      *ld1++ = *ld2++ = *argv;
		    }

		  stream = fopen (list_filename, "r");
		  if (stream == NULL)
		    fatal_error ("can't open %s: %m", list_filename);

		  while (fgets (buf, sizeof buf, stream) != NULL)
		    {
		      /* Remove end of line.  */
		      int len = strlen (buf);
		      if (len >= 1 && buf[len - 1] =='\n')
			buf[len - 1] = '\0';

		      /* Put on object vector.
			 Note: we only expanse vector here, so we must keep
			 extra space for remaining arguments.  */
		      if (add_nbr >= add_max)
			{
			  int pos =
			    object - CONST_CAST2 (const char **, char **,
						  object_lst);
			  add_max = (add_max == 0) ? 16 : add_max * 2;
			  object_lst = XRESIZEVEC (char *, object_lst,
                                                   object_nbr + add_max);
			  object = CONST_CAST2 (const char **, char **,
						object_lst) + pos;
			  object_nbr += add_max;
			}
		      *object++ = xstrdup (buf);
		      add_nbr++;
		    }
		  fclose (stream);
		}
#endif
              break;

	    case 'l':
	      if (first_file)
		{
		  /* place o_file BEFORE this argument! */
		  first_file = 0;
		  ld2--;
		  *ld2++ = o_file;
		  *ld2++ = arg;
		}
#ifdef COLLECT_EXPORT_LIST
	      {
		/* Resolving full library name.  */
		const char *s = resolve_lib_name (arg+2);

		/* Saving a full library name.  */
		add_to_list (&libs, s);
	      }
#endif
	      break;

#ifdef COLLECT_EXPORT_LIST
	    /* Saving directories where to search for libraries.  */
	    case 'L':
	      add_prefix (&cmdline_lib_dirs, arg+2);
	      break;
#endif

	    case 'o':
	      if (arg[2] == '\0')
		output_file = *ld1++ = *ld2++ = *++argv;
	      else
		output_file = &arg[2];
	      break;

	    case 'r':
	      if (arg[2] == '\0')
		rflag = 1;
	      break;

	    case 's':
	      if (arg[2] == '\0' && do_collecting)
		{
		  /* We must strip after the nm run, otherwise C++ linking
		     will not work.  Thus we strip in the second ld run, or
		     else with strip if there is no second ld run.  */
		  strip_flag = 1;
		  ld1--;
		}
	      break;

	    case 'v':
	      if (arg[2] == '\0')
		vflag = true;
	      break;

	    case '-':
	      if (strcmp (arg, "--no-demangle") == 0)
		{
#ifndef HAVE_LD_DEMANGLE
		  no_demangle = 1;
		  ld1--;
		  ld2--;
#endif
		}
	      else if (strncmp (arg, "--demangle", 10) == 0)
		{
#ifndef HAVE_LD_DEMANGLE
		  no_demangle = 0;
		  if (arg[10] == '=')
		    {
		      enum demangling_styles style
			= cplus_demangle_name_to_style (arg+11);
		      if (style == unknown_demangling)
			error ("unknown demangling style '%s'", arg+11);
		      else
			current_demangling_style = style;
		    }
		  ld1--;
		  ld2--;
#endif
		}
	      else if (strncmp (arg, "--sysroot=", 10) == 0)
		target_system_root = arg + 10;
	      else if (strcmp (arg, "--version") == 0)
		vflag = true;
	      else if (strcmp (arg, "--help") == 0)
		helpflag = true;
	      break;
	    }
	}
      else if ((p = strrchr (arg, '.')) != (char *) 0
	       && (strcmp (p, ".o") == 0 || strcmp (p, ".a") == 0
		   || strcmp (p, ".so") == 0 || strcmp (p, ".lo") == 0
		   || strcmp (p, ".obj") == 0))
	{
	  if (first_file)
	    {
	      first_file = 0;
	      if (p[1] == 'o')
		*ld2++ = o_file;
	      else
		{
		  /* place o_file BEFORE this argument! */
		  ld2--;
		  *ld2++ = o_file;
		  *ld2++ = arg;
		}
	    }
	  if (p[1] == 'o' || p[1] == 'l')
	    *object++ = arg;
#ifdef COLLECT_EXPORT_LIST
	  /* libraries can be specified directly, i.e. without -l flag.  */
	  else
	    {
	      /* Saving a full library name.  */
	      add_to_list (&libs, arg);
	    }
#endif
	}
    }

#ifdef COLLECT_EXPORT_LIST
  /* This is added only for debugging purposes.  */
  if (debug)
    {
      fprintf (stderr, "List of libraries:\n");
      dump_list (stderr, "\t", libs.first);
    }

  /* The AIX linker will discard static constructors in object files if
     nothing else in the file is referenced, so look at them first.  Unless
     we are building a shared object, ignore the eh frame tables, as we
     would otherwise reference them all, hence drag all the corresponding
     objects even if nothing else is referenced.  */
  {
    const char **export_object_lst
      = CONST_CAST2 (const char **, char **, object_lst);

    struct id *list = libs.first;

    /* Compute the filter to use from the current one, do scan, then adjust
       the "current" filter to remove what we just included here.  This will
       control whether we need a first pass link later on or not, and what
       will remain to be scanned there.  */

    scanfilter this_filter = ld1_filter;
#if HAVE_AS_REF
    if (!shared_obj)
      this_filter &= ~SCAN_DWEH;
#endif

    while (export_object_lst < object)
      scan_prog_file (*export_object_lst++, PASS_OBJ, this_filter);

    for (; list; list = list->next)
      scan_prog_file (list->name, PASS_FIRST, this_filter);

    ld1_filter = ld1_filter & ~this_filter;
  }

  if (exports.first)
    {
      char *buf = concat ("-bE:", export_file, NULL);

      *ld1++ = buf;
      *ld2++ = buf;

      exportf = fopen (export_file, "w");
      if (exportf == (FILE *) 0)
	fatal_error ("fopen %s: %m", export_file);
      write_aix_file (exportf, exports.first);
      if (fclose (exportf))
	fatal_error ("fclose %s: %m", export_file);
    }
#endif

  *c_ptr++ = c_file;
  *c_ptr = *ld1 = *object = (char *) 0;

  if (vflag)
    notice ("collect2 version %s\n", version_string);

  if (helpflag)
    {
      printf ("Usage: collect2 [options]\n");
      printf (" Wrap linker and generate constructor code if needed.\n");
      printf (" Options:\n");
      printf ("  -debug          Enable debug output\n");
      printf ("  --help          Display this information\n");
      printf ("  -v, --version   Display this program's version number\n");
      printf ("\n");
      printf ("Overview: http://gcc.gnu.org/onlinedocs/gccint/Collect2.html\n");
      printf ("Report bugs: %s\n", bug_report_url);
      printf ("\n");
    }

  if (debug)
    {
      const char *ptr;
      fprintf (stderr, "ld_file_name        = %s\n",
	       (ld_file_name ? ld_file_name : "not found"));
      fprintf (stderr, "c_file_name         = %s\n",
	       (c_file_name ? c_file_name : "not found"));
      fprintf (stderr, "nm_file_name        = %s\n",
	       (nm_file_name ? nm_file_name : "not found"));
#ifdef LDD_SUFFIX
      fprintf (stderr, "ldd_file_name       = %s\n",
	       (ldd_file_name ? ldd_file_name : "not found"));
#endif
      fprintf (stderr, "strip_file_name     = %s\n",
	       (strip_file_name ? strip_file_name : "not found"));
      fprintf (stderr, "c_file              = %s\n",
	       (c_file ? c_file : "not found"));
      fprintf (stderr, "o_file              = %s\n",
	       (o_file ? o_file : "not found"));

      ptr = getenv ("COLLECT_GCC_OPTIONS");
      if (ptr)
	fprintf (stderr, "COLLECT_GCC_OPTIONS = %s\n", ptr);

      ptr = getenv ("COLLECT_GCC");
      if (ptr)
	fprintf (stderr, "COLLECT_GCC         = %s\n", ptr);

      ptr = getenv ("COMPILER_PATH");
      if (ptr)
	fprintf (stderr, "COMPILER_PATH       = %s\n", ptr);

      ptr = getenv (LIBRARY_PATH_ENV);
      if (ptr)
	fprintf (stderr, "%-20s= %s\n", LIBRARY_PATH_ENV, ptr);

      fprintf (stderr, "\n");
    }

  /* Load the program, searching all libraries and attempting to provide
     undefined symbols from repository information.

     If -r or they will be run via some other method, do not build the
     constructor or destructor list, just return now.  */
  {
    bool early_exit
      = rflag || (! DO_COLLECT_EXPORT_LIST && ! do_collecting);

    /* Perform the first pass link now, if we're about to exit or if we need
       to scan for things we haven't collected yet before pursuing further.

       On AIX, the latter typically includes nothing for shared objects or
       frame tables for an executable, out of what the required early scan on
       objects and libraries has performed above.  In the !shared_obj case, we
       expect the relevant tables to be dragged together with their associated
       functions from precise cross reference insertions by the compiler.  */

    if (early_exit || ld1_filter != SCAN_NOTHING)
      do_tlink (ld1_argv, object_lst);

    if (early_exit)
      {
#ifdef COLLECT_EXPORT_LIST
	/* Make sure we delete the export file we may have created.  */
	if (export_file != 0 && export_file[0])
	  maybe_unlink (export_file);
#endif
	if (lto_mode != LTO_MODE_NONE)
	  maybe_run_lto_and_relink (ld1_argv, object_lst, object, false);
	else
	  post_ld_pass (false);

	maybe_unlink (c_file);
	maybe_unlink (o_file);
	return 0;
      }
  }

  /* Unless we have done it all already, examine the namelist and search for
     static constructors and destructors to call.  Write the constructor and
     destructor tables to a .s file and reload.  */

  if (ld1_filter != SCAN_NOTHING)
    scan_prog_file (output_file, PASS_FIRST, ld1_filter);

#ifdef SCAN_LIBRARIES
  scan_libraries (output_file);
#endif

  if (debug)
    {
      notice_translated (ngettext ("%d constructor found\n",
                                   "%d constructors found\n",
                                   constructors.number),
                         constructors.number);
      notice_translated (ngettext ("%d destructor found\n",
                                   "%d destructors found\n",
                                   destructors.number),
                         destructors.number);
      notice_translated (ngettext("%d frame table found\n",
                                  "%d frame tables found\n",
                                  frame_tables.number),
                         frame_tables.number);
    }

  /* If the scan exposed nothing of special interest, there's no need to
     generate the glue code and relink so return now.  */

  if (constructors.number == 0 && destructors.number == 0
      && frame_tables.number == 0
#if defined (SCAN_LIBRARIES) || defined (COLLECT_EXPORT_LIST)
      /* If we will be running these functions ourselves, we want to emit
	 stubs into the shared library so that we do not have to relink
	 dependent programs when we add static objects.  */
      && ! shared_obj
#endif
      )
    {
      /* Do tlink without additional code generation now if we didn't
	 do it earlier for scanning purposes.  */
      if (ld1_filter == SCAN_NOTHING)
	do_tlink (ld1_argv, object_lst);

      if (lto_mode)
        maybe_run_lto_and_relink (ld1_argv, object_lst, object, false);

      /* Strip now if it was requested on the command line.  */
      if (strip_flag)
	{
	  char **real_strip_argv = XCNEWVEC (char *, 3);
	  const char ** strip_argv = CONST_CAST2 (const char **, char **,
						  real_strip_argv);

	  strip_argv[0] = strip_file_name;
	  strip_argv[1] = output_file;
	  strip_argv[2] = (char *) 0;
	  fork_execute ("strip", real_strip_argv);
	}

#ifdef COLLECT_EXPORT_LIST
      maybe_unlink (export_file);
#endif
      post_ld_pass (false);

      maybe_unlink (c_file);
      maybe_unlink (o_file);
      return 0;
    }

  /* Sort ctor and dtor lists by priority.  */
  sort_ids (&constructors);
  sort_ids (&destructors);

  maybe_unlink(output_file);
  outf = fopen (c_file, "w");
  if (outf == (FILE *) 0)
    fatal_error ("fopen %s: %m", c_file);

  write_c_file (outf, c_file);

  if (fclose (outf))
    fatal_error ("fclose %s: %m", c_file);

  /* Tell the linker that we have initializer and finalizer functions.  */
#ifdef LD_INIT_SWITCH
#ifdef COLLECT_EXPORT_LIST
  *ld2++ = concat (LD_INIT_SWITCH, ":", initname, ":", fininame, NULL);
#else
  *ld2++ = LD_INIT_SWITCH;
  *ld2++ = initname;
  *ld2++ = LD_FINI_SWITCH;
  *ld2++ = fininame;
#endif
#endif

#ifdef COLLECT_EXPORT_LIST
  if (shared_obj)
    {
      /* If we did not add export flag to link arguments before, add it to
	 second link phase now.  No new exports should have been added.  */
      if (! exports.first)
	*ld2++ = concat ("-bE:", export_file, NULL);

#ifndef LD_INIT_SWITCH
      add_to_list (&exports, initname);
      add_to_list (&exports, fininame);
      add_to_list (&exports, "_GLOBAL__DI");
      add_to_list (&exports, "_GLOBAL__DD");
#endif
      exportf = fopen (export_file, "w");
      if (exportf == (FILE *) 0)
	fatal_error ("fopen %s: %m", export_file);
      write_aix_file (exportf, exports.first);
      if (fclose (exportf))
	fatal_error ("fclose %s: %m", export_file);
    }
#endif

  /* End of arguments to second link phase.  */
  *ld2 = (char*) 0;

  if (debug)
    {
      fprintf (stderr, "\n========== output_file = %s, c_file = %s\n",
	       output_file, c_file);
      write_c_file (stderr, "stderr");
      fprintf (stderr, "========== end of c_file\n\n");
#ifdef COLLECT_EXPORT_LIST
      fprintf (stderr, "\n========== export_file = %s\n", export_file);
      write_aix_file (stderr, exports.first);
      fprintf (stderr, "========== end of export_file\n\n");
#endif
    }

  /* Assemble the constructor and destructor tables.
     Link the tables in with the rest of the program.  */

  fork_execute ("gcc",  c_argv);
#ifdef COLLECT_EXPORT_LIST
  /* On AIX we must call tlink because of possible templates resolution.  */
  do_tlink (ld2_argv, object_lst);

  if (lto_mode)
    maybe_run_lto_and_relink (ld2_argv, object_lst, object, false);
#else
  /* Otherwise, simply call ld because tlink is already done.  */
  if (lto_mode)
    maybe_run_lto_and_relink (ld2_argv, object_lst, object, true);
  else
    {
      fork_execute ("ld", ld2_argv);
      post_ld_pass (false);
    }

  /* Let scan_prog_file do any final mods (OSF/rose needs this for
     constructors/destructors in shared libraries.  */
  scan_prog_file (output_file, PASS_SECOND, SCAN_ALL);
#endif

  maybe_unlink (c_file);
  maybe_unlink (o_file);

#ifdef COLLECT_EXPORT_LIST
  maybe_unlink (export_file);
#endif

  return 0;
}


/* Wait for a process to finish, and exit if a nonzero status is found.  */

int
collect_wait (const char *prog, struct pex_obj *pex)
{
  int status;

  if (!pex_get_status (pex, 1, &status))
    fatal_error ("can't get program status: %m");
  pex_free (pex);

  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  error ("%s terminated with signal %d [%s]%s",
		 prog, sig, strsignal(sig),
		 WCOREDUMP(status) ? ", core dumped" : "");
	  exit (FATAL_EXIT_CODE);
	}

      if (WIFEXITED (status))
	return WEXITSTATUS (status);
    }
  return 0;
}

static void
do_wait (const char *prog, struct pex_obj *pex)
{
  int ret = collect_wait (prog, pex);
  if (ret != 0)
    {
      error ("%s returned %d exit status", prog, ret);
      exit (ret);
    }

  if (response_file)
    {
      unlink (response_file);
      response_file = NULL;
    }
}


/* Execute a program, and wait for the reply.  */

struct pex_obj *
collect_execute (const char *prog, char **argv, const char *outname,
		 const char *errname, int flags)
{
  struct pex_obj *pex;
  const char *errmsg;
  int err;
  char *response_arg = NULL;
  char *response_argv[3] ATTRIBUTE_UNUSED;

  if (HAVE_GNU_LD && at_file_supplied && argv[0] != NULL)
    {
      /* If using @file arguments, create a temporary file and put the
         contents of argv into it.  Then change argv to an array corresponding
         to a single argument @FILE, where FILE is the temporary filename.  */

      char **current_argv = argv + 1;
      char *argv0 = argv[0];
      int status;
      FILE *f;

      /* Note: we assume argv contains at least one element; this is
         checked above.  */

      response_file = make_temp_file ("");

      f = fopen (response_file, "w");

      if (f == NULL)
        fatal_error ("could not open response file %s", response_file);

      status = writeargv (current_argv, f);

      if (status)
        fatal_error ("could not write to response file %s", response_file);

      status = fclose (f);

      if (EOF == status)
        fatal_error ("could not close response file %s", response_file);

      response_arg = concat ("@", response_file, NULL);
      response_argv[0] = argv0;
      response_argv[1] = response_arg;
      response_argv[2] = NULL;

      argv = response_argv;
    }

  if (vflag || debug)
    {
      char **p_argv;
      const char *str;

      if (argv[0])
	fprintf (stderr, "%s", argv[0]);
      else
	notice ("[cannot find %s]", prog);

      for (p_argv = &argv[1]; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  /* If we cannot find a program we need, complain error.  Do this here
     since we might not end up needing something that we could not find.  */

  if (argv[0] == 0)
    fatal_error ("cannot find '%s'", prog);

  pex = pex_init (0, "collect2", NULL);
  if (pex == NULL)
    fatal_error ("pex_init failed: %m");

  errmsg = pex_run (pex, flags, argv[0], argv, outname,
		    errname, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_error ("%s: %m", _(errmsg));
	}
      else
	fatal_error (errmsg);
    }

  free (response_arg);

  return pex;
}

static void
fork_execute (const char *prog, char **argv)
{
  struct pex_obj *pex;

  pex = collect_execute (prog, argv, NULL, NULL, PEX_LAST | PEX_SEARCH);
  do_wait (prog, pex);
}

/* Unlink FILE unless we are debugging or this is the output_file
   and we may not unlink it.  */

static void
maybe_unlink (const char *file)
{
  if (debug)
    {
      notice ("[Leaving %s]\n", file);
      return;
    }

  if (file == output_file && !may_unlink_output_file)
    return;

  unlink_if_ordinary (file);
}

/* Call maybe_unlink on the NULL-terminated list, FILE_LIST.  */

static void
maybe_unlink_list (char **file_list)
{
  char **tmp = file_list;

  while (*tmp)
    maybe_unlink (*(tmp++));
}


static long sequence_number = 0;

/* Add a name to a linked list.  */

static void
add_to_list (struct head *head_ptr, const char *name)
{
  struct id *newid
    = (struct id *) xcalloc (sizeof (struct id) + strlen (name), 1);
  struct id *p;
  strcpy (newid->name, name);

  if (head_ptr->first)
    head_ptr->last->next = newid;
  else
    head_ptr->first = newid;

  /* Check for duplicate symbols.  */
  for (p = head_ptr->first;
       strcmp (name, p->name) != 0;
       p = p->next)
    ;
  if (p != newid)
    {
      head_ptr->last->next = 0;
      free (newid);
      return;
    }

  newid->sequence = ++sequence_number;
  head_ptr->last = newid;
  head_ptr->number++;
}

/* Grab the init priority number from an init function name that
   looks like "_GLOBAL_.I.12345.foo".  */

static int
extract_init_priority (const char *name)
{
  int pos = 0, pri;

  while (name[pos] == '_')
    ++pos;
  pos += 10; /* strlen ("GLOBAL__X_") */

  /* Extract init_p number from ctor/dtor name.  */
  pri = atoi (name + pos);
  return pri ? pri : DEFAULT_INIT_PRIORITY;
}

/* Insertion sort the ids from ctor/dtor list HEAD_PTR in descending order.
   ctors will be run from right to left, dtors from left to right.  */

static void
sort_ids (struct head *head_ptr)
{
  /* id holds the current element to insert.  id_next holds the next
     element to insert.  id_ptr iterates through the already sorted elements
     looking for the place to insert id.  */
  struct id *id, *id_next, **id_ptr;

  id = head_ptr->first;

  /* We don't have any sorted elements yet.  */
  head_ptr->first = NULL;

  for (; id; id = id_next)
    {
      id_next = id->next;
      id->sequence = extract_init_priority (id->name);

      for (id_ptr = &(head_ptr->first); ; id_ptr = &((*id_ptr)->next))
	if (*id_ptr == NULL
	    /* If the sequence numbers are the same, we put the id from the
	       file later on the command line later in the list.  */
	    || id->sequence > (*id_ptr)->sequence
	    /* Hack: do lexical compare, too.
	    || (id->sequence == (*id_ptr)->sequence
		&& strcmp (id->name, (*id_ptr)->name) > 0) */
	    )
	  {
	    id->next = *id_ptr;
	    *id_ptr = id;
	    break;
	  }
    }

  /* Now set the sequence numbers properly so write_c_file works.  */
  for (id = head_ptr->first; id; id = id->next)
    id->sequence = ++sequence_number;
}

/* Write: `prefix', the names on list LIST, `suffix'.  */

static void
write_list (FILE *stream, const char *prefix, struct id *list)
{
  while (list)
    {
      fprintf (stream, "%sx%d,\n", prefix, list->sequence);
      list = list->next;
    }
}

#ifdef COLLECT_EXPORT_LIST
/* This function is really used only on AIX, but may be useful.  */
#if 0
static int
is_in_list (const char *prefix, struct id *list)
{
  while (list)
    {
      if (!strcmp (prefix, list->name)) return 1;
      list = list->next;
    }
    return 0;
}
#endif
#endif /* COLLECT_EXPORT_LIST */

/* Added for debugging purpose.  */
#ifdef COLLECT_EXPORT_LIST
static void
dump_list (FILE *stream, const char *prefix, struct id *list)
{
  while (list)
    {
      fprintf (stream, "%s%s,\n", prefix, list->name);
      list = list->next;
    }
}
#endif

#if 0
static void
dump_prefix_list (FILE *stream, const char *prefix, struct prefix_list *list)
{
  while (list)
    {
      fprintf (stream, "%s%s,\n", prefix, list->prefix);
      list = list->next;
    }
}
#endif

static void
write_list_with_asm (FILE *stream, const char *prefix, struct id *list)
{
  while (list)
    {
      fprintf (stream, "%sx%d __asm__ (\"%s\");\n",
	       prefix, list->sequence, list->name);
      list = list->next;
    }
}

/* Write out the constructor and destructor tables statically (for a shared
   object), along with the functions to execute them.  */

static void
write_c_file_stat (FILE *stream, const char *name ATTRIBUTE_UNUSED)
{
  const char *p, *q;
  char *prefix, *r;
  int frames = (frame_tables.number > 0);

  /* Figure out name of output_file, stripping off .so version.  */
  q = p = lbasename (output_file);

  while (q)
    {
      q = strchr (q,'.');
      if (q == 0)
	{
	  q = p + strlen (p);
	  break;
	}
      else
	{
	  if (filename_ncmp (q, SHLIB_SUFFIX, strlen (SHLIB_SUFFIX)) == 0)
	    {
	      q += strlen (SHLIB_SUFFIX);
	      break;
	    }
	  else
	    q++;
	}
    }
  /* q points to null at end of the string (or . of the .so version) */
  prefix = XNEWVEC (char, q - p + 1);
  strncpy (prefix, p, q - p);
  prefix[q - p] = 0;
  for (r = prefix; *r; r++)
    if (!ISALNUM ((unsigned char)*r))
      *r = '_';
  if (debug)
    notice ("\nwrite_c_file - output name is %s, prefix is %s\n",
	    output_file, prefix);

  initname = concat ("_GLOBAL__FI_", prefix, NULL);
  fininame = concat ("_GLOBAL__FD_", prefix, NULL);

  free (prefix);

  /* Write the tables as C code.  */

  fprintf (stream, "static int count;\n");
  fprintf (stream, "typedef void entry_pt();\n");
  write_list_with_asm (stream, "extern entry_pt ", constructors.first);

  if (frames)
    {
      write_list_with_asm (stream, "extern void *", frame_tables.first);

      fprintf (stream, "\tstatic void *frame_table[] = {\n");
      write_list (stream, "\t\t&", frame_tables.first);
      fprintf (stream, "\t0\n};\n");

      /* This must match what's in frame.h.  */
      fprintf (stream, "struct object {\n");
      fprintf (stream, "  void *pc_begin;\n");
      fprintf (stream, "  void *pc_end;\n");
      fprintf (stream, "  void *fde_begin;\n");
      fprintf (stream, "  void *fde_array;\n");
      fprintf (stream, "  __SIZE_TYPE__ count;\n");
      fprintf (stream, "  struct object *next;\n");
      fprintf (stream, "};\n");

      fprintf (stream, "extern void __register_frame_info_table (void *, struct object *);\n");
      fprintf (stream, "extern void *__deregister_frame_info (void *);\n");

      fprintf (stream, "static void reg_frame () {\n");
      fprintf (stream, "\tstatic struct object ob;\n");
      fprintf (stream, "\t__register_frame_info_table (frame_table, &ob);\n");
      fprintf (stream, "\t}\n");

      fprintf (stream, "static void dereg_frame () {\n");
      fprintf (stream, "\t__deregister_frame_info (frame_table);\n");
      fprintf (stream, "\t}\n");
    }

  fprintf (stream, "void %s() {\n", initname);
  if (constructors.number > 0 || frames)
    {
      fprintf (stream, "\tstatic entry_pt *ctors[] = {\n");
      write_list (stream, "\t\t", constructors.first);
      if (frames)
	fprintf (stream, "\treg_frame,\n");
      fprintf (stream, "\t};\n");
      fprintf (stream, "\tentry_pt **p;\n");
      fprintf (stream, "\tif (count++ != 0) return;\n");
      fprintf (stream, "\tp = ctors + %d;\n", constructors.number + frames);
      fprintf (stream, "\twhile (p > ctors) (*--p)();\n");
    }
  else
    fprintf (stream, "\t++count;\n");
  fprintf (stream, "}\n");
  write_list_with_asm (stream, "extern entry_pt ", destructors.first);
  fprintf (stream, "void %s() {\n", fininame);
  if (destructors.number > 0 || frames)
    {
      fprintf (stream, "\tstatic entry_pt *dtors[] = {\n");
      write_list (stream, "\t\t", destructors.first);
      if (frames)
	fprintf (stream, "\tdereg_frame,\n");
      fprintf (stream, "\t};\n");
      fprintf (stream, "\tentry_pt **p;\n");
      fprintf (stream, "\tif (--count != 0) return;\n");
      fprintf (stream, "\tp = dtors;\n");
      fprintf (stream, "\twhile (p < dtors + %d) (*p++)();\n",
	       destructors.number + frames);
    }
  fprintf (stream, "}\n");

  if (shared_obj)
    {
      COLLECT_SHARED_INIT_FUNC(stream, initname);
      COLLECT_SHARED_FINI_FUNC(stream, fininame);
    }
}

/* Write the constructor/destructor tables.  */

#ifndef LD_INIT_SWITCH
static void
write_c_file_glob (FILE *stream, const char *name ATTRIBUTE_UNUSED)
{
  /* Write the tables as C code.  */

  int frames = (frame_tables.number > 0);

  fprintf (stream, "typedef void entry_pt();\n\n");

  write_list_with_asm (stream, "extern entry_pt ", constructors.first);

  if (frames)
    {
      write_list_with_asm (stream, "extern void *", frame_tables.first);

      fprintf (stream, "\tstatic void *frame_table[] = {\n");
      write_list (stream, "\t\t&", frame_tables.first);
      fprintf (stream, "\t0\n};\n");

      /* This must match what's in frame.h.  */
      fprintf (stream, "struct object {\n");
      fprintf (stream, "  void *pc_begin;\n");
      fprintf (stream, "  void *pc_end;\n");
      fprintf (stream, "  void *fde_begin;\n");
      fprintf (stream, "  void *fde_array;\n");
      fprintf (stream, "  __SIZE_TYPE__ count;\n");
      fprintf (stream, "  struct object *next;\n");
      fprintf (stream, "};\n");

      fprintf (stream, "extern void __register_frame_info_table (void *, struct object *);\n");
      fprintf (stream, "extern void *__deregister_frame_info (void *);\n");

      fprintf (stream, "static void reg_frame () {\n");
      fprintf (stream, "\tstatic struct object ob;\n");
      fprintf (stream, "\t__register_frame_info_table (frame_table, &ob);\n");
      fprintf (stream, "\t}\n");

      fprintf (stream, "static void dereg_frame () {\n");
      fprintf (stream, "\t__deregister_frame_info (frame_table);\n");
      fprintf (stream, "\t}\n");
    }

  fprintf (stream, "\nentry_pt * __CTOR_LIST__[] = {\n");
  fprintf (stream, "\t(entry_pt *) %d,\n", constructors.number + frames);
  write_list (stream, "\t", constructors.first);
  if (frames)
    fprintf (stream, "\treg_frame,\n");
  fprintf (stream, "\t0\n};\n\n");

  write_list_with_asm (stream, "extern entry_pt ", destructors.first);

  fprintf (stream, "\nentry_pt * __DTOR_LIST__[] = {\n");
  fprintf (stream, "\t(entry_pt *) %d,\n", destructors.number + frames);
  write_list (stream, "\t", destructors.first);
  if (frames)
    fprintf (stream, "\tdereg_frame,\n");
  fprintf (stream, "\t0\n};\n\n");

  fprintf (stream, "extern entry_pt %s;\n", NAME__MAIN);
  fprintf (stream, "entry_pt *__main_reference = %s;\n\n", NAME__MAIN);
}
#endif /* ! LD_INIT_SWITCH */

static void
write_c_file (FILE *stream, const char *name)
{
#ifndef LD_INIT_SWITCH
  if (! shared_obj)
    write_c_file_glob (stream, name);
  else
#endif
    write_c_file_stat (stream, name);
}

#ifdef COLLECT_EXPORT_LIST
static void
write_aix_file (FILE *stream, struct id *list)
{
  for (; list; list = list->next)
    {
      fputs (list->name, stream);
      putc ('\n', stream);
    }
}
#endif

#ifdef OBJECT_FORMAT_NONE

/* Check to make sure the file is an LTO object file.  */

static bool
maybe_lto_object_file (const char *prog_name)
{
  FILE *f;
  unsigned char buf[4];
  int i;

  static unsigned char elfmagic[4] = { 0x7f, 'E', 'L', 'F' };
  static unsigned char coffmagic[2] = { 0x4c, 0x01 };
  static unsigned char coffmagic_x64[2] = { 0x64, 0x86 };
  static unsigned char machomagic[4][4] = {
    { 0xcf, 0xfa, 0xed, 0xfe },
    { 0xce, 0xfa, 0xed, 0xfe },
    { 0xfe, 0xed, 0xfa, 0xcf },
    { 0xfe, 0xed, 0xfa, 0xce }
  };

  f = fopen (prog_name, "rb");
  if (f == NULL)
    return false;
  if (fread (buf, sizeof (buf), 1, f) != 1)
    buf[0] = 0;
  fclose (f);

  if (memcmp (buf, elfmagic, sizeof (elfmagic)) == 0
      || memcmp (buf, coffmagic, sizeof (coffmagic)) == 0
      || memcmp (buf, coffmagic_x64, sizeof (coffmagic_x64)) == 0)
    return true;
  for (i = 0; i < 4; i++)
    if (memcmp (buf, machomagic[i], sizeof (machomagic[i])) == 0)
      return true;

  return false;
}

/* Generic version to scan the name list of the loaded program for
   the symbols g++ uses for static constructors and destructors.  */

static void
scan_prog_file (const char *prog_name, scanpass which_pass,
		scanfilter filter)
{
  void (*int_handler) (int);
#ifdef SIGQUIT
  void (*quit_handler) (int);
#endif
  char *real_nm_argv[4];
  const char **nm_argv = CONST_CAST2 (const char **, char**, real_nm_argv);
  int argc = 0;
  struct pex_obj *pex;
  const char *errmsg;
  int err;
  char *p, buf[1024];
  FILE *inf;
  int found_lto = 0;

  if (which_pass == PASS_SECOND)
    return;

  /* LTO objects must be in a known format.  This check prevents
     us from accepting an archive containing LTO objects, which
     gcc cannot currently handle.  */
  if (which_pass == PASS_LTOINFO && !maybe_lto_object_file (prog_name))
    return;

  /* If we do not have an `nm', complain.  */
  if (nm_file_name == 0)
    fatal_error ("cannot find 'nm'");

  nm_argv[argc++] = nm_file_name;
  if (NM_FLAGS[0] != '\0')
    nm_argv[argc++] = NM_FLAGS;

  nm_argv[argc++] = prog_name;
  nm_argv[argc++] = (char *) 0;

  /* Trace if needed.  */
  if (vflag)
    {
      const char **p_argv;
      const char *str;

      for (p_argv = &nm_argv[0]; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pex = pex_init (PEX_USE_PIPES, "collect2", NULL);
  if (pex == NULL)
    fatal_error ("pex_init failed: %m");

  errmsg = pex_run (pex, 0, nm_file_name, real_nm_argv, NULL, HOST_BIT_BUCKET,
		    &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_error ("%s: %m", _(errmsg));
	}
      else
	fatal_error (errmsg);
    }

  int_handler  = (void (*) (int)) signal (SIGINT,  SIG_IGN);
#ifdef SIGQUIT
  quit_handler = (void (*) (int)) signal (SIGQUIT, SIG_IGN);
#endif

  inf = pex_read_output (pex, 0);
  if (inf == NULL)
    fatal_error ("can't open nm output: %m");

  if (debug)
    {
      if (which_pass == PASS_LTOINFO)
        fprintf (stderr, "\nnm output with LTO info marker symbol.\n");
      else
        fprintf (stderr, "\nnm output with constructors/destructors.\n");
    }

  /* Read each line of nm output.  */
  while (fgets (buf, sizeof buf, inf) != (char *) 0)
    {
      int ch, ch2;
      char *name, *end;

      if (debug)
        fprintf (stderr, "\t%s\n", buf);

      if (which_pass == PASS_LTOINFO)
        {
          if (found_lto)
            continue;

          /* Look for the LTO info marker symbol, and add filename to
             the LTO objects list if found.  */
          for (p = buf; (ch = *p) != '\0' && ch != '\n'; p++)
            if (ch == ' '  && p[1] == '_' && p[2] == '_'
		&& (strncmp (p + (p[3] == '_' ? 2 : 1), "__gnu_lto_v1", 12) == 0)
		&& ISSPACE (p[p[3] == '_' ? 14 : 13]))
              {
                add_lto_object (&lto_objects, prog_name);

                /* We need to read all the input, so we can't just
                   return here.  But we can avoid useless work.  */
                found_lto = 1;

                break;
              }

	  continue;
        }

      /* If it contains a constructor or destructor name, add the name
	 to the appropriate list unless this is a kind of symbol we're
	 not supposed to even consider.  */

      for (p = buf; (ch = *p) != '\0' && ch != '\n' && ch != '_'; p++)
	if (ch == ' ' && p[1] == 'U' && p[2] == ' ')
	  break;

      if (ch != '_')
	continue;

      name = p;
      /* Find the end of the symbol name.
	 Do not include `|', because Encore nm can tack that on the end.  */
      for (end = p; (ch2 = *end) != '\0' && !ISSPACE (ch2) && ch2 != '|';
	   end++)
	continue;


      *end = '\0';
      switch (is_ctor_dtor (name))
	{
	case SYM_CTOR:
	  if (! (filter & SCAN_CTOR))
	    break;
	  if (which_pass != PASS_LIB)
	    add_to_list (&constructors, name);
	  break;

	case SYM_DTOR:
	  if (! (filter & SCAN_DTOR))
	    break;
	  if (which_pass != PASS_LIB)
	    add_to_list (&destructors, name);
	  break;

	case SYM_INIT:
	  if (! (filter & SCAN_INIT))
	    break;
	  if (which_pass != PASS_LIB)
	    fatal_error ("init function found in object %s", prog_name);
#ifndef LD_INIT_SWITCH
	  add_to_list (&constructors, name);
#endif
	  break;

	case SYM_FINI:
	  if (! (filter & SCAN_FINI))
	    break;
	  if (which_pass != PASS_LIB)
	    fatal_error ("fini function found in object %s", prog_name);
#ifndef LD_FINI_SWITCH
	  add_to_list (&destructors, name);
#endif
	  break;

	case SYM_DWEH:
	  if (! (filter & SCAN_DWEH))
	    break;
	  if (which_pass != PASS_LIB)
	    add_to_list (&frame_tables, name);
	  break;

	default:		/* not a constructor or destructor */
	  continue;
	}
    }

  if (debug)
    fprintf (stderr, "\n");

  do_wait (nm_file_name, pex);

  signal (SIGINT,  int_handler);
#ifdef SIGQUIT
  signal (SIGQUIT, quit_handler);
#endif
}

#ifdef LDD_SUFFIX

/* Use the List Dynamic Dependencies program to find shared libraries that
   the output file depends upon and their initialization/finalization
   routines, if any.  */

static void
scan_libraries (const char *prog_name)
{
  static struct head libraries;		/* list of shared libraries found */
  struct id *list;
  void (*int_handler) (int);
#ifdef SIGQUIT
  void (*quit_handler) (int);
#endif
  char *real_ldd_argv[4];
  const char **ldd_argv = CONST_CAST2 (const char **, char **, real_ldd_argv);
  int argc = 0;
  struct pex_obj *pex;
  const char *errmsg;
  int err;
  char buf[1024];
  FILE *inf;

  /* If we do not have an `ldd', complain.  */
  if (ldd_file_name == 0)
    {
      error ("cannot find 'ldd'");
      return;
    }

  ldd_argv[argc++] = ldd_file_name;
  ldd_argv[argc++] = prog_name;
  ldd_argv[argc++] = (char *) 0;

  /* Trace if needed.  */
  if (vflag)
    {
      const char **p_argv;
      const char *str;

      for (p_argv = &ldd_argv[0]; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pex = pex_init (PEX_USE_PIPES, "collect2", NULL);
  if (pex == NULL)
    fatal_error ("pex_init failed: %m");

  errmsg = pex_run (pex, 0, ldd_file_name, real_ldd_argv, NULL, NULL, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_error ("%s: %m", _(errmsg));
	}
      else
	fatal_error (errmsg);
    }

  int_handler  = (void (*) (int)) signal (SIGINT,  SIG_IGN);
#ifdef SIGQUIT
  quit_handler = (void (*) (int)) signal (SIGQUIT, SIG_IGN);
#endif

  inf = pex_read_output (pex, 0);
  if (inf == NULL)
    fatal_error ("can't open ldd output: %m");

  if (debug)
    notice ("\nldd output with constructors/destructors.\n");

  /* Read each line of ldd output.  */
  while (fgets (buf, sizeof buf, inf) != (char *) 0)
    {
      int ch2;
      char *name, *end, *p = buf;

      /* Extract names of libraries and add to list.  */
      PARSE_LDD_OUTPUT (p);
      if (p == 0)
	continue;

      name = p;
      if (strncmp (name, "not found", sizeof ("not found") - 1) == 0)
	fatal_error ("dynamic dependency %s not found", buf);

      /* Find the end of the symbol name.  */
      for (end = p;
	   (ch2 = *end) != '\0' && ch2 != '\n' && !ISSPACE (ch2) && ch2 != '|';
	   end++)
	continue;
      *end = '\0';

      if (access (name, R_OK) == 0)
	add_to_list (&libraries, name);
      else
	fatal_error ("unable to open dynamic dependency '%s'", buf);

      if (debug)
	fprintf (stderr, "\t%s\n", buf);
    }
  if (debug)
    fprintf (stderr, "\n");

  do_wait (ldd_file_name, pex);

  signal (SIGINT,  int_handler);
#ifdef SIGQUIT
  signal (SIGQUIT, quit_handler);
#endif

  /* Now iterate through the library list adding their symbols to
     the list.  */
  for (list = libraries.first; list; list = list->next)
    scan_prog_file (list->name, PASS_LIB, SCAN_ALL);
}

#endif /* LDD_SUFFIX */

#endif /* OBJECT_FORMAT_NONE */


/*
 * COFF specific stuff.
 */

#ifdef OBJECT_FORMAT_COFF

#if defined (EXTENDED_COFF)

#   define GCC_SYMBOLS(X)	(SYMHEADER(X).isymMax + SYMHEADER(X).iextMax)
#   define GCC_SYMENT		SYMR
#   define GCC_OK_SYMBOL(X)	((X).st == stProc || (X).st == stGlobal)
#   define GCC_SYMINC(X)	(1)
#   define GCC_SYMZERO(X)	(SYMHEADER(X).isymMax)
#   define GCC_CHECK_HDR(X)	(PSYMTAB(X) != 0)

#else

#   define GCC_SYMBOLS(X)	(HEADER(ldptr).f_nsyms)
#   define GCC_SYMENT		SYMENT
#   if defined (C_WEAKEXT)
#     define GCC_OK_SYMBOL(X) \
       (((X).n_sclass == C_EXT || (X).n_sclass == C_WEAKEXT) && \
	((X).n_scnum > N_UNDEF) && \
	(aix64_flag \
	 || (((X).n_type & N_TMASK) == (DT_NON << N_BTSHFT) \
	     || ((X).n_type & N_TMASK) == (DT_FCN << N_BTSHFT))))
#     define GCC_UNDEF_SYMBOL(X) \
       (((X).n_sclass == C_EXT || (X).n_sclass == C_WEAKEXT) && \
	((X).n_scnum == N_UNDEF))
#   else
#     define GCC_OK_SYMBOL(X) \
       (((X).n_sclass == C_EXT) && \
	((X).n_scnum > N_UNDEF) && \
	(aix64_flag \
	 || (((X).n_type & N_TMASK) == (DT_NON << N_BTSHFT) \
	     || ((X).n_type & N_TMASK) == (DT_FCN << N_BTSHFT))))
#     define GCC_UNDEF_SYMBOL(X) \
       (((X).n_sclass == C_EXT) && ((X).n_scnum == N_UNDEF))
#   endif
#   define GCC_SYMINC(X)	((X).n_numaux+1)
#   define GCC_SYMZERO(X)	0

/* 0757 = U803XTOCMAGIC (AIX 4.3) and 0767 = U64_TOCMAGIC (AIX V5) */
#if TARGET_AIX_VERSION >= 51
#   define GCC_CHECK_HDR(X) \
     (((HEADER (X).f_magic == U802TOCMAGIC && ! aix64_flag) \
       || (HEADER (X).f_magic == 0767 && aix64_flag)) \
      && !(HEADER (X).f_flags & F_LOADONLY))
#else
#   define GCC_CHECK_HDR(X) \
     (((HEADER (X).f_magic == U802TOCMAGIC && ! aix64_flag) \
       || (HEADER (X).f_magic == 0757 && aix64_flag)) \
      && !(HEADER (X).f_flags & F_LOADONLY))
#endif

#endif

#ifdef COLLECT_EXPORT_LIST
/* Array of standard AIX libraries which should not
   be scanned for ctors/dtors.  */
static const char *const aix_std_libs[] = {
  "/unix",
  "/lib/libc.a",
  "/lib/libm.a",
  "/lib/libc_r.a",
  "/lib/libm_r.a",
  "/usr/lib/libc.a",
  "/usr/lib/libm.a",
  "/usr/lib/libc_r.a",
  "/usr/lib/libm_r.a",
  "/usr/lib/threads/libc.a",
  "/usr/ccs/lib/libc.a",
  "/usr/ccs/lib/libm.a",
  "/usr/ccs/lib/libc_r.a",
  "/usr/ccs/lib/libm_r.a",
  NULL
};

/* This function checks the filename and returns 1
   if this name matches the location of a standard AIX library.  */
static int ignore_library (const char *);
static int
ignore_library (const char *name)
{
  const char *const *p;
  size_t length;

  if (target_system_root[0] != '\0')
    {
      length = strlen (target_system_root);
      if (strncmp (name, target_system_root, length) != 0)
	return 0;
      name += length;
    }
  for (p = &aix_std_libs[0]; *p != NULL; ++p)
    if (strcmp (name, *p) == 0)
      return 1;
  return 0;
}
#endif /* COLLECT_EXPORT_LIST */

#if defined (HAVE_DECL_LDGETNAME) && !HAVE_DECL_LDGETNAME
extern char *ldgetname (LDFILE *, GCC_SYMENT *);
#endif

/* COFF version to scan the name list of the loaded program for
   the symbols g++ uses for static constructors and destructors.  */

static void
scan_prog_file (const char *prog_name, scanpass which_pass,
		scanfilter filter)
{
  LDFILE *ldptr = NULL;
  int sym_index, sym_count;
  int is_shared = 0;

  if (which_pass != PASS_FIRST && which_pass != PASS_OBJ)
    return;

#ifdef COLLECT_EXPORT_LIST
  /* We do not need scanning for some standard C libraries.  */
  if (which_pass == PASS_FIRST && ignore_library (prog_name))
    return;

  /* On AIX we have a loop, because there is not much difference
     between an object and an archive. This trick allows us to
     eliminate scan_libraries() function.  */
  do
    {
#endif
      /* Some platforms (e.g. OSF4) declare ldopen as taking a
	 non-const char * filename parameter, even though it will not
	 modify that string.  So we must cast away const-ness here,
	 using CONST_CAST to prevent complaints from -Wcast-qual.  */
      if ((ldptr = ldopen (CONST_CAST (char *, prog_name), ldptr)) != NULL)
	{
	  if (! MY_ISCOFF (HEADER (ldptr).f_magic))
	    fatal_error ("%s: not a COFF file", prog_name);

	  if (GCC_CHECK_HDR (ldptr))
	    {
	      sym_count = GCC_SYMBOLS (ldptr);
	      sym_index = GCC_SYMZERO (ldptr);

#ifdef COLLECT_EXPORT_LIST
	      /* Is current archive member a shared object?  */
	      is_shared = HEADER (ldptr).f_flags & F_SHROBJ;
#endif

	      while (sym_index < sym_count)
		{
		  GCC_SYMENT symbol;

		  if (ldtbread (ldptr, sym_index, &symbol) <= 0)
		    break;
		  sym_index += GCC_SYMINC (symbol);

		  if (GCC_OK_SYMBOL (symbol))
		    {
		      char *name;

		      if ((name = ldgetname (ldptr, &symbol)) == NULL)
			continue;		/* Should never happen.  */

#ifdef XCOFF_DEBUGGING_INFO
		      /* All AIX function names have a duplicate entry
			 beginning with a dot.  */
		      if (*name == '.')
			++name;
#endif

		      switch (is_ctor_dtor (name))
			{
			case SYM_CTOR:
			  if (! (filter & SCAN_CTOR))
			    break;
			  if (! is_shared)
			    add_to_list (&constructors, name);
#if defined (COLLECT_EXPORT_LIST) && !defined (LD_INIT_SWITCH)
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

			case SYM_DTOR:
			  if (! (filter & SCAN_DTOR))
			    break;
			  if (! is_shared)
			    add_to_list (&destructors, name);
#if defined (COLLECT_EXPORT_LIST) && !defined (LD_INIT_SWITCH)
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

#ifdef COLLECT_EXPORT_LIST
			case SYM_INIT:
			  if (! (filter & SCAN_INIT))
			    break;
#ifndef LD_INIT_SWITCH
			  if (is_shared)
			    add_to_list (&constructors, name);
#endif
			  break;

			case SYM_FINI:
			  if (! (filter & SCAN_FINI))
			    break;
#ifndef LD_INIT_SWITCH
			  if (is_shared)
			    add_to_list (&destructors, name);
#endif
			  break;
#endif

			case SYM_DWEH:
			  if (! (filter & SCAN_DWEH))
			    break;
			  if (! is_shared)
			    add_to_list (&frame_tables, name);
#if defined (COLLECT_EXPORT_LIST) && !defined (LD_INIT_SWITCH)
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

			default:	/* not a constructor or destructor */
#ifdef COLLECT_EXPORT_LIST
			  /* Explicitly export all global symbols when
			     building a shared object on AIX, but do not
			     re-export symbols from another shared object
			     and do not export symbols if the user
			     provides an explicit export list.  */
			  if (shared_obj && !is_shared
			      && which_pass == PASS_OBJ && !export_flag)
			    add_to_list (&exports, name);
#endif
			  continue;
			}

		      if (debug)
#if !defined(EXTENDED_COFF)
			fprintf (stderr, "\tsec=%d class=%d type=%s%o %s\n",
				 symbol.n_scnum, symbol.n_sclass,
				 (symbol.n_type ? "0" : ""), symbol.n_type,
				 name);
#else
			fprintf (stderr,
				 "\tiss = %5d, value = %5ld, index = %5d, name = %s\n",
				 symbol.iss, (long) symbol.value, symbol.index, name);
#endif
		    }
		}
	    }
#ifdef COLLECT_EXPORT_LIST
	  else
	    {
	      /* If archive contains both 32-bit and 64-bit objects,
		 we want to skip objects in other mode so mismatch normal.  */
	      if (debug)
		fprintf (stderr, "%s : magic=%o aix64=%d mismatch\n",
			 prog_name, HEADER (ldptr).f_magic, aix64_flag);
	    }
#endif
	}
      else
	{
	  fatal_error ("%s: cannot open as COFF file", prog_name);
	}
#ifdef COLLECT_EXPORT_LIST
      /* On AIX loop continues while there are more members in archive.  */
    }
  while (ldclose (ldptr) == FAILURE);
#else
  /* Otherwise we simply close ldptr.  */
  (void) ldclose(ldptr);
#endif
}
#endif /* OBJECT_FORMAT_COFF */

#ifdef COLLECT_EXPORT_LIST
/* Given a library name without "lib" prefix, this function
   returns a full library name including a path.  */
static char *
resolve_lib_name (const char *name)
{
  char *lib_buf;
  int i, j, l = 0;
  /* Library extensions for AIX dynamic linking.  */
  const char * const libexts[2] = {"a", "so"};

  for (i = 0; libpaths[i]; i++)
    if (libpaths[i]->max_len > l)
      l = libpaths[i]->max_len;

  lib_buf = XNEWVEC (char, l + strlen(name) + 10);

  for (i = 0; libpaths[i]; i++)
    {
      struct prefix_list *list = libpaths[i]->plist;
      for (; list; list = list->next)
	{
	  /* The following lines are needed because path_prefix list
	     may contain directories both with trailing DIR_SEPARATOR and
	     without it.  */
	  const char *p = "";
	  if (!IS_DIR_SEPARATOR (list->prefix[strlen(list->prefix)-1]))
	    p = "/";
	  for (j = 0; j < 2; j++)
	    {
	      sprintf (lib_buf, "%s%slib%s.%s",
		       list->prefix, p, name,
		       libexts[(j + aixrtl_flag) % 2]);
	      if (debug) fprintf (stderr, "searching for: %s\n", lib_buf);
	      if (file_exists (lib_buf))
		{
		  if (debug) fprintf (stderr, "found: %s\n", lib_buf);
		  return (lib_buf);
		}
	    }
	}
    }
  if (debug)
    fprintf (stderr, "not found\n");
  else
    fatal_error ("library lib%s not found", name);
  return (NULL);
}
#endif /* COLLECT_EXPORT_LIST */

#ifdef COLLECT_RUN_DSYMUTIL
static int flag_dsym = false;
static int flag_idsym = false;

static void
process_args (int *argcp, char **argv) {
  int i, j;
  int argc = *argcp;
  for (i=0; i<argc; ++i)
    {
      if (strcmp (argv[i], "-dsym") == 0)
	{
	  flag_dsym = true;
	  /* Remove the flag, as we handle all processing for it.  */
	  j = i;
	  do
	    argv[j] = argv[j+1];
	  while (++j < argc);
	  --i;
	  argc = --(*argcp);
	}
      else if (strcmp (argv[i], "-idsym") == 0)
	{
	  flag_idsym = true;
	  /* Remove the flag, as we handle all processing for it.  */
	  j = i;
	  do
	    argv[j] = argv[j+1];
	  while (++j < argc);
	  --i;
	  argc = --(*argcp);
	}
    }
}

static void
do_dsymutil (const char *output_file) {
  const char *dsymutil = DSYMUTIL + 1;
  struct pex_obj *pex;
  char **real_argv = XCNEWVEC (char *, 3);
  const char ** argv = CONST_CAST2 (const char **, char **,
				    real_argv);

  argv[0] = dsymutil;
  argv[1] = output_file;
  argv[2] = (char *) 0;

  pex = collect_execute (dsymutil, real_argv, NULL, NULL, PEX_LAST | PEX_SEARCH);
  do_wait (dsymutil, pex);
}

static void
post_ld_pass (bool temp_file) {
  if (!(temp_file && flag_idsym) && !flag_dsym)
    return;
      
  do_dsymutil (output_file);
}
#else
static void
process_args (int *argcp ATTRIBUTE_UNUSED, char **argv ATTRIBUTE_UNUSED) { }
static void post_ld_pass (bool temp_file ATTRIBUTE_UNUSED) { }
#endif
