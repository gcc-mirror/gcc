/* Collect static initialization info into data structures that can be
   traversed by C++ initialization and finalization routines.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Chris Smith (csmith@convex.com).
   Heavily modified by Michael Meissner (meissner@cygnus.com),
   Per Bothner (bothner@cygnus.com), and John Gilmore (gnu@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* Build tables of static constructors and destructors and run ld.  */

#include "config.h"
#include "system.h"
#include <signal.h>
#if ! defined( SIGCHLD ) && defined( SIGCLD )
#  define SIGCHLD SIGCLD
#endif

#ifdef vfork /* Autoconf may define this to fork for us.  */
# define VFORK_STRING "fork"
#else
# define VFORK_STRING "vfork"
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
#ifdef VMS
#define vfork() (decc$$alloc_vfork_blocks() >= 0 ? \
               lib$get_current_invo_context(decc$$get_vfork_jmpbuf()) : -1)
#endif /* VMS */

#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

#define COLLECT

#include "collect2.h"
#include "demangle.h"
#include "obstack.h"
#include "intl.h"
#include "version.h"

/* Obstack allocation and deallocation routines.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* On certain systems, we have code that works by scanning the object file
   directly.  But this code uses system-specific header files and library
   functions, so turn it off in a cross-compiler.  Likewise, the names of
   the utilities are not correct for a cross-compiler; we have to hope that
   cross-versions are in the proper directories.  */

#ifdef CROSS_COMPILE
#undef SUNOS4_SHARED_LIBRARIES
#undef OBJECT_FORMAT_COFF
#undef OBJECT_FORMAT_ROSE
#undef MD_EXEC_PREFIX
#undef REAL_LD_FILE_NAME
#undef REAL_NM_FILE_NAME
#undef REAL_STRIP_FILE_NAME
#endif

/* If we cannot use a special method, use the ordinary one:
   run nm to find what symbols are present.
   In a cross-compiler, this means you need a cross nm,
   but that is not quite as unpleasant as special headers.  */

#if !defined (OBJECT_FORMAT_COFF) && !defined (OBJECT_FORMAT_ROSE)
#define OBJECT_FORMAT_NONE
#endif

#ifdef OBJECT_FORMAT_COFF

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

/* Some systems have an ISCOFF macro, but others do not.  In some cases
   the macro may be wrong.  MY_ISCOFF is defined in tm.h files for machines
   that either do not have an ISCOFF macro in /usr/include or for those 
   where it is wrong.  */

#ifndef MY_ISCOFF
#define MY_ISCOFF(X) ISCOFF (X)
#endif

#endif /* OBJECT_FORMAT_COFF */

#ifdef OBJECT_FORMAT_ROSE

#ifdef _OSF_SOURCE
#define USE_MMAP
#endif

#ifdef USE_MMAP
#include <sys/mman.h>
#endif

#include <unistd.h>
#include <mach_o_format.h>
#include <mach_o_header.h>
#include <mach_o_vals.h>
#include <mach_o_types.h>

#endif /* OBJECT_FORMAT_ROSE */

#ifdef OBJECT_FORMAT_NONE

/* Default flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-n"
#endif

#endif /* OBJECT_FORMAT_NONE */

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#define SYMBOL__MAIN __main
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

#if defined (LDD_SUFFIX) || SUNOS4_SHARED_LIBRARIES
#define SCAN_LIBRARIES
#endif

#ifdef USE_COLLECT2
int do_collecting = 1;
#else
int do_collecting = 0;
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

/* Enumeration giving which pass this is for scanning the program file.  */

enum pass {
  PASS_FIRST,				/* without constructors */
  PASS_OBJ,				/* individual objects */
  PASS_LIB,			        /* looking for shared libraries */
  PASS_SECOND				/* with constructors linked in */
};

int vflag;				/* true if -v */
static int rflag;			/* true if -r */
static int strip_flag;			/* true if -s */
#ifdef COLLECT_EXPORT_LIST
static int export_flag;                 /* true if -bE */
static int aix64_flag;			/* true if -b64 */
#endif

int debug;				/* true if -debug */

static int shared_obj;		        /* true if -shared */

static const char *c_file;		/* <xxx>.c for constructor/destructor list.  */
static const char *o_file;		/* <xxx>.o for constructor/destructor list.  */
#ifdef COLLECT_EXPORT_LIST
static const char *export_file;	        /* <xxx>.x for AIX export list.  */
#endif
const char *ldout;			/* File for ld errors.  */
static const char *output_file;		/* Output file for ld.  */
static const char *nm_file_name;	/* pathname of nm */
#ifdef LDD_SUFFIX
static const char *ldd_file_name;	/* pathname of ldd (or equivalent) */
#endif
static const char *strip_file_name;		/* pathname of strip */
const char *c_file_name;	        /* pathname of gcc */
static char *initname, *fininame;	/* names of init and fini funcs */

static struct head constructors;	/* list of constructors found */
static struct head destructors;		/* list of destructors found */
#ifdef COLLECT_EXPORT_LIST
static struct head exports;		/* list of exported symbols */
#endif
static struct head frame_tables;	/* list of frame unwind info tables */

struct obstack temporary_obstack;
struct obstack permanent_obstack;
char * temporary_firstobj;

/* Holds the return value of pexecute.  */
int pexecute_pid;

/* Defined in the automatically-generated underscore.c.  */
extern int prepends_underscore;

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME)	do { (VAR) = getenv (NAME); } while (0)
#endif

/* Structure to hold all the directories in which to search for files to
   execute.  */

struct prefix_list
{
  const char *prefix;         /* String to prepend to the path.  */
  struct prefix_list *next;   /* Next in linked list.  */
};

struct path_prefix
{
  struct prefix_list *plist;  /* List of prefixes to try */
  int max_len;                /* Max length of a prefix in PLIST */
  const char *name;           /* Name of this list (used in config stuff) */
};

#ifdef COLLECT_EXPORT_LIST
/* Lists to keep libraries to be scanned for global constructors/destructors.  */
static struct head libs;                    /* list of libraries */
static struct path_prefix cmdline_lib_dirs; /* directories specified with -L */
static struct path_prefix libpath_lib_dirs; /* directories in LIBPATH */
static struct path_prefix *libpaths[3] = {&cmdline_lib_dirs,
					  &libpath_lib_dirs, NULL};
static const char *const libexts[3] = {"a", "so", NULL};  /* possible library extensions */
#endif

static void handler		PARAMS ((int));
static int is_ctor_dtor		PARAMS ((const char *));
static char *find_a_file	PARAMS ((struct path_prefix *, const char *));
static void add_prefix		PARAMS ((struct path_prefix *, const char *));
static void prefix_from_env	PARAMS ((const char *, struct path_prefix *));
static void prefix_from_string	PARAMS ((const char *, struct path_prefix *));
static void do_wait		PARAMS ((const char *));
static void fork_execute	PARAMS ((const char *, char **));
static void maybe_unlink	PARAMS ((const char *));
static void add_to_list		PARAMS ((struct head *, const char *));
static int extract_init_priority PARAMS ((const char *));
static void sort_ids		PARAMS ((struct head *));
static void write_list		PARAMS ((FILE *, const char *, struct id *));
#ifdef COLLECT_EXPORT_LIST
static void dump_list		PARAMS ((FILE *, const char *, struct id *));
#endif
#if 0
static void dump_prefix_list	PARAMS ((FILE *, const char *, struct prefix_list *));
#endif
static void write_list_with_asm PARAMS ((FILE *, const char *, struct id *));
static void write_c_file	PARAMS ((FILE *, const char *));
static void write_c_file_stat	PARAMS ((FILE *, const char *));
#ifndef LD_INIT_SWITCH
static void write_c_file_glob	PARAMS ((FILE *, const char *));
#endif
static void scan_prog_file	PARAMS ((const char *, enum pass));
#ifdef SCAN_LIBRARIES
static void scan_libraries	PARAMS ((const char *));
#endif
#if LINK_ELIMINATE_DUPLICATE_LDIRECTORIES
static int is_in_args		PARAMS ((const char *, const char **, const char **));
#endif
#ifdef COLLECT_EXPORT_LIST
#if 0
static int is_in_list		PARAMS ((const char *, struct id *));
#endif
static void write_aix_file	PARAMS ((FILE *, struct id *));
static char *resolve_lib_name	PARAMS ((const char *));
static int ignore_library	PARAMS ((const char *));
#endif
static char *extract_string	PARAMS ((const char **));

#ifndef HAVE_DUP2
static int dup2 PARAMS ((int, int));
static int
dup2 (oldfd, newfd)
     int oldfd;
     int newfd;
{
  int fdtmp[256];
  int fdx = 0;
  int fd;
 
  if (oldfd == newfd)
    return oldfd;
  close (newfd);
  while ((fd = dup (oldfd)) != newfd && fd >= 0) /* good enough for low fd's */
    fdtmp[fdx++] = fd;
  while (fdx > 0)
    close (fdtmp[--fdx]);

  return fd;
}
#endif /* ! HAVE_DUP2 */

/* Delete tempfiles and exit function.  */

void
collect_exit (status)
     int status;
{
  if (c_file != 0 && c_file[0])
    maybe_unlink (c_file);

  if (o_file != 0 && o_file[0])
    maybe_unlink (o_file);

#ifdef COLLECT_EXPORT_LIST
  if (export_file != 0 && export_file[0])
    maybe_unlink (export_file);
#endif

  if (ldout != 0 && ldout[0])
    {
      dump_file (ldout);
      maybe_unlink (ldout);
    }

  if (status != 0 && output_file != 0 && output_file[0])
    maybe_unlink (output_file);

  exit (status);
}


/* Notify user of a non-error.  */
void
notice VPARAMS ((const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  vfprintf (stderr, _(msgid), ap);
  VA_CLOSE (ap);
}

/* Die when sys call fails.  */

void
fatal_perror VPARAMS ((const char * msgid, ...))
{
  int e = errno;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  fprintf (stderr, "collect2: ");
  vfprintf (stderr, _(msgid), ap);
  fprintf (stderr, ": %s\n", xstrerror (e));
  VA_CLOSE (ap);

  collect_exit (FATAL_EXIT_CODE);
}

/* Just die.  */

void
fatal VPARAMS ((const char * msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);
  
  fprintf (stderr, "collect2: ");
  vfprintf (stderr, _(msgid), ap);
  fprintf (stderr, "\n");
  VA_CLOSE (ap);

  collect_exit (FATAL_EXIT_CODE);
}

/* Write error message.  */

void
error VPARAMS ((const char * msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  fprintf (stderr, "collect2: ");
  vfprintf (stderr, _(msgid), ap);
  fprintf (stderr, "\n");
  VA_CLOSE(ap);
}

/* In case obstack is linked in, and abort is defined to fancy_abort,
   provide a default entry.  */

void
fancy_abort ()
{
  fatal ("internal error");
}

static void
handler (signo)
     int signo;
{
  if (c_file != 0 && c_file[0])
    maybe_unlink (c_file);

  if (o_file != 0 && o_file[0])
    maybe_unlink (o_file);

  if (ldout != 0 && ldout[0])
    maybe_unlink (ldout);

#ifdef COLLECT_EXPORT_LIST
  if (export_file != 0 && export_file[0])
    maybe_unlink (export_file);
#endif

  signal (signo, SIG_DFL);
  kill (getpid (), signo);
}


int
file_exists (name)
     const char *name;
{
  return access (name, R_OK) == 0;
}

/* Parse a reasonable subset of shell quoting syntax.  */

static char *
extract_string (pp)
     const char **pp;
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
  return obstack_finish (&temporary_obstack);
}

void
dump_file (name)
     const char *name;
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
	  word = obstack_finish (&temporary_obstack);

	  if (*word == '.')
	    ++word, putc ('.', stderr);
	  p = word;
	  if (*p == '_' && prepends_underscore)
	    ++p;

	  if (no_demangle)
	    result = 0;
	  else
	    result = cplus_demangle (p, DMGL_PARAMS | DMGL_ANSI | DMGL_VERBOSE);

	  if (result)
	    {
	      int diff;
	      fputs (result, stderr);

	      diff = strlen (word) - strlen (result);
	      while (diff > 0 && c == ' ')
		--diff, putc (' ', stderr);
	      while (diff < 0 && c == ' ')
		++diff, c = getc (stream);

	      free (result);
	    }
	  else
	    fputs (word, stderr);

	  fflush (stderr);
	  obstack_free (&temporary_obstack, temporary_firstobj);
	}
      if (c == EOF)
	break;
      putc (c, stderr);
    }
  fclose (stream);
}

/* Decide whether the given symbol is: a constructor (1), a destructor
   (2), a routine in a shared object that calls all the constructors
   (3) or destructors (4), a DWARF exception-handling table (5), or
   nothing special (0).  */

static int
is_ctor_dtor (s)
     const char *s;
{
  struct names { const char *const name; const int len; const int ret;
    const int two_underscores; };

  const struct names *p;
  int ch;
  const char *orig_s = s;

  static const struct names special[] = {
#ifndef NO_DOLLAR_IN_LABEL
    { "GLOBAL__I$", sizeof ("GLOBAL__I$")-1, 1, 0 },
    { "GLOBAL__D$", sizeof ("GLOBAL__D$")-1, 2, 0 },
#else
#ifndef NO_DOT_IN_LABEL
    { "GLOBAL__I.", sizeof ("GLOBAL__I.")-1, 1, 0 },
    { "GLOBAL__D.", sizeof ("GLOBAL__D.")-1, 2, 0 },
#endif /* NO_DOT_IN_LABEL */
#endif /* NO_DOLLAR_IN_LABEL */
    { "GLOBAL__I_", sizeof ("GLOBAL__I_")-1, 1, 0 },
    { "GLOBAL__D_", sizeof ("GLOBAL__D_")-1, 2, 0 },
    { "GLOBAL__F_", sizeof ("GLOBAL__F_")-1, 5, 0 },
    { "GLOBAL__FI_", sizeof ("GLOBAL__FI_")-1, 3, 0 },
    { "GLOBAL__FD_", sizeof ("GLOBAL__FD_")-1, 4, 0 },
#ifdef CFRONT_LOSSAGE /* Do not collect cfront initialization functions.
			 cfront has its own linker procedure to collect them;
			 if collect2 gets them too, they get collected twice
			 when the cfront procedure is run and the compiler used
			 for linking happens to be GCC.  */
    { "sti__", sizeof ("sti__")-1, 1, 1 },
    { "std__", sizeof ("std__")-1, 2, 1 },
#endif /* CFRONT_LOSSAGE */
    { NULL, 0, 0, 0 }
  };

  while ((ch = *s) == '_')
    ++s;

  if (s == orig_s)
    return 0;

  for (p = &special[0]; p->len > 0; p++)
    {
      if (ch == p->name[0]
	  && (!p->two_underscores || ((s - orig_s) >= 2))
	  && strncmp(s, p->name, p->len) == 0)
	{
	  return p->ret;
	}
    }
  return 0;
}

/* We maintain two prefix lists: one from COMPILER_PATH environment variable
   and one from the PATH variable.  */

static struct path_prefix cpath, path;

#ifdef CROSS_COMPILE
/* This is the name of the target machine.  We use it to form the name
   of the files to execute.  */

static const char *const target_machine = TARGET_MACHINE;
#endif

/* Search for NAME using prefix list PPREFIX.  We only look for executable
   files. 

   Return 0 if not found, otherwise return its name, allocated with malloc.  */

static char *
find_a_file (pprefix, name)
     struct path_prefix *pprefix;
     const char *name;
{
  char *temp;
  struct prefix_list *pl;
  int len = pprefix->max_len + strlen (name) + 1;

  if (debug)
    fprintf (stderr, "Looking for '%s'\n", name);
  
#ifdef HOST_EXECUTABLE_SUFFIX
  len += strlen (HOST_EXECUTABLE_SUFFIX);
#endif

  temp = xmalloc (len);

  /* Determine the filename to execute (special case for absolute paths).  */

  if (*name == '/'
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
      || (*name && name[1] == ':')
#endif
      )
    {
      if (access (name, X_OK) == 0)
	{
	  strcpy (temp, name);

	  if (debug)
	    fprintf (stderr, "  - found: absolute path\n");
	  
	  return temp;
	}

#ifdef HOST_EXECUTABLE_SUFFIX
	/* Some systems have a suffix for executable files.
	   So try appending that.  */
      strcpy (temp, name);
	strcat (temp, HOST_EXECUTABLE_SUFFIX);
	
	if (access (temp, X_OK) == 0)
	  return temp;
#endif

      if (debug)
	fprintf (stderr, "  - failed to locate using absolute path\n");
    }
  else
    for (pl = pprefix->plist; pl; pl = pl->next)
      {
	struct stat st;

	strcpy (temp, pl->prefix);
	strcat (temp, name);
	
	if (stat (temp, &st) >= 0
	    && ! S_ISDIR (st.st_mode)
	    && access (temp, X_OK) == 0)
	  return temp;

#ifdef HOST_EXECUTABLE_SUFFIX
	/* Some systems have a suffix for executable files.
	   So try appending that.  */
	strcat (temp, HOST_EXECUTABLE_SUFFIX);
	
	if (stat (temp, &st) >= 0
	    && ! S_ISDIR (st.st_mode)
	    && access (temp, X_OK) == 0)
	  return temp;
#endif
      }

  if (debug && pprefix->plist == NULL)
    fprintf (stderr, "  - failed: no entries in prefix list\n");

  free (temp);
  return 0;
}

/* Add an entry for PREFIX to prefix list PPREFIX.  */

static void
add_prefix (pprefix, prefix)
     struct path_prefix *pprefix;
     const char *prefix;
{
  struct prefix_list *pl, **prev;
  int len;

  if (pprefix->plist)
    {
      for (pl = pprefix->plist; pl->next; pl = pl->next)
	;
      prev = &pl->next;
    }
  else
    prev = &pprefix->plist;

  /* Keep track of the longest prefix */

  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = (struct prefix_list *) xmalloc (sizeof (struct prefix_list));
  pl->prefix = xstrdup (prefix);

  if (*prev)
    pl->next = *prev;
  else
    pl->next = (struct prefix_list *) 0;
  *prev = pl;
}

/* Take the value of the environment variable ENV, break it into a path, and
   add of the entries to PPREFIX.  */

static void
prefix_from_env (env, pprefix)
     const char *env;
     struct path_prefix *pprefix;
{
  const char *p;
  GET_ENV_PATH_LIST (p, env);

  if (p)
    prefix_from_string (p, pprefix);
}

static void
prefix_from_string (p, pprefix)
     const char *p;
     struct path_prefix *pprefix;
{
  const char *startp, *endp;
  char *nstore = (char *) xmalloc (strlen (p) + 3);

  if (debug)
    fprintf (stderr, "Convert string '%s' into prefixes, separator = '%c'\n", p, PATH_SEPARATOR);
  
  startp = endp = p;
  while (1)
    {
      if (*endp == PATH_SEPARATOR || *endp == 0)
	{
	  strncpy (nstore, startp, endp-startp);
	  if (endp == startp)
	    {
	      strcpy (nstore, "./");
	    }
	  else if (! IS_DIR_SEPARATOR (endp[-1]))
	    {
	      nstore[endp-startp] = DIR_SEPARATOR;
	      nstore[endp-startp+1] = 0;
	    }
	  else
	    nstore[endp-startp] = 0;

	  if (debug)
	    fprintf (stderr, "  - add prefix: %s\n", nstore);
	  
	  add_prefix (pprefix, nstore);
	  if (*endp == 0)
	    break;
	  endp = startp = endp + 1;
	}
      else
	endp++;
    }
}

/* Main program.  */

int main 		PARAMS ((int, char *[]));
int
main (argc, argv)
     int argc;
     char *argv[];
{
  static const char *const ld_suffix	= "ld";
  static const char *const real_ld_suffix = "real-ld";
  static const char *const collect_ld_suffix = "collect-ld";
  static const char *const nm_suffix	= "nm";
  static const char *const gnm_suffix	= "gnm";
#ifdef LDD_SUFFIX
  static const char *const ldd_suffix	= LDD_SUFFIX;
#endif
  static const char *const strip_suffix = "strip";
  static const char *const gstrip_suffix = "gstrip";

#ifdef CROSS_COMPILE
  /* If we look for a program in the compiler directories, we just use
     the short name, since these directories are already system-specific.
     But it we look for a program in the system directories, we need to
     qualify the program name with the target machine.  */

  const char *const full_ld_suffix =
    concat(target_machine, "-", ld_suffix, NULL);
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
  const char *const full_ld_suffix	= ld_suffix;
  const char *const full_nm_suffix	= nm_suffix;
  const char *const full_gnm_suffix	= gnm_suffix;
#ifdef LDD_SUFFIX
  const char *const full_ldd_suffix	= ldd_suffix;
#endif
  const char *const full_strip_suffix	= strip_suffix;
  const char *const full_gstrip_suffix	= gstrip_suffix;
#endif /* CROSS_COMPILE */

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
  char **ld2_argv;
  const char **ld2;
  char **object_lst;
  const char **object;
  int first_file;
  int num_c_args	= argc+9;

  no_demangle = !! getenv ("COLLECT_NO_DEMANGLE");

  /* Suppress demangling by the real linker, which may be broken.  */
  putenv (xstrdup ("COLLECT_NO_DEMANGLE="));

#if defined (COLLECT2_HOST_INITIALIZATION)
  /* Perform system dependent initialization, if necessary.  */
  COLLECT2_HOST_INITIALIZATION;
#endif

#ifdef SIGCHLD
  /* We *MUST* set SIGCHLD to SIG_DFL so that the wait4() call will
     receive the signal.  A different setting is inheritable */
  signal (SIGCHLD, SIG_DFL);
#endif

  gcc_init_libintl ();

  /* Do not invoke xcalloc before this point, since locale needs to be
     set first, in case a diagnostic is issued.  */

  ld1 = (const char **)(ld1_argv = (char **) xcalloc(sizeof (char *), argc+3));
  ld2 = (const char **)(ld2_argv = (char **) xcalloc(sizeof (char *), argc+10));
  object = (const char **)(object_lst = (char **) xcalloc(sizeof (char *), argc));

#ifdef DEBUG
  debug = 1;
#endif

  /* Parse command line early for instances of -debug.  This allows
     the debug flag to be set before functions like find_a_file()
     are called.  */
  {
    int i;
    
    for (i = 1; argv[i] != NULL; i ++)
      if (! strcmp (argv[i], "-debug"))
	debug = 1;
    vflag = debug;
  }

#ifndef DEFAULT_A_OUT_NAME
  output_file = "a.out";
#else
  output_file = DEFAULT_A_OUT_NAME;
#endif

  obstack_begin (&temporary_obstack, 0);
  obstack_begin (&permanent_obstack, 0);
  temporary_firstobj = (char *) obstack_alloc (&temporary_obstack, 0);

  current_demangling_style = auto_demangling;
  p = getenv ("COLLECT_GCC_OPTIONS");
  while (p && *p)
    {
      const char *q = extract_string (&p);
      if (*q == '-' && (q[1] == 'm' || q[1] == 'f'))
	num_c_args++;
    }
  obstack_free (&temporary_obstack, temporary_firstobj);

  /* -fno-exceptions -w */
  num_c_args += 2;

  c_ptr = (const char **)
    (c_argv = (char **) xcalloc (sizeof (char *), num_c_args));

  if (argc < 2)
    fatal ("no arguments");

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
  ld_file_name = find_a_file (&path, REAL_LD_FILE_NAME);
  if (ld_file_name == 0)
#endif
  /* Search the (target-specific) compiler dirs for ld'.  */
  ld_file_name = find_a_file (&cpath, real_ld_suffix);
  /* Likewise for `collect-ld'.  */
  if (ld_file_name == 0)
    ld_file_name = find_a_file (&cpath, collect_ld_suffix);
  /* Search the compiler directories for `ld'.  We have protection against
     recursive calls in find_a_file.  */
  if (ld_file_name == 0)
    ld_file_name = find_a_file (&cpath, ld_suffix);
  /* Search the ordinary system bin directories
     for `ld' (if native linking) or `TARGET-ld' (if cross).  */
  if (ld_file_name == 0)
    ld_file_name = find_a_file (&path, full_ld_suffix);

#ifdef REAL_NM_FILE_NAME
  nm_file_name = find_a_file (&path, REAL_NM_FILE_NAME);
  if (nm_file_name == 0)
#endif
  nm_file_name = find_a_file (&cpath, gnm_suffix);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&path, full_gnm_suffix);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&cpath, nm_suffix);
  if (nm_file_name == 0)
    nm_file_name = find_a_file (&path, full_nm_suffix);

#ifdef LDD_SUFFIX
  ldd_file_name = find_a_file (&cpath, ldd_suffix);
  if (ldd_file_name == 0)
    ldd_file_name = find_a_file (&path, full_ldd_suffix);
#endif

#ifdef REAL_STRIP_FILE_NAME
  strip_file_name = find_a_file (&path, REAL_STRIP_FILE_NAME);
  if (strip_file_name == 0)
#endif
  strip_file_name = find_a_file (&cpath, gstrip_suffix);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&path, full_gstrip_suffix);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&cpath, strip_suffix);
  if (strip_file_name == 0)
    strip_file_name = find_a_file (&path, full_strip_suffix);

  /* Determine the full path name of the C compiler to use.  */
  c_file_name = getenv ("COLLECT_GCC");
  if (c_file_name == 0)
    {
#ifdef CROSS_COMPILE
      c_file_name = concat (target_machine, "-gcc", NULL);
#else
      c_file_name = "gcc";
#endif
    }

  p = find_a_file (&cpath, c_file_name);

  /* Here it should be safe to use the system search path since we should have
     already qualified the name of the compiler when it is needed.  */
  if (p == 0)
    p = find_a_file (&path, c_file_name);

  if (p)
    c_file_name = p;

  *ld1++ = *ld2++ = ld_file_name;

  /* Make temp file names.  */
  c_file = make_temp_file (".c");
  o_file = make_temp_file (".o");
#ifdef COLLECT_EXPORT_LIST
  export_file = make_temp_file (".x");
#endif
  ldout = make_temp_file (".ld");
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
	*c_ptr++ = obstack_copy0 (&permanent_obstack, q, strlen (q));
      if (strcmp (q, "-EL") == 0 || strcmp (q, "-EB") == 0)
	*c_ptr++ = obstack_copy0 (&permanent_obstack, q, strlen (q));
      if (strcmp (q, "-shared") == 0)
	shared_obj = 1;
      if (*q == '-' && q[1] == 'B')
	{
	  *c_ptr++ = obstack_copy0 (&permanent_obstack, q, strlen (q));
	  if (q[2] == 0)
	    {
	      q = extract_string (&p);
	      *c_ptr++ = obstack_copy0 (&permanent_obstack, q, strlen (q));
	    }
	}
    }
  obstack_free (&temporary_obstack, temporary_firstobj);
  *c_ptr++ = "-fno-exceptions";
  *c_ptr++ = "-w";

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
#ifdef COLLECT_EXPORT_LIST
	    /* We want to disable automatic exports on AIX when user
	       explicitly puts an export list in command line */
	    case 'b':
	      if (arg[2] == 'E' || strncmp (&arg[2], "export", 6) == 0)
                export_flag = 1;
	      else if (arg[2] == '6' && arg[3] == '4')
		aix64_flag = 1;
	      break;
#endif

	    case 'd':
	      if (!strcmp (arg, "-debug"))
		{
		  /* Already parsed.  */
		  ld1--;
		  ld2--;
		}
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
#else 
#if LINK_ELIMINATE_DUPLICATE_LDIRECTORIES
	    case 'L':
	      if (is_in_args (arg, (const char **) ld1_argv, ld1-1))
		--ld1;
	      break;
#endif /* LINK_ELIMINATE_DUPLICATE_LDIRECTORIES */
#endif

	    case 'o':
	      if (arg[2] == '\0')
		output_file = *ld1++ = *ld2++ = *++argv;
	      else if (1
#ifdef SWITCHES_NEED_SPACES
		       && ! strchr (SWITCHES_NEED_SPACES, arg[1])
#endif
		       )

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
		vflag = 1;
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
     nothing else in the file is referenced, so look at them first.  */
  {
      const char **export_object_lst = (const char **)object_lst;

      while (export_object_lst < object)
	scan_prog_file (*export_object_lst++, PASS_OBJ);
  }
  {
    struct id *list = libs.first;

    for (; list; list = list->next)
      scan_prog_file (list->name, PASS_FIRST);
  }

  if (exports.first)
    {
      char *buf = concat ("-bE:", export_file, NULL);
      
      *ld1++ = buf;
      *ld2++ = buf;

      exportf = fopen (export_file, "w");
      if (exportf == (FILE *) 0)
	fatal_perror ("fopen %s", export_file);
      write_aix_file (exportf, exports.first);
      if (fclose (exportf))
	fatal_perror ("fclose %s", export_file);
    }
#endif

  *c_ptr++ = c_file;
  *c_ptr = *ld1 = *object = (char *) 0;

  if (vflag)
    {
      notice ("collect2 version %s", version_string);
#ifdef TARGET_VERSION
      TARGET_VERSION;
#endif
      fprintf (stderr, "\n");
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
     undefined symbols from repository information.  */

  /* On AIX we do this later.  */
#ifndef COLLECT_EXPORT_LIST
  do_tlink (ld1_argv, object_lst);
#endif

  /* If -r or they will be run via some other method, do not build the
     constructor or destructor list, just return now.  */
  if (rflag
#ifndef COLLECT_EXPORT_LIST
      || ! do_collecting
#endif
      )
    {
#ifdef COLLECT_EXPORT_LIST
      /* Do the link we avoided above if we are exiting.  */
      do_tlink (ld1_argv, object_lst);

      /* But make sure we delete the export file we may have created.  */
      if (export_file != 0 && export_file[0])
	maybe_unlink (export_file);
#endif
      maybe_unlink (c_file);
      maybe_unlink (o_file);
      return 0;
    }

  /* Examine the namelist with nm and search it for static constructors
     and destructors to call.
     Write the constructor and destructor tables to a .s file and reload.  */

  /* On AIX we already scanned for global constructors/destructors.  */
#ifndef COLLECT_EXPORT_LIST
  scan_prog_file (output_file, PASS_FIRST);
#endif

#ifdef SCAN_LIBRARIES
  scan_libraries (output_file);
#endif

  if (debug)
    {
      notice ("%d constructor(s) found\n", constructors.number);
      notice ("%d destructor(s)  found\n", destructors.number);
      notice ("%d frame table(s) found\n", frame_tables.number);
    }

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
#ifdef COLLECT_EXPORT_LIST
      /* Do tlink without additional code generation */
      do_tlink (ld1_argv, object_lst);
#endif
      /* Strip now if it was requested on the command line.  */
      if (strip_flag)
	{
	  char **real_strip_argv = (char **) xcalloc (sizeof (char *), 3);
	  const char ** strip_argv = (const char **) real_strip_argv;
	  
	  strip_argv[0] = strip_file_name;
	  strip_argv[1] = output_file;
	  strip_argv[2] = (char *) 0;
	  fork_execute ("strip", real_strip_argv);
	}

#ifdef COLLECT_EXPORT_LIST
      maybe_unlink (export_file);
#endif
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
    fatal_perror ("fopen %s", c_file);

  write_c_file (outf, c_file);

  if (fclose (outf))
    fatal_perror ("fclose %s", c_file);

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

      add_to_list (&exports, initname);
      add_to_list (&exports, fininame);
      add_to_list (&exports, "_GLOBAL__DI");
      add_to_list (&exports, "_GLOBAL__DD");
      exportf = fopen (export_file, "w");
      if (exportf == (FILE *) 0)
	fatal_perror ("fopen %s", export_file);
      write_aix_file (exportf, exports.first);
      if (fclose (exportf))
	fatal_perror ("fclose %s", export_file);
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
  /* On AIX we must call tlink because of possible templates resolution */
  do_tlink (ld2_argv, object_lst);
#else
  /* Otherwise, simply call ld because tlink is already done */
  fork_execute ("ld", ld2_argv);

  /* Let scan_prog_file do any final mods (OSF/rose needs this for
     constructors/destructors in shared libraries.  */
  scan_prog_file (output_file, PASS_SECOND);
#endif 

  maybe_unlink (c_file);
  maybe_unlink (o_file);

#ifdef COLLECT_EXPORT_LIST
  maybe_unlink (export_file);
#endif

  return 0;
}


/* Wait for a process to finish, and exit if a non-zero status is found.  */

int
collect_wait (prog)
     const char *prog;
{
  int status;

  pwait (pexecute_pid, &status, 0);
  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  error ("%s terminated with signal %d [%s]%s",
		 prog, sig, strsignal(sig),
		 WCOREDUMP(status) ? ", core dumped" : "");
	  collect_exit (FATAL_EXIT_CODE);
	}

      if (WIFEXITED (status))
	return WEXITSTATUS (status);
    }
  return 0;
}

static void
do_wait (prog)
     const char *prog;
{
  int ret = collect_wait (prog);
  if (ret != 0)
    {
      error ("%s returned %d exit status", prog, ret);
      collect_exit (ret);
    }
}


/* Execute a program, and wait for the reply.  */

void
collect_execute (prog, argv, redir)
     const char *prog;
     char **argv;
     const char *redir;
{
  char *errmsg_fmt;
  char *errmsg_arg;
  int redir_handle = -1;
  int stdout_save = -1;
  int stderr_save = -1;

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
    fatal ("cannot find `%s'", prog);

  if (redir)
    {
      /* Open response file.  */
      redir_handle = open (redir, O_WRONLY | O_TRUNC | O_CREAT);

      /* Duplicate the stdout and stderr file handles
	 so they can be restored later.  */
      stdout_save = dup (STDOUT_FILENO);
      if (stdout_save == -1)
	fatal_perror ("redirecting stdout: %s", redir);
      stderr_save = dup (STDERR_FILENO);
      if (stderr_save == -1)
	fatal_perror ("redirecting stdout: %s", redir);

      /* Redirect stdout & stderr to our response file.  */
      dup2 (redir_handle, STDOUT_FILENO);
      dup2 (redir_handle, STDERR_FILENO);
    }

  pexecute_pid = pexecute (argv[0], argv, argv[0], NULL,
			   &errmsg_fmt, &errmsg_arg,
			   (PEXECUTE_FIRST | PEXECUTE_LAST | PEXECUTE_SEARCH));

  if (redir)
    {
      /* Restore stdout and stderr to their previous settings.  */
      dup2 (stdout_save, STDOUT_FILENO);
      dup2 (stderr_save, STDERR_FILENO);

      /* Close response file.  */
      close (redir_handle);
    }

 if (pexecute_pid == -1)
   fatal_perror (errmsg_fmt, errmsg_arg);
}

static void
fork_execute (prog, argv)
     const char *prog;
     char **argv;
{
  collect_execute (prog, argv, NULL);
  do_wait (prog);
}

/* Unlink a file unless we are debugging.  */

static void
maybe_unlink (file)
     const char *file;
{
  if (!debug)
    unlink (file);
  else
    notice ("[Leaving %s]\n", file);
}


static long sequence_number = 0;

/* Add a name to a linked list.  */

static void
add_to_list (head_ptr, name)
     struct head *head_ptr;
     const char *name;
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
extract_init_priority (name)
     const char *name;
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
sort_ids (head_ptr)
     struct head *head_ptr;
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
write_list (stream, prefix, list)
     FILE *stream;
     const char *prefix;
     struct id *list;
{
  while (list)
    {
      fprintf (stream, "%sx%d,\n", prefix, list->sequence);
      list = list->next;
    }
}

#if LINK_ELIMINATE_DUPLICATE_LDIRECTORIES
/* Given a STRING, return nonzero if it occurs in the list in range
   [ARGS_BEGIN,ARGS_END).  */

static int
is_in_args (string, args_begin, args_end)
     const char *string;
     const char **args_begin;
     const char **args_end;
{
  const char **args_pointer;
  for (args_pointer = args_begin; args_pointer != args_end; ++args_pointer)
    if (strcmp (string, *args_pointer) == 0)
      return 1;
  return 0;
}
#endif /* LINK_ELIMINATE_DUPLICATE_LDIRECTORIES */

#ifdef COLLECT_EXPORT_LIST
/* This function is really used only on AIX, but may be useful.  */
#if 0
static int
is_in_list (prefix, list)
     const char *prefix;
     struct id *list;
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
dump_list (stream, prefix, list)
     FILE *stream;
     const char *prefix;
     struct id *list;
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
dump_prefix_list (stream, prefix, list)
     FILE *stream;
     const char *prefix;
     struct prefix_list *list;
{
  while (list)
    {
      fprintf (stream, "%s%s,\n", prefix, list->prefix);
      list = list->next;
    }
}
#endif

static void
write_list_with_asm (stream, prefix, list)
     FILE *stream;
     const char *prefix;
     struct id *list;
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
write_c_file_stat (stream, name)
     FILE *stream;
     const char *name ATTRIBUTE_UNUSED;
{
  const char *p, *q;
  char *prefix, *r;
  int frames = (frame_tables.number > 0);

  /* Figure out name of output_file, stripping off .so version.  */
  p = strrchr (output_file, '/');
  if (p == 0)
    p = output_file;
  else
    p++;
  q = p;
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
	  if (strncmp (q, ".so", 3) == 0)
	    {
	      q += 3;
	      break;
	    }
	  else
	    q++;
	}
    }
  /* q points to null at end of the string (or . of the .so version) */
  prefix = xmalloc (q - p + 1);
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

  /* Write the tables as C code  */

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
write_c_file_glob (stream, name)
     FILE *stream;
     const char *name ATTRIBUTE_UNUSED;
{
  /* Write the tables as C code  */

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
write_c_file (stream, name)
     FILE *stream;
     const char *name;
{
  fprintf (stream, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
#ifndef LD_INIT_SWITCH
  if (! shared_obj)
    write_c_file_glob (stream, name);
  else
#endif
    write_c_file_stat (stream, name);
  fprintf (stream, "#ifdef __cplusplus\n}\n#endif\n");
}

#ifdef COLLECT_EXPORT_LIST
static void
write_aix_file (stream, list)
     FILE *stream;
     struct id *list;
{
  for (; list; list = list->next)
    {
      fputs (list->name, stream);
      putc ('\n', stream);
    }
}
#endif

#ifdef OBJECT_FORMAT_NONE

/* Generic version to scan the name list of the loaded program for
   the symbols g++ uses for static constructors and destructors.

   The constructor table begins at __CTOR_LIST__ and contains a count
   of the number of pointers (or -1 if the constructors are built in a
   separate section by the linker), followed by the pointers to the
   constructor functions, terminated with a null pointer.  The
   destructor table has the same format, and begins at __DTOR_LIST__.  */

static void
scan_prog_file (prog_name, which_pass)
     const char *prog_name;
     enum pass which_pass;
{
  void (*int_handler) PARAMS ((int));
  void (*quit_handler) PARAMS ((int));
  char *real_nm_argv[4];
  const char **nm_argv = (const char **) real_nm_argv;
  int pid;
  int argc = 0;
  int pipe_fd[2];
  char *p, buf[1024];
  FILE *inf;

  if (which_pass == PASS_SECOND)
    return;

  /* If we do not have an `nm', complain.  */
  if (nm_file_name == 0)
    fatal ("cannot find `nm'");

  nm_argv[argc++] = nm_file_name;
  if (NM_FLAGS[0] != '\0')
    nm_argv[argc++] = NM_FLAGS;

  nm_argv[argc++] = prog_name;
  nm_argv[argc++] = (char *) 0;

  if (pipe (pipe_fd) < 0)
    fatal_perror ("pipe");

  inf = fdopen (pipe_fd[0], "r");
  if (inf == (FILE *) 0)
    fatal_perror ("fdopen");

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

  /* Spawn child nm on pipe */
  pid = vfork ();
  if (pid == -1)
    fatal_perror (VFORK_STRING);

  if (pid == 0)			/* child context */
    {
      /* setup stdout */
      if (dup2 (pipe_fd[1], 1) < 0)
	fatal_perror ("dup2 %d 1", pipe_fd[1]);

      if (close (pipe_fd[0]) < 0)
	fatal_perror ("close %d", pipe_fd[0]);

      if (close (pipe_fd[1]) < 0)
	fatal_perror ("close %d", pipe_fd[1]);

      execv (nm_file_name, real_nm_argv);
      fatal_perror ("execvp %s", nm_file_name);
    }

  /* Parent context from here on.  */
  int_handler  = (void (*) PARAMS ((int))) signal (SIGINT,  SIG_IGN);
#ifdef SIGQUIT
  quit_handler = (void (*) PARAMS ((int))) signal (SIGQUIT, SIG_IGN);
#endif

  if (close (pipe_fd[1]) < 0)
    fatal_perror ("close %d", pipe_fd[1]);

  if (debug)
    fprintf (stderr, "\nnm output with constructors/destructors.\n");

  /* Read each line of nm output.  */
  while (fgets (buf, sizeof buf, inf) != (char *) 0)
    {
      int ch, ch2;
      char *name, *end;

      /* If it contains a constructor or destructor name, add the name
	 to the appropriate list.  */

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
	case 1:
	  if (which_pass != PASS_LIB)
	    add_to_list (&constructors, name);
	  break;

	case 2:
	  if (which_pass != PASS_LIB)
	    add_to_list (&destructors, name);
	  break;

	case 3:
	  if (which_pass != PASS_LIB)
	    fatal ("init function found in object %s", prog_name);
#ifndef LD_INIT_SWITCH
	  add_to_list (&constructors, name);
#endif
	  break;

	case 4:
	  if (which_pass != PASS_LIB)
	    fatal ("fini function found in object %s", prog_name);
#ifndef LD_FINI_SWITCH
	  add_to_list (&destructors, name);
#endif
	  break;

	case 5:
	  if (which_pass != PASS_LIB)
	    add_to_list (&frame_tables, name);
	  break;

	default:		/* not a constructor or destructor */
	  continue;
	}

      if (debug)
	fprintf (stderr, "\t%s\n", buf);
    }

  if (debug)
    fprintf (stderr, "\n");

  if (fclose (inf) != 0)
    fatal_perror ("fclose");

  do_wait (nm_file_name);

  signal (SIGINT,  int_handler);
#ifdef SIGQUIT
  signal (SIGQUIT, quit_handler);
#endif
}

#if SUNOS4_SHARED_LIBRARIES

/* Routines to scan the SunOS 4 _DYNAMIC structure to find shared libraries
   that the output file depends upon and their initialization/finalization
   routines, if any.  */

#include <a.out.h>
#include <fcntl.h>
#include <link.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <unistd.h>
#include <sys/dir.h>

/* pointers to the object file */
unsigned object;    	/* address of memory mapped file */
unsigned objsize;    	/* size of memory mapped to file */
char * code;		/* pointer to code segment */
char * data;		/* pointer to data segment */
struct nlist *symtab;	/* pointer to symbol table */
struct link_dynamic *ld;
struct link_dynamic_2 *ld_2;
struct head libraries;

/* Map the file indicated by NAME into memory and store its address.  */

static void mapfile			PARAMS ((const char *));

static void
mapfile (name)
     const char *name;
{
  int fp;
  struct stat s;
  if ((fp = open (name, O_RDONLY)) == -1)
    fatal ("unable to open file '%s'", name);
  if (fstat (fp, &s) == -1)
    fatal ("unable to stat file '%s'", name);

  objsize = s.st_size;
  object = (unsigned) mmap (0, objsize, PROT_READ|PROT_WRITE, MAP_PRIVATE,
			    fp, 0);
  if (object == (unsigned)-1)
    fatal ("unable to mmap file '%s'", name);

  close (fp);
}

/* Helpers for locatelib.  */

static const char *libname;

static int libselect			PARAMS ((struct direct *));

static int
libselect (d)
     struct direct *d;
{
  return (strncmp (libname, d->d_name, strlen (libname)) == 0);
}

/* If one file has an additional numeric extension past LIBNAME, then put
   that one first in the sort.  If both files have additional numeric
   extensions, then put the one with the higher number first in the sort.

   We must verify that the extension is numeric, because Sun saves the
   original versions of patched libraries with a .FCS extension.  Files with
   invalid extensions must go last in the sort, so that they will not be used.  */
static int libcompare		PARAMS ((struct direct **, struct direct **));

static int
libcompare (d1, d2)
     struct direct **d1, **d2;
{
  int i1, i2 = strlen (libname);
  char *e1 = (*d1)->d_name + i2;
  char *e2 = (*d2)->d_name + i2;

  while (*e1 && *e2 && *e1 == '.' && *e2 == '.'
	 && e1[1] && ISDIGIT (e1[1]) && e2[1] && ISDIGIT (e2[1]))
    {
      ++e1;
      ++e2;
      i1 = strtol (e1, &e1, 10);
      i2 = strtol (e2, &e2, 10);
      if (i1 != i2)
	return i1 - i2;
    }

  if (*e1)
    {
      /* It has a valid numeric extension, prefer this one.  */
      if (*e1 == '.' && e1[1] && ISDIGIT (e1[1]))
	return 1;
      /* It has an invalid numeric extension, must prefer the other one.  */
      else
	return -1;
    }
  else if (*e2)
    {
      /* It has a valid numeric extension, prefer this one.  */
      if (*e2 == '.' && e2[1] && ISDIGIT (e2[1]))
	return -1;
      /* It has an invalid numeric extension, must prefer the other one.  */
      else
	return 1;
    }
  else
    return 0;
}

/* Given the name NAME of a dynamic dependency, find its pathname and add
   it to the list of libraries.  */
static void locatelib			PARAMS ((const char *));

static void
locatelib (name)
     const char *name;
{
  static const char **l;
  static int cnt;
  char buf[MAXPATHLEN];
  char *p, *q;
  const char **pp;

  if (l == 0)
    {
      char *ld_rules;
      char *ldr = 0;
      /* counting elements in array, need 1 extra for null */
      cnt = 1;  
      ld_rules = (char *) (ld_2->ld_rules + code);
      if (ld_rules)
	{
	  cnt++;
	  for (; *ld_rules != 0; ld_rules++)
	    if (*ld_rules == ':')
	      cnt++;
	  ld_rules = (char *) (ld_2->ld_rules + code);
	  ldr = xstrdup (ld_rules);
	}
      p = getenv ("LD_LIBRARY_PATH");
      q = 0;
      if (p)
	{
	  cnt++;
	  for (q = p ; *q != 0; q++)
	    if (*q == ':')
	      cnt++;
	  q = xstrdup (p);
	}
      l = (const char **) xmalloc ((cnt + 3) * sizeof (char *));
      pp = l;
      if (ldr)
	{
	  *pp++ = ldr;
	  for (; *ldr != 0; ldr++) 
	    if (*ldr == ':')
	      {
		*ldr++ = 0;
		*pp++ = ldr;
	      }
	}
      if (q)
	{
	  *pp++ = q;
	  for (; *q != 0; q++) 
	    if (*q == ':')
	      {
		*q++ = 0;
		*pp++ = q;
	      }
	}
      /* built in directories are /lib, /usr/lib, and /usr/local/lib */
      *pp++ = "/lib";
      *pp++ = "/usr/lib";
      *pp++ = "/usr/local/lib";
      *pp = 0;
    }
  libname = name;
  for (pp = l; *pp != 0 ; pp++)
    {
      struct direct **namelist;
      int entries;
      if ((entries = scandir (*pp, &namelist, libselect, libcompare)) > 0)
	{
	  sprintf (buf, "%s/%s", *pp, namelist[entries - 1]->d_name);
	  add_to_list (&libraries, buf);
	  if (debug)
	    fprintf (stderr, "%s\n", buf);
	  break;
	}
    }
  if (*pp == 0)
    {
      if (debug)
	notice ("not found\n");
      else
	fatal ("dynamic dependency %s not found", name);
    }
}

/* Scan the _DYNAMIC structure of the output file to find shared libraries
   that it depends upon and any constructors or destructors they contain.  */

static void 
scan_libraries (prog_name)
     const char *prog_name;
{
  struct exec *header;
  char *base;
  struct link_object *lo;
  char buff[MAXPATHLEN];
  struct id *list;

  mapfile (prog_name);
  header = (struct exec *)object;
  if (N_BADMAG (*header))
    fatal ("bad magic number in file '%s'", prog_name);
  if (header->a_dynamic == 0)
    return;

  code = (char *) (N_TXTOFF (*header) + (long) header);
  data = (char *) (N_DATOFF (*header) + (long) header);
  symtab = (struct nlist *) (N_SYMOFF (*header) + (long) header);

  if (header->a_magic == ZMAGIC && header->a_entry == 0x20)
    {
      /* shared object */
      ld = (struct link_dynamic *) (symtab->n_value + code);
      base = code;
    }
  else
    {
      /* executable */
      ld = (struct link_dynamic *) data;
      base = code-PAGSIZ;
    }

  if (debug)
    notice ("dynamic dependencies.\n");

  ld_2 = (struct link_dynamic_2 *) ((long) ld->ld_un.ld_2 + (long)base);
  for (lo = (struct link_object *) ld_2->ld_need; lo;
       lo = (struct link_object *) lo->lo_next)
    {
      char *name;
      lo = (struct link_object *) ((long) lo + code);
      name = (char *) (code + lo->lo_name);
      if (lo->lo_library)
	{
	  if (debug)
	    fprintf (stderr, "\t-l%s.%d => ", name, lo->lo_major);
	  sprintf (buff, "lib%s.so.%d.%d", name, lo->lo_major, lo->lo_minor);
	  locatelib (buff);
	}
      else
	{
	  if (debug)
	    fprintf (stderr, "\t%s\n", name);
	  add_to_list (&libraries, name);
	}
    }

  if (debug)
    fprintf (stderr, "\n");

  /* now iterate through the library list adding their symbols to
     the list.  */
  for (list = libraries.first; list; list = list->next)
    scan_prog_file (list->name, PASS_LIB);
}

#else  /* SUNOS4_SHARED_LIBRARIES */
#ifdef LDD_SUFFIX

/* Use the List Dynamic Dependencies program to find shared libraries that
   the output file depends upon and their initialization/finalization
   routines, if any.  */

static void 
scan_libraries (prog_name)
     const char *prog_name;
{
  static struct head libraries;		/* list of shared libraries found */
  struct id *list;
  void (*int_handler) PARAMS ((int));
  void (*quit_handler) PARAMS ((int));
  char *real_ldd_argv[4];
  const char **ldd_argv = (const char **) real_ldd_argv;
  int pid;
  int argc = 0;
  int pipe_fd[2];
  char buf[1024];
  FILE *inf;

  /* If we do not have an `ldd', complain.  */
  if (ldd_file_name == 0)
    {
      error ("cannot find `ldd'");
      return;
    }

  ldd_argv[argc++] = ldd_file_name;
  ldd_argv[argc++] = prog_name;
  ldd_argv[argc++] = (char *) 0;

  if (pipe (pipe_fd) < 0)
    fatal_perror ("pipe");

  inf = fdopen (pipe_fd[0], "r");
  if (inf == (FILE *) 0)
    fatal_perror ("fdopen");

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

  /* Spawn child ldd on pipe */
  pid = vfork ();
  if (pid == -1)
    fatal_perror (VFORK_STRING);

  if (pid == 0)			/* child context */
    {
      /* setup stdout */
      if (dup2 (pipe_fd[1], 1) < 0)
	fatal_perror ("dup2 %d 1", pipe_fd[1]);

      if (close (pipe_fd[0]) < 0)
	fatal_perror ("close %d", pipe_fd[0]);

      if (close (pipe_fd[1]) < 0)
	fatal_perror ("close %d", pipe_fd[1]);

      execv (ldd_file_name, real_ldd_argv);
      fatal_perror ("execv %s", ldd_file_name);
    }

  /* Parent context from here on.  */
  int_handler  = (void (*) PARAMS ((int))) signal (SIGINT,  SIG_IGN);
#ifdef SIGQUIT
  quit_handler = (void (*) PARAMS ((int))) signal (SIGQUIT, SIG_IGN);
#endif

  if (close (pipe_fd[1]) < 0)
    fatal_perror ("close %d", pipe_fd[1]);

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
	fatal ("dynamic dependency %s not found", buf);

      /* Find the end of the symbol name.  */
      for (end = p; 
	   (ch2 = *end) != '\0' && ch2 != '\n' && !ISSPACE (ch2) && ch2 != '|';
	   end++)
	continue;
      *end = '\0';

      if (access (name, R_OK) == 0)
        add_to_list (&libraries, name);
      else
	fatal ("unable to open dynamic dependency '%s'", buf);

      if (debug)
	fprintf (stderr, "\t%s\n", buf);
    }
  if (debug)
    fprintf (stderr, "\n");

  if (fclose (inf) != 0)
    fatal_perror ("fclose");

  do_wait (ldd_file_name);

  signal (SIGINT,  int_handler);
#ifdef SIGQUIT
  signal (SIGQUIT, quit_handler);
#endif

  /* now iterate through the library list adding their symbols to
     the list.  */
  for (list = libraries.first; list; list = list->next)
    scan_prog_file (list->name, PASS_LIB);
}

#endif /* LDD_SUFFIX */
#endif /* SUNOS4_SHARED_LIBRARIES */

#endif /* OBJECT_FORMAT_NONE */


/*
 * COFF specific stuff.
 */

#ifdef OBJECT_FORMAT_COFF

#if defined(EXTENDED_COFF)

#   define GCC_SYMBOLS(X)	(SYMHEADER(X).isymMax + SYMHEADER(X).iextMax)
#   define GCC_SYMENT		SYMR
#   define GCC_OK_SYMBOL(X)	((X).st == stProc || (X).st == stGlobal)
#   define GCC_SYMINC(X)	(1)
#   define GCC_SYMZERO(X)	(SYMHEADER(X).isymMax)
#   define GCC_CHECK_HDR(X)	(PSYMTAB(X) != 0)

#else

#   define GCC_SYMBOLS(X)	(HEADER(ldptr).f_nsyms)
#   define GCC_SYMENT		SYMENT
#   define GCC_OK_SYMBOL(X) \
     (((X).n_sclass == C_EXT) && \
      ((X).n_scnum > N_UNDEF) && \
      (aix64_flag \
       || (((X).n_type & N_TMASK) == (DT_NON << N_BTSHFT) \
           || ((X).n_type & N_TMASK) == (DT_FCN << N_BTSHFT))))
#   define GCC_UNDEF_SYMBOL(X) \
     (((X).n_sclass == C_EXT) && ((X).n_scnum == N_UNDEF))
#   define GCC_SYMINC(X)	((X).n_numaux+1)
#   define GCC_SYMZERO(X)	0

/* 0757 = U803XTOCMAGIC (AIX 4.3) and 0767 = U64_TOCMAGIC (AIX V5) */
#ifdef _AIX51
#   define GCC_CHECK_HDR(X) \
     ((HEADER (X).f_magic == U802TOCMAGIC && ! aix64_flag) \
      || (HEADER (X).f_magic == 0767 && aix64_flag))
#else
#   define GCC_CHECK_HDR(X) \
     ((HEADER (X).f_magic == U802TOCMAGIC && ! aix64_flag) \
      || (HEADER (X).f_magic == 0757 && aix64_flag))
#endif

#endif

extern char *ldgetname ();

/* COFF version to scan the name list of the loaded program for
   the symbols g++ uses for static constructors and destructors.

   The constructor table begins at __CTOR_LIST__ and contains a count
   of the number of pointers (or -1 if the constructors are built in a
   separate section by the linker), followed by the pointers to the
   constructor functions, terminated with a null pointer.  The
   destructor table has the same format, and begins at __DTOR_LIST__.  */

static void
scan_prog_file (prog_name, which_pass)
     const char *prog_name;
     enum pass which_pass;
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
         which will cause -Wcast-qual to burp.  */
      if ((ldptr = ldopen ((char *)prog_name, ldptr)) != NULL)
	{
	  if (! MY_ISCOFF (HEADER (ldptr).f_magic))
	    fatal ("%s: not a COFF file", prog_name);

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
			continue;		/* should never happen */

#ifdef XCOFF_DEBUGGING_INFO
		      /* All AIX function names have a duplicate entry
			 beginning with a dot.  */
		      if (*name == '.')
			++name;
#endif

		      switch (is_ctor_dtor (name))
			{
			case 1:
			  if (! is_shared)
			    add_to_list (&constructors, name);
#ifdef COLLECT_EXPORT_LIST
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

			case 2:
			  if (! is_shared)
			    add_to_list (&destructors, name);
#ifdef COLLECT_EXPORT_LIST
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

#ifdef COLLECT_EXPORT_LIST
			case 3:
#ifndef LD_INIT_SWITCH
			  if (is_shared)
			    add_to_list (&constructors, name);
#endif
			  break;

			case 4:
#ifndef LD_INIT_SWITCH
			  if (is_shared)
			    add_to_list (&destructors, name);
#endif
			  break;
#endif

			case 5:
			  if (! is_shared)
			    add_to_list (&frame_tables, name);
#ifdef COLLECT_EXPORT_LIST
			  if (which_pass == PASS_OBJ)
			    add_to_list (&exports, name);
#endif
			  break;

			default:	/* not a constructor or destructor */
#ifdef COLLECT_EXPORT_LIST
			  /* If we are building a shared object on AIX we need
			     to explicitly export all global symbols.  */
			  if (shared_obj) 
			    {
			      if (which_pass == PASS_OBJ && (! export_flag))
				add_to_list (&exports, name);
			    }
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
	  fatal ("%s: cannot open as COFF file", prog_name);
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


#ifdef COLLECT_EXPORT_LIST
/* Given a library name without "lib" prefix, this function
   returns a full library name including a path.  */
static char *
resolve_lib_name (name)
     const char *name;
{
  char *lib_buf;
  int i, j, l = 0;

  for (i = 0; libpaths[i]; i++)
    if (libpaths[i]->max_len > l)
      l = libpaths[i]->max_len;

  lib_buf = xmalloc (l + strlen(name) + 10);

  for (i = 0; libpaths[i]; i++)
    {
      struct prefix_list *list = libpaths[i]->plist;
      for (; list; list = list->next)
	{
	  /* The following lines are needed because path_prefix list
	     may contain directories both with trailing '/' and
	     without it.  */
	  const char *p = "";
	  if (list->prefix[strlen(list->prefix)-1] != '/')
	    p = "/";
	  for (j = 0; libexts[j]; j++)
	    {
       	      sprintf (lib_buf, "%s%slib%s.%s",
		       list->prefix, p, name, libexts[j]);
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
    fatal ("library lib%s not found", name);
  return (NULL);
}

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
static int
ignore_library (name)
     const char *name;
{
  const char *const *p = &aix_std_libs[0];
  while (*p++ != NULL)
    if (! strcmp (name, *p)) return 1;
  return 0;
}
#endif

#endif /* OBJECT_FORMAT_COFF */


/*
 * OSF/rose specific stuff.
 */

#ifdef OBJECT_FORMAT_ROSE

/* Union of the various load commands */

typedef union load_union
{
  ldc_header_t			hdr;	/* common header */
  load_cmd_map_command_t	map;	/* map indexing other load cmds */
  interpreter_command_t		iprtr;	/* interpreter pathname */
  strings_command_t		str;	/* load commands strings section */
  region_command_t		region;	/* region load command */
  reloc_command_t		reloc;	/* relocation section */
  package_command_t		pkg;	/* package load command */
  symbols_command_t		sym;	/* symbol sections */
  entry_command_t		ent;	/* program start section */
  gen_info_command_t		info;	/* object information */
  func_table_command_t		func;	/* function constructors/destructors */
} load_union_t;

/* Structure to point to load command and data section in memory.  */

typedef struct load_all
{
  load_union_t *load;			/* load command */
  char *section;			/* pointer to section */
} load_all_t;

/* Structure to contain information about a file mapped into memory.  */

struct file_info
{
  char *start;				/* start of map */
  char *name;				/* filename */
  long	size;				/* size of the file */
  long  rounded_size;			/* size rounded to page boundary */
  int	fd;				/* file descriptor */
  int	rw;				/* != 0 if opened read/write */
  int	use_mmap;			/* != 0 if mmap'ed */
};

extern int decode_mach_o_hdr ();
extern int encode_mach_o_hdr ();

static void add_func_table	PARAMS ((mo_header_t *, load_all_t *,
				       symbol_info_t *, int));
static void print_header	PARAMS ((mo_header_t *));
static void print_load_command	PARAMS ((load_union_t *, size_t, int));
static void bad_header		PARAMS ((int));
static struct file_info	*read_file  PARAMS ((const char *, int, int));
static void end_file		PARAMS ((struct file_info *));

/* OSF/rose specific version to scan the name list of the loaded
   program for the symbols g++ uses for static constructors and
   destructors.

   The constructor table begins at __CTOR_LIST__ and contains a count
   of the number of pointers (or -1 if the constructors are built in a
   separate section by the linker), followed by the pointers to the
   constructor functions, terminated with a null pointer.  The
   destructor table has the same format, and begins at __DTOR_LIST__.  */

static void
scan_prog_file (prog_name, which_pass)
     const char *prog_name;
     enum pass which_pass;
{
  char *obj;
  mo_header_t hdr;
  load_all_t *load_array;
  load_all_t *load_end;
  load_all_t *load_cmd;
  int symbol_load_cmds;
  off_t offset;
  int i;
  int num_syms;
  int status;
  char *str_sect;
  struct file_info *obj_file;
  int prog_fd;
  mo_lcid_t cmd_strings	  = -1;
  symbol_info_t *main_sym = 0;
  int rw		  = (which_pass != PASS_FIRST);

  prog_fd = open (prog_name, (rw) ? O_RDWR : O_RDONLY);
  if (prog_fd < 0)
    fatal_perror ("open %s", prog_name);

  obj_file = read_file (prog_name, prog_fd, rw);
  obj = obj_file->start;

  status = decode_mach_o_hdr (obj, MO_SIZEOF_RAW_HDR, MOH_HEADER_VERSION, &hdr);
  if (status != MO_HDR_CONV_SUCCESS)
    bad_header (status);


  /* Do some basic sanity checks.  Note we explicitly use the big endian magic number,
     since the hardware will automatically swap bytes for us on loading little endian
     integers.  */

#ifndef CROSS_COMPILE
  if (hdr.moh_magic != MOH_MAGIC_MSB
      || hdr.moh_header_version != MOH_HEADER_VERSION
      || hdr.moh_byte_order != OUR_BYTE_ORDER
      || hdr.moh_data_rep_id != OUR_DATA_REP_ID
      || hdr.moh_cpu_type != OUR_CPU_TYPE
      || hdr.moh_cpu_subtype != OUR_CPU_SUBTYPE
      || hdr.moh_vendor_type != OUR_VENDOR_TYPE)
    {
      fatal ("incompatibilities between object file & expected values");
    }
#endif

  if (debug)
    print_header (&hdr);

  offset = hdr.moh_first_cmd_off;
  load_end = load_array
    = (load_all_t *) xcalloc (sizeof (load_all_t), hdr.moh_n_load_cmds + 2);

  /* Build array of load commands, calculating the offsets */
  for (i = 0; i < hdr.moh_n_load_cmds; i++)
    {
      load_union_t *load_hdr;		/* load command header */

      load_cmd = load_end++;
      load_hdr = (load_union_t *) (obj + offset);

      /* If modifying the program file, copy the header.  */
      if (rw)
	{
	  load_union_t *ptr = (load_union_t *) xmalloc (load_hdr->hdr.ldci_cmd_size);
	  memcpy ((char *)ptr, (char *)load_hdr, load_hdr->hdr.ldci_cmd_size);
	  load_hdr = ptr;

	  /* null out old command map, because we will rewrite at the end.  */
	  if (ptr->hdr.ldci_cmd_type == LDC_CMD_MAP)
	    {
	      cmd_strings = ptr->map.lcm_ld_cmd_strings;
	      ptr->hdr.ldci_cmd_type = LDC_UNDEFINED;
	    }
	}

      load_cmd->load = load_hdr;
      if (load_hdr->hdr.ldci_section_off > 0)
	load_cmd->section = obj + load_hdr->hdr.ldci_section_off;

      if (debug)
	print_load_command (load_hdr, offset, i);

      offset += load_hdr->hdr.ldci_cmd_size;
    }

  /* If the last command is the load command map and is not undefined,
     decrement the count of load commands.  */
  if (rw && load_end[-1].load->hdr.ldci_cmd_type == LDC_UNDEFINED)
    {
      load_end--;
      hdr.moh_n_load_cmds--;
    }

  /* Go through and process each symbol table section.  */
  symbol_load_cmds = 0;
  for (load_cmd = load_array; load_cmd < load_end; load_cmd++)
    {
      load_union_t *load_hdr = load_cmd->load;

      if (load_hdr->hdr.ldci_cmd_type == LDC_SYMBOLS)
	{
	  symbol_load_cmds++;

	  if (debug)
	    {
	      const char *kind = "unknown";

	      switch (load_hdr->sym.symc_kind)
		{
		case SYMC_IMPORTS:	   kind = "imports"; break;
		case SYMC_DEFINED_SYMBOLS: kind = "defined"; break;
		case SYMC_STABS:	   kind = "stabs";   break;
		}

	      notice ("\nProcessing symbol table #%d, offset = 0x%.8lx, kind = %s\n",
		      symbol_load_cmds, load_hdr->hdr.ldci_section_off, kind);
	    }

	  if (load_hdr->sym.symc_kind != SYMC_DEFINED_SYMBOLS)
	    continue;

	  str_sect = load_array[load_hdr->sym.symc_strings_section].section;
	  if (str_sect == (char *) 0)
	    fatal ("string section missing");

	  if (load_cmd->section == (char *) 0)
	    fatal ("section pointer missing");

	  num_syms = load_hdr->sym.symc_nentries;
	  for (i = 0; i < num_syms; i++)
	    {
	      symbol_info_t *sym = ((symbol_info_t *) load_cmd->section) + i;
	      char *name = sym->si_name.symbol_name + str_sect;

	      if (name[0] != '_')
		continue;

	      if (rw)
		{
		  char *n = name + strlen (name) - strlen (NAME__MAIN);

		  if ((n - name) < 0 || strcmp (n, NAME__MAIN))
		    continue;
		  while (n != name)
		    if (*--n != '_')
		      continue;

		  main_sym = sym;
		}
	      else
		{
		  switch (is_ctor_dtor (name))
		    {
		    case 1:
		      add_to_list (&constructors, name);
		      break;

		    case 2:
		      add_to_list (&destructors, name);
		      break;

		    default:	/* not a constructor or destructor */
		      continue;
		    }
		}

	      if (debug)
		fprintf (stderr, "\ttype = 0x%.4x, sc = 0x%.2x, flags = 0x%.8x, name = %.30s\n",
			 sym->si_type, sym->si_sc_type, sym->si_flags, name);
	    }
	}
    }

  if (symbol_load_cmds == 0)
    fatal ("no symbol table found");

  /* Update the program file now, rewrite header and load commands.  At present,
     we assume that there is enough space after the last load command to insert
     one more.  Since the first section written out is page aligned, and the
     number of load commands is small, this is ok for the present.  */

  if (rw)
    {
      load_union_t *load_map;
      size_t size;

      if (cmd_strings == -1)
	fatal ("no cmd_strings found");

      /* Add __main to initializer list.
	 If we are building a program instead of a shared library, do not
	 do anything, since in the current version, you cannot do mallocs
	 and such in the constructors.  */

      if (main_sym != (symbol_info_t *) 0
	  && ((hdr.moh_flags & MOH_EXECABLE_F) == 0))
	add_func_table (&hdr, load_array, main_sym, FNTC_INITIALIZATION);

      if (debug)
	notice ("\nUpdating header and load commands.\n\n");

      hdr.moh_n_load_cmds++;
      size = sizeof (load_cmd_map_command_t) + (sizeof (mo_offset_t) * (hdr.moh_n_load_cmds - 1));

      /* Create new load command map.  */
      if (debug)
	notice ("load command map, %d cmds, new size %ld.\n",
		(int) hdr.moh_n_load_cmds, (long) size);

      load_map = (load_union_t *) xcalloc (1, size);
      load_map->map.ldc_header.ldci_cmd_type = LDC_CMD_MAP;
      load_map->map.ldc_header.ldci_cmd_size = size;
      load_map->map.lcm_ld_cmd_strings = cmd_strings;
      load_map->map.lcm_nentries = hdr.moh_n_load_cmds;
      load_array[hdr.moh_n_load_cmds-1].load = load_map;

      offset = hdr.moh_first_cmd_off;
      for (i = 0; i < hdr.moh_n_load_cmds; i++)
	{
	  load_map->map.lcm_map[i] = offset;
	  if (load_array[i].load->hdr.ldci_cmd_type == LDC_CMD_MAP)
	    hdr.moh_load_map_cmd_off = offset;

	  offset += load_array[i].load->hdr.ldci_cmd_size;
	}

      hdr.moh_sizeofcmds = offset - MO_SIZEOF_RAW_HDR;

      if (debug)
	print_header (&hdr);

      /* Write header */
      status = encode_mach_o_hdr (&hdr, obj, MO_SIZEOF_RAW_HDR);
      if (status != MO_HDR_CONV_SUCCESS)
	bad_header (status);

      if (debug)
	notice ("writing load commands.\n\n");

      /* Write load commands */
      offset = hdr.moh_first_cmd_off;
      for (i = 0; i < hdr.moh_n_load_cmds; i++)
	{
	  load_union_t *load_hdr = load_array[i].load;
	  size_t size = load_hdr->hdr.ldci_cmd_size;

	  if (debug)
	    print_load_command (load_hdr, offset, i);

	  bcopy ((char *) load_hdr, (char *) (obj + offset), size);
	  offset += size;
	}
    }

  end_file (obj_file);

  if (close (prog_fd))
    fatal_perror ("close %s", prog_name);

  if (debug)
    fprintf (stderr, "\n");
}


/* Add a function table to the load commands to call a function
   on initiation or termination of the process.  */

static void
add_func_table (hdr_p, load_array, sym, type)
     mo_header_t *hdr_p;		/* pointer to global header */
     load_all_t *load_array;		/* array of ptrs to load cmds */
     symbol_info_t *sym;		/* pointer to symbol entry */
     int type;				/* fntc_type value */
{
  /* Add a new load command.  */
  int num_cmds = ++hdr_p->moh_n_load_cmds;
  int load_index = num_cmds - 1;
  size_t size = sizeof (func_table_command_t) + sizeof (mo_addr_t);
  load_union_t *ptr = xcalloc (1, size);
  load_all_t *load_cmd;
  int i;

  /* Set the unresolved address bit in the header to force the loader to be
     used, since kernel exec does not call the initialization functions.  */
  hdr_p->moh_flags |= MOH_UNRESOLVED_F;

  load_cmd = &load_array[load_index];
  load_cmd->load = ptr;
  load_cmd->section = (char *) 0;

  /* Fill in func table load command.  */
  ptr->func.ldc_header.ldci_cmd_type = LDC_FUNC_TABLE;
  ptr->func.ldc_header.ldci_cmd_size = size;
  ptr->func.ldc_header.ldci_section_off = 0;
  ptr->func.ldc_header.ldci_section_len = 0;
  ptr->func.fntc_type = type;
  ptr->func.fntc_nentries = 1;

  /* copy address, turn it from abs. address to (region,offset) if necessary.  */
  /* Is the symbol already expressed as (region, offset)?  */
  if ((sym->si_flags & SI_ABSOLUTE_VALUE_F) == 0)
    {
      ptr->func.fntc_entry_loc[i].adr_lcid = sym->si_value.def_val.adr_lcid;
      ptr->func.fntc_entry_loc[i].adr_sctoff = sym->si_value.def_val.adr_sctoff;
    }

  /* If not, figure out which region it's in.  */
  else
    {
      mo_vm_addr_t addr = sym->si_value.abs_val;
      int found = 0;

      for (i = 0; i < load_index; i++)
	{
	  if (load_array[i].load->hdr.ldci_cmd_type == LDC_REGION)
	    {
	      region_command_t *region_ptr = &load_array[i].load->region;

	      if ((region_ptr->regc_flags & REG_ABS_ADDR_F) != 0
		  && addr >= region_ptr->regc_addr.vm_addr
		  && addr <= region_ptr->regc_addr.vm_addr + region_ptr->regc_vm_size)
		{
		  ptr->func.fntc_entry_loc[0].adr_lcid = i;
		  ptr->func.fntc_entry_loc[0].adr_sctoff = addr - region_ptr->regc_addr.vm_addr;
		  found++;
		  break;
		}
	    }
	}

      if (!found)
	fatal ("could not convert 0x%l.8x into a region", addr);
    }

  if (debug)
    notice ("%s function, region %d, offset = %ld (0x%.8lx)\n",
	    type == FNTC_INITIALIZATION ? "init" : "term",
	    (int) ptr->func.fntc_entry_loc[i].adr_lcid,
	    (long) ptr->func.fntc_entry_loc[i].adr_sctoff,
	    (long) ptr->func.fntc_entry_loc[i].adr_sctoff);

}


/* Print the global header for an OSF/rose object.  */

static void
print_header (hdr_ptr)
     mo_header_t *hdr_ptr;
{
  fprintf (stderr, "\nglobal header:\n");
  fprintf (stderr, "\tmoh_magic            = 0x%.8lx\n", hdr_ptr->moh_magic);
  fprintf (stderr, "\tmoh_major_version    = %d\n", (int)hdr_ptr->moh_major_version);
  fprintf (stderr, "\tmoh_minor_version    = %d\n", (int)hdr_ptr->moh_minor_version);
  fprintf (stderr, "\tmoh_header_version   = %d\n", (int)hdr_ptr->moh_header_version);
  fprintf (stderr, "\tmoh_max_page_size    = %d\n", (int)hdr_ptr->moh_max_page_size);
  fprintf (stderr, "\tmoh_byte_order       = %d\n", (int)hdr_ptr->moh_byte_order);
  fprintf (stderr, "\tmoh_data_rep_id      = %d\n", (int)hdr_ptr->moh_data_rep_id);
  fprintf (stderr, "\tmoh_cpu_type         = %d\n", (int)hdr_ptr->moh_cpu_type);
  fprintf (stderr, "\tmoh_cpu_subtype      = %d\n", (int)hdr_ptr->moh_cpu_subtype);
  fprintf (stderr, "\tmoh_vendor_type      = %d\n", (int)hdr_ptr->moh_vendor_type);
  fprintf (stderr, "\tmoh_load_map_cmd_off = %d\n", (int)hdr_ptr->moh_load_map_cmd_off);
  fprintf (stderr, "\tmoh_first_cmd_off    = %d\n", (int)hdr_ptr->moh_first_cmd_off);
  fprintf (stderr, "\tmoh_sizeofcmds       = %d\n", (int)hdr_ptr->moh_sizeofcmds);
  fprintf (stderr, "\tmon_n_load_cmds      = %d\n", (int)hdr_ptr->moh_n_load_cmds);
  fprintf (stderr, "\tmoh_flags            = 0x%.8lx", (long)hdr_ptr->moh_flags);

  if (hdr_ptr->moh_flags & MOH_RELOCATABLE_F)
    fprintf (stderr, ", relocatable");

  if (hdr_ptr->moh_flags & MOH_LINKABLE_F)
    fprintf (stderr, ", linkable");

  if (hdr_ptr->moh_flags & MOH_EXECABLE_F)
    fprintf (stderr, ", execable");

  if (hdr_ptr->moh_flags & MOH_EXECUTABLE_F)
    fprintf (stderr, ", executable");

  if (hdr_ptr->moh_flags & MOH_UNRESOLVED_F)
    fprintf (stderr, ", unresolved");

  fprintf (stderr, "\n\n");
  return;
}


/* Print a short summary of a load command.  */

static void
print_load_command (load_hdr, offset, number)
     load_union_t *load_hdr;
     size_t offset;
     int number;
{
  mo_long_t type = load_hdr->hdr.ldci_cmd_type;
  const char *type_str = (char *) 0;

  switch (type)
    {
    case LDC_UNDEFINED:   type_str = "UNDEFINED";	break;
    case LDC_CMD_MAP:	  type_str = "CMD_MAP";		break;
    case LDC_INTERPRETER: type_str = "INTERPRETER";	break;
    case LDC_STRINGS:	  type_str = "STRINGS";		break;
    case LDC_REGION:	  type_str = "REGION";		break;
    case LDC_RELOC:	  type_str = "RELOC";		break;
    case LDC_PACKAGE:	  type_str = "PACKAGE";		break;
    case LDC_SYMBOLS:	  type_str = "SYMBOLS";		break;
    case LDC_ENTRY:	  type_str = "ENTRY";		break;
    case LDC_FUNC_TABLE:  type_str = "FUNC_TABLE";	break;
    case LDC_GEN_INFO:	  type_str = "GEN_INFO";	break;
    }

  fprintf (stderr,
	   "cmd %2d, sz: 0x%.2lx, coff: 0x%.3lx, doff: 0x%.6lx, dlen: 0x%.6lx",
	   number,
	   (long) load_hdr->hdr.ldci_cmd_size,
	   (long) offset,
	   (long) load_hdr->hdr.ldci_section_off,
	   (long) load_hdr->hdr.ldci_section_len);

  if (type_str == (char *) 0)
    fprintf (stderr, ", ty: unknown (%ld)\n", (long) type);

  else if (type != LDC_REGION)
    fprintf (stderr, ", ty: %s\n", type_str);

  else
    {
      const char *region = "";
      switch (load_hdr->region.regc_usage_type)
	{
	case REG_TEXT_T:	region = ", .text";	break;
	case REG_DATA_T:	region = ", .data";	break;
	case REG_BSS_T:		region = ", .bss";	break;
	case REG_GLUE_T:	region = ", .glue";	break;
#if defined (REG_RDATA_T) && defined (REG_SDATA_T) && defined (REG_SBSS_T) /*mips*/
	case REG_RDATA_T:	region = ", .rdata";	break;
	case REG_SDATA_T:	region = ", .sdata";	break;
	case REG_SBSS_T:	region = ", .sbss";	break;
#endif
	}

      fprintf (stderr, ", ty: %s, vaddr: 0x%.8lx, vlen: 0x%.6lx%s\n",
	       type_str,
	       (long) load_hdr->region.regc_vm_addr,
	       (long) load_hdr->region.regc_vm_size,
	       region);
    }

  return;
}


/* Fatal error when {en,de}code_mach_o_header fails.  */

static void
bad_header (status)
     int status;
{
  switch (status)
    {
    case MO_ERROR_BAD_MAGIC:		fatal ("bad magic number");
    case MO_ERROR_BAD_HDR_VERS:		fatal ("bad header version");
    case MO_ERROR_BAD_RAW_HDR_VERS:	fatal ("bad raw header version");
    case MO_ERROR_BUF2SML:		fatal ("raw header buffer too small");
    case MO_ERROR_OLD_RAW_HDR_FILE:	fatal ("old raw header file");
    case MO_ERROR_UNSUPPORTED_VERS:	fatal ("unsupported version");
    default:
      fatal ("unknown {de,en}code_mach_o_hdr return value %d", status);
    }
}


/* Read a file into a memory buffer.  */

static struct file_info *
read_file (name, fd, rw)
     const char *name;		/* filename */
     int fd;			/* file descriptor */
     int rw;			/* read/write */
{
  struct stat stat_pkt;
  struct file_info *p = (struct file_info *) xcalloc (sizeof (struct file_info), 1);
#ifdef USE_MMAP
  static int page_size;
#endif

  if (fstat (fd, &stat_pkt) < 0)
    fatal_perror ("fstat %s", name);

  p->name	  = name;
  p->size	  = stat_pkt.st_size;
  p->rounded_size = stat_pkt.st_size;
  p->fd		  = fd;
  p->rw		  = rw;

#ifdef USE_MMAP
  if (debug)
    fprintf (stderr, "mmap %s, %s\n", name, (rw) ? "read/write" : "read-only");

  if (page_size == 0)
    page_size = sysconf (_SC_PAGE_SIZE);

  p->rounded_size = ((p->size + page_size - 1) / page_size) * page_size;
  p->start = mmap ((caddr_t) 0,
		   (rw) ? p->rounded_size : p->size,
		   (rw) ? (PROT_READ | PROT_WRITE) : PROT_READ,
		   MAP_FILE | MAP_VARIABLE | MAP_SHARED,
		   fd,
		   0L);

  if (p->start != (char *) 0 && p->start != (char *) -1)
    p->use_mmap = 1;

  else
#endif /* USE_MMAP */
    {
      long len;

      if (debug)
	fprintf (stderr, "read %s\n", name);

      p->use_mmap = 0;
      p->start = xmalloc (p->size);
      if (lseek (fd, 0L, SEEK_SET) < 0)
	fatal_perror ("lseek %s 0", name);

      len = read (fd, p->start, p->size);
      if (len < 0)
	fatal_perror ("read %s", name);

      if (len != p->size)
	fatal ("read %ld bytes, expected %ld, from %s", len, p->size, name);
    }

  return p;
}

/* Do anything necessary to write a file back from memory.  */

static void
end_file (ptr)
     struct file_info *ptr;	/* file information block */
{
#ifdef USE_MMAP
  if (ptr->use_mmap)
    {
      if (ptr->rw)
	{
	  if (debug)
	    fprintf (stderr, "msync %s\n", ptr->name);

	  if (msync (ptr->start, ptr->rounded_size, MS_ASYNC))
	    fatal_perror ("msync %s", ptr->name);
	}

      if (debug)
	fprintf (stderr, "munmap %s\n", ptr->name);

      if (munmap (ptr->start, ptr->size))
	fatal_perror ("munmap %s", ptr->name);
    }
  else
#endif /* USE_MMAP */
    {
      if (ptr->rw)
	{
	  long len;

	  if (debug)
	    fprintf (stderr, "write %s\n", ptr->name);

	  if (lseek (ptr->fd, 0L, SEEK_SET) < 0)
	    fatal_perror ("lseek %s 0", ptr->name);

	  len = write (ptr->fd, ptr->start, ptr->size);
	  if (len < 0)
	    fatal_perror ("write %s", ptr->name);

	  if (len != ptr->size)
	    fatal ("wrote %ld bytes, expected %ld, to %s", len, ptr->size, ptr->name);
	}

      free (ptr->start);
    }

  free (ptr);
}

#endif /* OBJECT_FORMAT_ROSE */
