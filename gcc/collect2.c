/* Collect static initialization info into data structures
   that can be traversed by C++ initialization and finalization
   routines.

   Copyright (C) 1992 Free Software Foundation, Inc.
   Contributed by Chris Smith (csmith@convex.com).
   Heavily modified by Michael Meissner (meissner@osf.org),
   Per Bothner (bothner@cygnus.com), and John Gilmore (gnu@cygnus.com).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Build tables of static constructors and destructors and run ld. */

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "gstddef.h"
#include <errno.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define COLLECT

#include "config.h"

#ifndef __STDC__
#include "gvarargs.h"
#define generic char
#define PROTO(x) ()
#define const

#else
#include "stdarg.h"
#define generic void
#define PROTO(x) x
#endif

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

/* Default flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-p"
#endif

#ifdef USG
#define vfork fork
#endif

/* On MSDOS, write temp files in current dir
   because there's no place else we can expect to use.  */
#if __MSDOS__
#ifndef P_tmpdir
#define P_tmpdir "./"
#endif
#endif


/* Linked lists of constructor and destructor names. */

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
  PASS_SECOND				/* with constructors linked in */
};

extern char *sys_siglist[];
extern char *version_string;

static int vflag;			/* true if -v */
static int rflag;			/* true if -r */

static int debug;			/* true if -debug */

static int   temp_filename_length;	/* Length of temp_filename */
static char *temp_filename;		/* Base of temp filenames */
static char *c_file;			/* <xxx>.c for constructor/destructor list. */
static char *o_file;			/* <xxx>.o for constructor/destructor list. */
static char *nm_file_name;		/* pathname of nm */

static struct head constructors;	/* list of constructors found */
static struct head destructors;		/* list of destructors found */

extern char *getenv		PROTO((	const char * ));
extern char *mktemp		PROTO((	char * ));
extern int   vfork		PROTO(( void ));
static void  add_to_list	PROTO((	struct head *headp, char *name ));
static void  scan_prog_file	PROTO((	char *, enum pass ));
static void  fork_execute	PROTO((	char *, char **argv ));
static void  do_wait		PROTO((	char * ));
static void  write_c_file	PROTO((	FILE *, char * ));
static void  my_exit		PROTO(( int ));
static void  handler		PROTO(( int ));
static void  maybe_unlink	PROTO(( char * ));
static void  choose_temp_base	PROTO(( void ));

generic		*xcalloc	PROTO((	size_t, size_t ));
generic		*xmalloc	PROTO((	size_t ));



#if !defined(HAVE_STRERROR) && !defined(_OSF_SOURCE)

char *strerror (e)
     int e;
{
  extern char *sys_errlist[];
  extern int sys_nerr;
  static char buffer[30];

  if (!e)
    return "";

  if (e > 0 && e < sys_nerr)
    return sys_errlist[e];

  sprintf (buffer, "Unknown error %d", e);
  return buffer;
}

#endif


/* Delete tempfiles and exit function.  */

static void
my_exit (status)
     int status;
{
  if (c_file[0])
    maybe_unlink (c_file);

  if (o_file[0])
    maybe_unlink (o_file);

  exit (status);
}


#ifndef __STDC__

/* Die when sys call fails. */

/*VARARGS*/
static void
fatal_perror (va_alist)
{
  char *string;
  va_list vptr;
  int e = errno;

  va_start (vptr);
  string = va_arg (vptr, char *);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, ": %s\n", strerror (e));
  va_end (vptr);
  my_exit (1);
}

/* Just die. */

/*VARARGS*/
static void
fatal (va_alist)
{
  char *string;
  va_list vptr;

  va_start (vptr);
  string = va_arg (vptr, char *);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, "\n");
  va_end (vptr);
  my_exit (1);
}

/* Write error message.  */

/*VARARGS*/
static void
error (va_alist)
{
  char *string;
  va_list vptr;

  va_start (vptr);
  string = va_arg (vptr, char *);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, "\n");
  va_end (vptr);
}

#else

static void
fatal_perror (char *string, ...)
{
  va_list vptr;
  int e = errno;

  va_start (vptr, string);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, ": %s\n", strerror (e));
  va_end (vptr);
  my_exit (1);
}

/* Just die. */

static void
fatal (char *string, ...)
{
  va_list vptr;

  va_start (vptr, string);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, "\n");
  va_end (vptr);
  my_exit (1);
}

/* Write error message.  */

static void
error (char *string, ...)
{
  va_list vptr;

  va_start (vptr, string);
  fprintf (stderr, "collect: ");
  vfprintf (stderr, string, vptr);
  fprintf (stderr, "\n");
  va_end (vptr);
}
#endif


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
  if (c_file[0])
    maybe_unlink (c_file);

  if (o_file[0])
    maybe_unlink (o_file);

  signal (signo, SIG_DFL);

  fatal ("Caught signal %d [%s]", signo, sys_siglist[signo]);
  kill (getpid (), signo);
}


generic *
xcalloc (size1, size2)
     size_t size1, size2;
{
  generic *ptr = calloc (size1, size2);
  if (ptr)
    return ptr;

  fatal ("Out of memory.");
  return (generic *)0;
}

generic *
xmalloc (size)
     size_t size;
{
  generic *ptr = malloc (size);
  if (ptr)
    return ptr;

  fatal ("Out of memory.");
  return (generic *)0;
}


/* Compute a string to use as the base of all temporary file names.
   It is substituted for %g.  */

static void
choose_temp_base PROTO((void))
{
  char *base = getenv ("TMPDIR");
  int len;

  if (base == (char *)0)
    {
#ifdef P_tmpdir
      if (access (P_tmpdir, R_OK | W_OK) == 0)
	base = P_tmpdir;
#endif
      if (base == (char *)0)
	{
	  if (access ("/usr/tmp", R_OK | W_OK) == 0)
	    base = "/usr/tmp/";
	  else
	    base = "/tmp/";
	}
    }

  len = strlen (base);
  temp_filename = xmalloc (len + sizeof("/ccXXXXXX"));
  strcpy (temp_filename, base);
  if (len > 0 && temp_filename[len-1] != '/')
    temp_filename[len++] = '/';
  strcpy (temp_filename + len, "ccXXXXXX");

  mktemp (temp_filename);
  temp_filename_length = strlen (temp_filename);
}


/* Main program. */

int
main (argc, argv)
     int argc;
     char *argv[];
{
  char *outfile		= "a.out";
  char *arg;
  FILE *outf;
  char *ld_file_name;
  char *c_file_name;
  char *B_option;
  char *p;
  char *prefix;
  char **c_argv		= (char **) xcalloc (sizeof (char *), argc+7);
  char **c_ptr		= c_argv;
  char **ld1_argv	= (char **) xcalloc (sizeof (char *), argc+2);
  char **ld1		= ld1_argv;
  char **ld2_argv	= (char **) xcalloc (sizeof (char *), argc+5);
  char **ld2		= ld2_argv;
  int first_file;
  int len;
  int clen;

#ifdef DEBUG
  debug = 1;
  vflag = 1;
#endif

  if (argc < 2)
    fatal ("no arguments");

  signal (SIGQUIT, handler);
  signal (SIGINT,  handler);
  signal (SIGALRM, handler);
  signal (SIGHUP,  handler);
  signal (SIGSEGV, handler);
  signal (SIGBUS,  handler);

  /* Try to discover a valid linker/assembler/nm to use.  */
  len = strlen (argv[0]);
  prefix = (char *)0;
  if (len >= sizeof ("ld")-1)
    {
      p = argv[0] + len - sizeof ("ld") + 1;
      if (strcmp (p, "ld") == 0)
	{
	  prefix = argv[0];
	  *p = '\0';
	}
    }

  if (prefix == (char *)0)
    {
      p = strrchr (argv[0], '/');
      if (p != (char *)0)
	{
	  prefix = argv[0];
	  p[1] = '\0';
	}

#ifdef STANDARD_EXEC_PREFIX
      else if (access (STANDARD_EXEC_PREFIX, X_OK) == 0)
	prefix = STANDARD_EXEC_PREFIX;
#endif

#ifdef MD_EXEC_PREFIX
      else if (access (MD_EXEC_PREFIX, X_OK) == 0)
	prefix = MD_EXEC_PREFIX;
#endif

      else if (access ("/usr/ccs/gcc", X_OK) == 0)
	prefix = "/usr/ccs/gcc/";

      else if (access ("/usr/ccs/bin", X_OK) == 0)
	prefix = "/usr/ccs/bin/";

      else
	prefix = "/bin/";
    }

  clen = len = strlen (prefix);

#ifdef STANDARD_BIN_PREFIX
  if (clen < sizeof (STANDARD_BIN_PREFIX) - 1)
    clen = sizeof (STANDARD_BIN_PREFIX) - 1;
#endif

  ld_file_name = xcalloc (len + sizeof ("real-ld"), 1);
  c_file_name  = xcalloc (clen + sizeof ("gcc"), 1);
  nm_file_name = xcalloc (len + sizeof ("gnm"), 1);
  B_option     = xcalloc (len + sizeof ("-B"), 1);

  memcpy (ld_file_name, prefix, len);
  strcpy (ld_file_name + len, "real-ld");
  if (access (ld_file_name, X_OK) < 0)
    {
      strcpy (ld_file_name + len, "gld");
      if (access (ld_file_name, X_OK) < 0)
	{
	  free (ld_file_name);
#ifdef REAL_LD_FILE_NAME
	  ld_file_name = REAL_LD_FILE_NAME;
#else
	  ld_file_name = (access ("/usr/bin/ld", X_OK) == 0) ? "/usr/bin/ld" : "/bin/ld";
#endif
	}
    }

  memcpy (c_file_name, prefix, len);
  strcpy (c_file_name + len, "gcc");
  if (access (c_file_name, X_OK) < 0)
    {
#ifdef STANDARD_BIN_PREFIX
      strcpy (c_file_name, STANDARD_BIN_PREFIX);
      strcat (c_file_name, "gcc");
      if (access (c_file_name, X_OK) < 0)
#endif
	{
#ifdef STANDARD_EXEC_PREFIX
	  strcpy (c_file_name, STANDARD_EXEC_PREFIX);
	  strcat (c_file_name, "gcc");
	  if (access (c_file_name, X_OK) < 0)
#endif
	    {
	      strcpy (c_file_name, "gcc");
	    }
	}
    }

  memcpy (nm_file_name, prefix, len);
  strcpy (nm_file_name + len, "nm");
  if (access (nm_file_name, X_OK) < 0)
    {
      strcpy (nm_file_name + len, "gnm");
      if (access (nm_file_name, X_OK) < 0)
	{
	  free (nm_file_name);
#ifdef REAL_NM_FILE_NAME
	  nm_file_name = REAL_NM_FILE_NAME;
#else
	  nm_file_name = (access ("/usr/bin/nm", X_OK) == 0) ? "/usr/bin/nm" : "/bin/nm";
#endif
	}
    }

  strcpy (B_option, "-B");
  strcpy (B_option + sizeof ("-B") - 1, prefix);

  *ld1++ = *ld2++ = "ld";

  /* Make temp file names. */
  choose_temp_base ();
  c_file = xcalloc (temp_filename_length + sizeof (".c"), 1);
  o_file = xcalloc (temp_filename_length + sizeof (".o"), 1);
  sprintf (c_file, "%s.c", temp_filename);
  sprintf (o_file, "%s.o", temp_filename);
  *c_ptr++ = "gcc";
  *c_ptr++ = "-c";
  *c_ptr++ = "-o";
  *c_ptr++ = o_file;

  /* Parse arguments.  Remember output file spec, pass the rest to ld. */
  /* After the first file, put in the c++ rt0 */
  first_file = 1;
  while ((arg = *++argv) != (char *)0)
    {
      *ld1++ = *ld2++ = arg;

      if (arg[0] == '-')
	  switch (arg[1])
	    {
	    case 'd':
	      if (!strcmp (arg, "-debug"))
		{
		  debug = 1;
		  vflag = 1;
		  ld1--;
		  ld2--;
		}
	      break;

	      /* pass -f<xxx>, -B<xxx>, -b<xxx>, -V<xxx>, and -m<xxx>
		 options to gcc.  This allows options to be passed
		 that affect search rules, and the size of pointers. */
	    case 'b':
	    case 'B':
	    case 'f':
	    case 'm':
	    case 'V':
	      if (arg[1] != '\0')
		{
		  ld1--;
		  ld2--;
		  *c_ptr++ = arg;
		}
	      break;

	    case 'o':
	      outfile = (arg[2] == '\0') ? argv[1] : &arg[2];
	      break;

	    case 'r':
	      if (arg[2] == '\0')
		rflag = 1;
	      break;

	    case 'v':
	      if (arg[2] == '\0')
		vflag = 1;
	      break;
	    }

      else if (first_file
	       && (p = strrchr (arg, '.')) != (char *)0
	       && strcmp (p, ".o") == 0)
	{
	  first_file = 0;
	  *ld2++ = o_file;
	}
    }

  *c_ptr++ = B_option;
  *c_ptr++ = c_file;
  *c_ptr = *ld1 = *ld2 = (char *)0;

  if (vflag)
    {
      fprintf (stderr, "GNU COLLECT2 version %s", version_string);
#ifdef TARGET_VERSION
      TARGET_VERSION;
#endif
      fprintf (stderr, "\n");
    }

  if (debug)
    {
      fprintf (stderr, "prefix       = %s\n", prefix);
      fprintf (stderr, "ld_file_name = %s\n", ld_file_name);
      fprintf (stderr, "c_file_name  = %s\n", c_file_name);
      fprintf (stderr, "nm_file_name = %s\n", nm_file_name);
      fprintf (stderr, "B_option     = %s\n", B_option);
      fprintf (stderr, "c_file       = %s\n", c_file);
      fprintf (stderr, "o_file       = %s\n", o_file);
    }

  /* Load the program, searching all libraries.
     Examine the namelist with nm and search it for static constructors
     and destructors to call.
     Write the constructor and destructor tables to a .s file and reload. */

  fork_execute (ld_file_name, ld1_argv);

  /* If -r, don't build the constructor or destructor list, just return now.  */
  if (rflag)
    return 0;

  scan_prog_file (outfile, PASS_FIRST);

  if (debug)
    {
      fprintf (stderr, "%d constructor(s) found\n", constructors.number);
      fprintf (stderr, "%d destructor(s)  found\n", destructors.number);
    }

  if (constructors.number == 0 && destructors.number == 0)
    return 0;

  outf = fopen (c_file, "w");
  if (outf == (FILE *)0)
    fatal_perror ("Can't write %s", c_file);

  write_c_file (outf, c_file);

  if (fclose (outf))
    fatal_perror ("Can't close %s", c_file);

  if (debug)
    {
      fprintf (stderr, "\n========== outfile = %s, c_file = %s\n", outfile, c_file);
      write_c_file (stderr, "stderr");
      fprintf (stderr, "========== end of c_file\n\n");
    }

  /* Assemble the constructor and destructor tables.
     Link the tables in with the rest of the program. */

  fork_execute (c_file_name,  c_argv);
  fork_execute (ld_file_name, ld2_argv);

  /* Let scan_prog_file do any final mods (OSF/rose needs this for
     constructors/destructors in shared libraries.  */
  scan_prog_file (outfile, PASS_SECOND);

  maybe_unlink (c_file);
  maybe_unlink (o_file);
  return 0;
}


/* Wait for a process to finish, and exit if a non-zero status is found. */

static void
do_wait (prog)
     char *prog;
{
  int status;

  wait (&status);
  if (status)
    {
      int sig = WTERMSIG (status);
      int ret;

      if (sig != -1 && sig != 0)
	{
	  error ("%s terminated with signal %d [%s]%s",
		 prog,
		 sig,
		 sys_siglist[sig],
		 (status & 0200) ? ", core dumped" : "");

	  my_exit (127);
	}

      ret = WEXITSTATUS (status);
      if (ret != -1 && ret != 0)
	{
	  error ("%s returned %d exit status", prog, ret);
	  my_exit (ret);
	}
    }
}


/* Fork and execute a program, and wait for the reply.  */

static void
fork_execute (prog, argv)
     char *prog;
     char **argv;
{
  int pid;
  void (*int_handler) PROTO((int));
  void (*quit_handler) PROTO((int));

  if (vflag || debug)
    {
      char **p_argv;
      char *str;

      fprintf (stderr, "%s", prog);
      for (p_argv = &argv[1]; (str = *p_argv) != (char *)0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pid = vfork ();
  if (pid == -1)
    fatal_perror ("vfork");

  if (pid == 0)			/* child context */
    {
      execvp (prog, argv);
      fatal_perror ("Execute %s", prog);
    }

  int_handler  = (void (*)PROTO((int)))signal (SIGINT,  SIG_IGN);
  quit_handler = (void (*)PROTO((int)))signal (SIGQUIT, SIG_IGN);

  do_wait (prog);

  signal (SIGINT,  int_handler);
  signal (SIGQUIT, quit_handler);
}


/* Unlink a file unless we are debugging.  */

static void
maybe_unlink (file)
     char *file;
{
  if (!debug)
    unlink (file);
  else
    fprintf (stderr, "[Leaving %s]\n", file);
}


/* Add a name to a linked list.  */

static void
add_to_list (head_ptr, name)
     struct head *head_ptr;
     char *name;
{
  struct id *newid = (struct id *) xcalloc (sizeof (*newid) + strlen (name), 1);
  static long sequence_number = 0;
  newid->sequence = ++sequence_number;
  strcpy (newid->name, name);

  if (head_ptr->first)
    head_ptr->last->next = newid;
  else
    head_ptr->first = newid;

  head_ptr->last = newid;
  head_ptr->number++;
}

/* Write: `prefix', the names on list LIST, `suffix'.  */

static void
write_list (stream, prefix, list)
     FILE *stream;
     char *prefix;
     struct id *list;
{
  while (list)
    {
      fprintf (stream, "%sx%d,\n", prefix, list->sequence);
      list = list->next;
    }
}

static void
write_list_with_asm (stream, prefix, list)
     FILE *stream;
     char *prefix;
     struct id *list;
{
  while (list)
    {
      fprintf (stream, "%sx%d asm (\"%s\");\n",
	       prefix, list->sequence, list->name);
      list = list->next;
    }
}

/* Write the constructor/destructor tables. */

static void
write_c_file (stream, name)
     FILE *stream;
     char *name;
{
  /* Write the tables as C code  */

  fprintf (stream, "typedef void entry_pt();\n\n");
    
  write_list_with_asm (stream, "entry_pt ", constructors);
    
  fprintf (stream, "\nentry_pt * __CTOR_LIST__[] = {\n");
  fprintf (stream, "\t(entry_pt *) %d,\n", constructors.number);
  write_list (stream, "\t", constructors);
  fprintf (stream, "\t0\n};\n\n");

  write_list_with_asm (stream, "entry_pt ", destructors);

  fprintf (stream, "\nentry_pt * __DTOR_LIST__[] = {\n");
  fprintf (stream, "\t(entry_pt *) %d,\n", destructors.number);
  write_list (stream, "\t", destructors);
  fprintf (stream, "\t0\n};\n\n");

  fprintf (stream, "extern entry_pt __main;\n");
  fprintf (stream, "entry_pt *__main_reference = __main;\n\n");
}


#ifndef OBJECT_FORMAT_ROSE

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
     char *prog_name;
     enum pass which_pass;
{
  void (*int_handler) PROTO((int));
  void (*quit_handler) PROTO((int));
  char *nm_argv[4];
  int pid;
  int argc = 0;
  int pipe_fd[2];
  char *p, buf[1024];
  FILE *inf;

  if (which_pass != PASS_FIRST)
    return;

  nm_argv[ argc++ ] = "nm";
  if (NM_FLAGS[0] != '\0')
    nm_argv[ argc++ ] = NM_FLAGS;

  nm_argv[ argc++ ] = prog_name;
  nm_argv[ argc++ ] = (char *)0;

  if (pipe (pipe_fd) < 0)
    fatal_perror ("pipe");

  inf = fdopen (pipe_fd[0], "r");
  if (inf == (FILE *)0)
    fatal_perror ("fdopen");

  /* Trace if needed.  */
  if (vflag)
    {
      char **p_argv;
      char *str;

      fprintf (stderr, "%s", nm_file_name);
      for (p_argv = &nm_argv[1]; (str = *p_argv) != (char *)0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  /* Spawn child nm on pipe */
  pid = vfork ();
  if (pid == -1)
    fatal_perror ("vfork");

  if (pid == 0)			/* child context */
    {
      /* setup stdout */
      if (dup2 (pipe_fd[1], 1) < 0)
	fatal_perror ("Dup2 (%d, 1)", pipe_fd[1]);

      if (close (pipe_fd[0]) < 0)
	fatal_perror ("Close (%d)", pipe_fd[0]);

      if (close (pipe_fd[1]) < 0)
	fatal_perror ("Close (%d)", pipe_fd[1]);

      execv (nm_file_name, nm_argv);
      fatal_perror ("Execute %s", nm_file_name);
    }

  /* Parent context from here on.  */
  int_handler  = (void (*)PROTO((int)))signal (SIGINT,  SIG_IGN);
  quit_handler = (void (*)PROTO((int)))signal (SIGQUIT, SIG_IGN);

  if (close (pipe_fd[1]) < 0)
    fatal_perror ("Close (%d)", pipe_fd[1]);

  if (debug)
    fprintf (stderr, "\nnm output with constructors/destructors.\n");

  /* Read each line of nm output.  */
  while (fgets (buf, sizeof buf, inf) != (char *)0)
    {
      int ch, ch2;
      char *start;
      char *end;

      /* If it contains a constructor or destructor name, add the name
	 to the appropriate list. */

      for (p = buf; (ch = *p) != '\0' && ch != '\n' && ch != '_'; p++)
	;

      if (ch == '\0' || ch == '\n')
	continue;

      start = p;
      while ((ch = *p) == '_')	/* skip any extra '_' inserted */
	p++;

      for (end = p; (ch2 = *end) != '\0' && !isspace (ch2); end++)
	;

      *end = '\0';
      if (ch == 'G')
	{
	  if (! strncmp (p, "GLOBAL_$I$", 10))
	    add_to_list (&constructors, p-1);

	  else if (! strncmp (p, "GLOBAL_$D$", 10))
	    add_to_list (&destructors, p-1);

	  else				/* not a constructor or destructor */
	    continue;
	}

      else if (ch == 's' && (p - start) >= 2)
	{
	  if (! strncmp (p, "sti__", 5))
	    add_to_list (&constructors, p-2);

	  else if (! strncmp (p, "std__", 5))
	    add_to_list (&destructors, p-2);

	  else				/* not a constructor or destructor */
	    continue;
	}

      else
	continue;

      if (debug)
	fprintf (stderr, "\t%s\n", buf);
    }

  if (debug)
    fprintf (stderr, "\n");

  if (fclose (inf) != 0)
    fatal_perror ("fclose of pipe");

  do_wait (nm_file_name);

  signal (SIGINT,  int_handler);
  signal (SIGQUIT, quit_handler);
}

#endif /* !OBJECT_FORMAT_ROSE */


/*
 * OSF/rose specific stuff.
 */

#ifdef OBJECT_FORMAT_ROSE

/* Union of the various load commands */

typedef union load_union
{
  ldc_header_t			hdr;	/* common header */
  load_cmd_map_command_t	map;	/* map indexing other load cmds */
  interpreter_command_t		iprtr;	/* interpereter pathname */
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

extern int decode_mach_o_hdr		PROTO((	void *in_bufp,
						size_t in_bufsize,
						unsigned long hdr_version,
						mo_header_t *headerp ));

extern int encode_mach_o_hdr		PROTO((	mo_header_t *headerp,
						void *out_bufp,
						size_t out_bufsize ));

static void bad_header			PROTO(( int status ));

static void print_header		PROTO(( mo_header_t *hdr_ptr ));

static void print_load_command		PROTO(( load_union_t *load_hdr,
					        size_t offset,
					        int number ));

static void add_func_table		PROTO(( mo_header_t *hdr_p,
					        load_all_t *load_array,
					        symbol_info_t *sym,
					        int type ));

static struct file_info	*read_file	PROTO((	char *, int, int ));

static void end_file			PROTO((	struct file_info * ));


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
     char *prog_name;
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
    fatal_perror ("Can't read %s", prog_name);

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
      fatal ("incompatibilities exist between object file & expected values.");
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

      /* If modifing the program file, copy the header.  */
      if (rw)
	{
	  load_union_t *ptr = (load_union_t *) xmalloc (load_hdr->hdr.ldci_cmd_size);
	  memcpy (ptr, load_hdr, load_hdr->hdr.ldci_cmd_size);
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
	      char *kind = "uknown";

	      switch (load_hdr->sym.symc_kind)
		{
		case SYMC_IMPORTS:	   kind = "imports"; break;
		case SYMC_DEFINED_SYMBOLS: kind = "defined"; break;
		case SYMC_STABS:	   kind = "stabs";   break;
		}

	      fprintf (stderr, "\nProcessing symbol table #%d, offset = 0x%.8lx, kind = %s\n",
		       symbol_load_cmds, load_hdr->hdr.ldci_section_off, kind);
	    }

	  if (load_hdr->sym.symc_kind != SYMC_DEFINED_SYMBOLS)
	    continue;

	  str_sect = load_array[ load_hdr->sym.symc_strings_section ].section;
	  if (str_sect == (char *)0)
	    fatal ("string section missing");

	  if (load_cmd->section == (char *)0)
	    fatal ("section pointer missing");

	  num_syms = load_hdr->sym.symc_nentries;
	  for (i = 0; i < num_syms; i++)
	    {
	      symbol_info_t *sym = ((symbol_info_t *) load_cmd->section) + i;
	      char *name = sym->si_name.symbol_name + str_sect;
	      char *name_start = name;

	      if (name[0] != '_')
		continue;

	      while (*++name == '_')	/* skip any extra '_' inserted */
		;

	      if (rw)
		{
		  if (*name != 'm' || (name - name_start) < 2
		      || strcmp (name, "main"))
		    continue;

		  main_sym = sym;
		}

	      else if (*name == 'G')
		{
		  if (! strncmp (name, "GLOBAL_$I$", 10))
		    add_to_list (&constructors, name_start);

		  else if (! strncmp (name, "GLOBAL_$D$", 10))
		    add_to_list (&destructors, name_start);

		  else		/* not a constructor or destructor */
		    continue;
		}

	      else if (*name == 's' && (name - name_start) > 2)
		{
		  if (! strncmp (name, "sti__", 5))
		    add_to_list (&constructors, name_start);

		  else if (! strncmp (name, "std__", 5))
		    add_to_list (&destructors, name_start);

		  else		/* not a constructor or destructor */
		    continue;
		}

	      else
		continue;

	      if (debug)
		fprintf (stderr, "\ttype = 0x%.4x, sc = 0x%.2x, flags = 0x%.8x, name = %.30s\n",
			 sym->si_type, sym->si_sc_type, sym->si_flags, name);
	    }
	}
    }

  if (symbol_load_cmds == 0)
    fatal ("no symbol table found.");

  /* Update the program file now, rewrite header and load commands.  At present,
     we assume that there is enough space after the last load command to insert
     one more.  Since the first section written out is page aligned, and the
     number of load commands is small, this is ok for the present.  */

  if (rw)
    {
      load_union_t *load_map;
      size_t size;

      if (cmd_strings == -1)
	fatal ("no cmd_strings found.");

      /* Add __main to initializer list.  */
      if (main_sym != (symbol_info_t *)0)
	add_func_table (&hdr, load_array, main_sym, FNTC_INITIALIZATION);

      if (debug)
	fprintf (stderr, "\nUpdating header and load commands.\n\n");

      hdr.moh_n_load_cmds++;
      size = sizeof (load_cmd_map_command_t) + (sizeof (mo_offset_t) * (hdr.moh_n_load_cmds - 1));

      /* Create new load command map.  */
      if (debug)
	fprintf (stderr, "load command map, %d cmds, new size %ld.\n",
		 (int)hdr.moh_n_load_cmds, (long)size);

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
	fprintf (stderr, "writing load commands.\n\n");

      /* Write load commands */
      offset = hdr.moh_first_cmd_off;
      for (i = 0; i < hdr.moh_n_load_cmds; i++)
	{
	  load_union_t *load_hdr = load_array[i].load;
	  size_t size = load_hdr->hdr.ldci_cmd_size;

	  if (debug)
	    print_load_command (load_hdr, offset, i);

	  memcpy (obj + offset, load_hdr, size);
	  offset += size;
	}
    }

  end_file (obj_file);

  if (close (prog_fd))
    fatal_perror ("Can't close %s", prog_name);

  if (debug)
    fprintf (stderr, "\n");
}


/* Add a function table to the load commands to call a function
   on initition or termination of the process.  */

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
  load_cmd->section = (char *)0;

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
    fprintf (stderr,
	     "%s function, region %d, offset = %ld (0x%.8lx)\n",
	     (type == FNTC_INITIALIZATION) ? "init" : "term",
	     (int)ptr->func.fntc_entry_loc[i].adr_lcid,
	     (long)ptr->func.fntc_entry_loc[i].adr_sctoff,
	     (long)ptr->func.fntc_entry_loc[i].adr_sctoff);

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
  char *type_str = (char *)0;

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

  if (type_str == (char *)0)
    fprintf (stderr, ", ty: unknown (%ld)\n", (long) type);

  else if (type != LDC_REGION)
    fprintf (stderr, ", ty: %s\n", type_str);

  else
    {
      char *region = "";
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
  char *msg = (char *)0;

  switch (status)
    {
    case MO_ERROR_BAD_MAGIC:		msg = "bad magic number";		break;
    case MO_ERROR_BAD_HDR_VERS:		msg = "bad header version";		break;
    case MO_ERROR_BAD_RAW_HDR_VERS:	msg = "bad raw header version";		break;
    case MO_ERROR_BUF2SML:		msg = "raw header buffer too small";	break;
    case MO_ERROR_OLD_RAW_HDR_FILE:	msg = "old raw header file";		break;
    case MO_ERROR_UNSUPPORTED_VERS:	msg = "unsupported version";		break;
    }

  if (msg == (char *)0)
    fatal ("unknown {de,en}code_mach_o_hdr return value %d", status);
  else
    fatal ("%s", msg);
}


/* Read a file into a memory buffer.  */

static struct file_info *
read_file (name, fd, rw)
     char *name;		/* filename */
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
  p->start = mmap ((caddr_t)0,
		   (rw) ? p->rounded_size : p->size,
		   (rw) ? (PROT_READ | PROT_WRITE) : PROT_READ,
		   MAP_FILE | MAP_VARIABLE | MAP_SHARED,
		   fd,
		   0L);

  if (p->start != (char *)0 && p->start != (char *)-1)
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
	fatal_perror ("lseek to 0 on %s", name);

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
	    fatal_perror ("lseek to 0 on %s", ptr->name);

	  len = write (ptr->fd, ptr->start, ptr->size);
	  if (len < 0)
	    fatal_perror ("read %s", ptr->name);

	  if (len != ptr->size)
	    fatal ("wrote %ld bytes, expected %ld, to %s", len, ptr->size, ptr->name);
	}

      free ((generic *)ptr->start);
    }

  free ((generic *)ptr);
}

#endif /* OBJECT_FORMAT_ROSE */
