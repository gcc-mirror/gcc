
/* Install modified versions of certain ANSI-incompatible system header
   files which are fixed to work correctly with ANSI C and placed in a
   directory that GNU C will search.

   Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.

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

#include "fixlib.h"

#if HAVE_MMAP
#include <sys/mman.h>
#define  BAD_ADDR ((void*)-1)
#endif

#include <signal.h>

#include "server.h"

#define NO_BOGOSITY

/*  Quality Assurance Marker  :-)

    Any file that contains this string is presumed to have
    been carefully constructed and will not be fixed  */

/*  The contents of this string are not very important.  It is mostly
    just used as part of the "I am alive and working" test.  */

static const char program_id[] = "fixincl version 1.1";

/*  Test Descriptor

    Each fix may have associated tests that determine
    whether the fix needs to be applied or not.
    Each test has a type (from the te_test_type enumeration);
    associated test text; and, if the test is TT_EGREP or
    the negated form TT_NEGREP, a pointer to the compiled
    version of the text string.

    */
typedef enum
{
  TT_TEST, TT_EGREP, TT_NEGREP, TT_FUNCTION
} te_test_type;

typedef struct test_desc tTestDesc;

struct test_desc
{
  te_test_type type;
  const char *pz_test_text;
  regex_t *p_test_regex;
};

typedef struct patch_desc tPatchDesc;

/*  Fix Descriptor

    Everything you ever wanted to know about how to apply
    a particular fix (which files, how to qualify them,
    how to actually make the fix, etc...)

    NB:  the FD_ defines are BIT FLAGS

    */
#define FD_MACH_ONLY      0x0000
#define FD_MACH_IFNOT     0x0001
#define FD_SHELL_SCRIPT   0x0002
#define FD_SUBROUTINE     0x0004
#define FD_REPLACEMENT    0x0008
#define FD_SKIP_TEST      0x8000

typedef struct fix_desc tFixDesc;
struct fix_desc
{
  const char*   fix_name;       /* Name of the fix */
  const char*   file_list;      /* List of files it applies to */
  const char**  papz_machs;     /* List of machine/os-es it applies to */
  regex_t*      unused;
  int           test_ct;
  int           fd_flags;
  tTestDesc*    p_test_desc;
  const char**  patch_args;
};

/*  Working environment strings.  Essentially, invocation 'options'.  */
char *pz_dest_dir = NULL;
char *pz_src_dir = NULL;
char *pz_machine = NULL;
int find_base_len = 0;

typedef enum {
  VERB_SILENT = 0,
  VERB_FIXES,
  VERB_APPLIES,
  VERB_PROGRESS,
  VERB_TESTS,
  VERB_EVERYTHING
} te_verbose;

te_verbose  verbose_level = VERB_PROGRESS;
int have_tty = 0;

#define VLEVEL(l)  (verbose_level >= l)
#define NOT_SILENT VLEVEL(VERB_FIXES)

pid_t process_chain_head = (pid_t) -1;

char*  pz_curr_file;  /*  name of the current file under test/fix  */
char*  pz_curr_data;  /*  original contents of that file  */
t_bool curr_data_mapped;
int    data_map_fd;
size_t data_map_size;
size_t ttl_data_size = 0;
#ifdef DO_STATS
int process_ct = 0;
int apply_ct = 0;
int fixed_ct = 0;
int altered_ct = 0;
#endif /* DO_STATS */

#ifdef HAVE_MMAP
#define UNLOAD_DATA() do { if (curr_data_mapped) { \
  munmap ((void*)pz_curr_data, data_map_size); close (data_map_fd); } \
  else free ((void*)pz_curr_data); } while(0)
#else
#define UNLOAD_DATA() free ((void*)pz_curr_data)
#endif

const char incl_quote_pat[] = "^[ \t]*#[ \t]*include[ \t]*\"[^/]";
tSCC z_fork_err[] = "Error %d (%s) starting filter process for %s\n";
regex_t incl_quote_re;

void do_version ();
char *load_file  _P_((const char *));
void process  _P_((char *, const char *));
void run_compiles ();
void initialize ();
void process ();

/*  External Source Code */

#include "fixincl.x"
#include "fixtests.c"
#include "fixfixes.c"

/* * * * * * * * * * * * * * * * * * *
 *
 *  MAIN ROUTINE
 */
int
main (argc, argv)
     int argc;
     char **argv;
{
  char *file_name_buf;

  switch (argc)
    {
    case 1:
      break;

    case 2:
      if (strcmp (argv[1], "-v") == 0)
        do_version ();
      if (freopen (argv[1], "r", stdin) == (FILE*)NULL)
        {
          fprintf (stderr, "Error %d (%s) reopening %s as stdin\n",
                   errno, strerror (errno), argv[1] );
          exit (EXIT_FAILURE);
        }
      break;

    default:
      fputs ("fixincl ERROR:  too many command line arguments\n", stderr);
      exit (EXIT_FAILURE);
    }

  initialize ();

  have_tty = isatty (fileno (stderr));

  /* Before anything else, ensure we can allocate our file name buffer. */
  file_name_buf = load_file_data (stdin);

  /*  Because of the way server shells work, you have to keep stdin, out
      and err open so that the proper input file does not get closed
      by accident  */

  freopen ("/dev/null", "r", stdin);

  if (file_name_buf == (char *) NULL)
    {
      fputs ("No file names listed for fixing\n", stderr);
      exit (EXIT_FAILURE);
    }

  for (;;)
    {
      char* pz_end;

      /*  skip to start of name, past any "./" prefixes */

      while (ISSPACE (*file_name_buf))  file_name_buf++;
      while ((file_name_buf[0] == '.') && (file_name_buf[1] == '/'))
        file_name_buf += 2;

      /*  Check for end of list  */

      if (*file_name_buf == NUL)
        break;

      /*  Set global file name pointer and find end of name */

      pz_curr_file = file_name_buf;
      pz_end = strchr( pz_curr_file, '\n' );
      if (pz_end == (char*)NULL)
        pz_end = file_name_buf = pz_curr_file + strlen (pz_curr_file);
      else
        file_name_buf = pz_end + 1;

      while ((pz_end > pz_curr_file) && ISSPACE( pz_end[-1]))  pz_end--;

      /*  IF no name is found (blank line) or comment marker, skip line  */

      if ((pz_curr_file == pz_end) || (*pz_curr_file == '#'))
        continue;
      *pz_end = NUL;

#ifdef NO_BOGOSITY
      process ();
#else
      /*  Prevent duplicate output by child process  */

      fflush (stdout);
      fflush (stderr);

      {
        void wait_for_pid _P_(( pid_t ));
        pid_t child = fork ();
        if (child == NULLPROCESS)
          {
            process ();
            return EXIT_SUCCESS;
          }

        if (child == NOPROCESS)
          {
            fprintf (stderr, "Error %d (%s) forking in main\n",
                     errno, strerror (errno));
            exit (EXIT_FAILURE);
          }

        wait_for_pid( child );
      }
#endif
    } /*  for (;;) */

#ifdef DO_STATS
  if (VLEVEL( VERB_PROGRESS )) {
    tSCC zFmt[] =
      "\
Processed %5d files containing %d bytes    \n\
Applying  %5d fixes to %d files\n\
Altering  %5d of them\n";

    fprintf (stderr, zFmt, process_ct, ttl_data_size, apply_ct,
             fixed_ct, altered_ct);
  }
#endif /* DO_STATS */
  return EXIT_SUCCESS;
}


void
do_version ()
{
  static const char zFmt[] = "echo '%s'";
  char zBuf[ 1024 ];

  /* The 'version' option is really used to test that:
     1.  The program loads correctly (no missing libraries)
     2.  we can correctly run our server shell process
     3.  that we can compile all the regular expressions.
  */
  run_compiles ();
  sprintf (zBuf, zFmt, program_id);
  fputs (zBuf + 5, stdout);
  exit (strcmp (run_shell (zBuf), program_id));
}

/* * * * * * * * * * * * */

void
initialize ()
{
  static const char var_not_found[] =
    "fixincl ERROR:  %s environment variable not defined\n\
\tTARGET_MACHINE, DESTDIR, SRCDIR and FIND_BASE are required\n";

  {
    static const char var[] = "TARGET_MACHINE";
    pz_machine = getenv (var);
    if (pz_machine == (char *) NULL)
      {
        fprintf (stderr, var_not_found, var);
        exit (EXIT_FAILURE);
      }
  }

  {
    static const char var[] = "DESTDIR";
    pz_dest_dir = getenv (var);
    if (pz_dest_dir == (char *) NULL)
      {
        fprintf (stderr, var_not_found, var);
        exit (EXIT_FAILURE);
      }
  }

  {
    static const char var[] = "SRCDIR";
    pz_src_dir = getenv (var);
    if (pz_src_dir == (char *) NULL)
      {
        fprintf (stderr, var_not_found, var);
        exit (EXIT_FAILURE);
      }
  }

  {
    static const char var[] = "VERBOSE";
    char* pz = getenv (var);
    if (pz != (char *) NULL)
      {
        if (isdigit( *pz ))
          verbose_level = (te_verbose)atoi( pz );
        else
          switch (*pz) {
          case 's':
          case 'S':
            verbose_level = VERB_SILENT;     break;

          case 'f':
          case 'F':
            verbose_level = VERB_FIXES;      break;

          case 'a':
          case 'A':
            verbose_level = VERB_APPLIES;    break;

          case 'p':
          case 'P':
            verbose_level = VERB_PROGRESS;   break;

          case 't':
          case 'T':
            verbose_level = VERB_TESTS;      break;

          case 'e':
          case 'E':
            verbose_level = VERB_EVERYTHING; break;
          }
      }
  }

  {
    static const char var[] = "FIND_BASE";
    char *pz = getenv (var);
    if (pz == (char *) NULL)
      {
        fprintf (stderr, var_not_found, var);
        exit (EXIT_FAILURE);
      }
    while ((pz[0] == '.') && (pz[1] == '/'))
      pz += 2;
    if ((pz[0] != '.') || (pz[1] != NUL))
      find_base_len = strlen( pz );
  }

  /*  Compile all the regular expressions now.
      That way, it is done only once for the whole run.
      */
  run_compiles ();

  signal (SIGQUIT, SIG_IGN);
  signal (SIGIOT,  SIG_IGN);
  signal (SIGPIPE, SIG_IGN);
  signal (SIGALRM, SIG_IGN);
  signal (SIGTERM, SIG_IGN);
#ifndef NO_BOGOSITY
  /*
     Make sure that if we opened a server process, we close it now.
     This is the grandparent process.  We don't need the server anymore
     and our children should make their own.  */

  close_server ();
  (void)wait ( (int*)NULL );
#endif
}

#ifndef NO_BOGOSITY
/* * * * * * * * * * * * *

   wait_for_pid  -  Keep calling `wait(2)' until it returns
   the process id we are looking for.  Not every system has
   `waitpid(2)'.  We also ensure that the children exit with success. */

void
wait_for_pid(child)
     pid_t child;
{
  for (;;) {
    int status;
    pid_t dead_kid = wait (&status);

    if (dead_kid == child)
      {
        if (! WIFEXITED( status ))
          {
            if (WSTOPSIG( status ) == 0)
              break;

            fprintf (stderr, "child process %d is hung on signal %d\n",
                     child, WSTOPSIG( status ));
            exit (EXIT_FAILURE);
          }
        if (WEXITSTATUS( status ) != 0)
          {
            fprintf (stderr, "child process %d exited with status %d\n",
                     child, WEXITSTATUS( status ));
            exit (EXIT_FAILURE);
          }
        break; /* normal child completion */
      }

    /*
       IF there is an error, THEN see if it is retryable.
       If it is not retryable, then break out of this loop.  */
    if (dead_kid == NOPROCESS)
      {
        switch (errno) {
        case EINTR:
        case EAGAIN:
          break;

        default:
          if (NOT_SILENT)
            fprintf (stderr, "Error %d (%s) waiting for %d to finish\n",
                     errno, strerror( errno ), child );
          /* FALLTHROUGH */

        case ECHILD: /* no children to wait for?? */
          return;
        }
      }
  } done_waiting:;
}
#endif /* NO_BOGOSITY */

/* * * * * * * * * * * * *

   load_file loads all the contents of a file into malloc-ed memory.
   Its argument is the name of the file to read in; the returned
   result is the NUL terminated contents of the file.  The file
   is presumed to be an ASCII text file containing no NULs.  */
char *
load_file ( fname )
    const char* fname;
{
  struct stat stbf;
  char* res;

  if (stat (fname, &stbf) != 0)
    {
      if (NOT_SILENT)
        fprintf (stderr, "error %d (%s) stat-ing %s\n",
                 errno, strerror (errno), fname );
      return (char *) NULL;
    }
  if (stbf.st_size == 0)
    return (char*)NULL;

  data_map_size = stbf.st_size+1;
  data_map_fd   = open (fname, O_RDONLY);
  ttl_data_size += data_map_size-1;

  if (data_map_fd < 0)
    {
      if (NOT_SILENT)
        fprintf (stderr, "error %d (%s) opening %s for read\n",
                 errno, strerror (errno), fname);
      return (char*)NULL;
    }

#ifdef HAVE_MMAP
  curr_data_mapped = BOOL_TRUE;
  res = (char*)mmap ((void*)NULL, data_map_size, PROT_READ, MAP_PRIVATE,
                     data_map_fd, 0);
  if (res == (char*)BAD_ADDR)
    {
      curr_data_mapped = BOOL_FALSE;
      res = load_file_data ( fdopen (data_map_fd, "r"));
    }
#else
  curr_data_mapped = BOOL_FALSE;
  res = load_file_data ( fdopen (data_map_fd, "r"));
#endif

  return res;
}


/* * * * * * * * * * * * *

   run_compiles   run all the regexp compiles for all the fixes once.
 */
void
run_compiles ()
{
  tFixDesc *p_fixd = fixDescList;
  int fix_ct = FIX_COUNT;
  tTestDesc *p_test;
  int test_ct;
  int re_ct = REGEX_COUNT;
  const char *pz_err;
  regex_t *p_re = (regex_t *) malloc (REGEX_COUNT * sizeof (regex_t));

  if (p_re == (regex_t *) NULL)
    {
      fprintf (stderr, "fixincl ERROR:  cannot allocate %d bytes for regex\n",
               REGEX_COUNT * sizeof (regex_t));
      exit (EXIT_FAILURE);
    }

  /*  Make sure compile_re does not stumble across invalid data */

  memset ( (void*)p_re, '\0', REGEX_COUNT * sizeof (regex_t) );
  memset ( (void*)&incl_quote_re, '\0', sizeof (regex_t) );

  compile_re (incl_quote_pat, &incl_quote_re, 1,
	      "quoted include", "run_compiles");

  /* FOR every fixup, ...  */
  do
    {
      p_test = p_fixd->p_test_desc;
      test_ct = p_fixd->test_ct;

      /*  IF the machine type pointer is not NULL (we are not in test mode)
             AND this test is for or not done on particular machines
          THEN ...   */

      if (  (pz_machine != NULL)
         && (p_fixd->papz_machs != (const char**) NULL) )
        {
          tSCC case_fmt[] = "case %s in\n";     /*  9 bytes, plus string */
          tSCC esac_fmt[] =
               " )\n    echo %s ;;\n* ) echo %s ;;\nesac";/*  4 bytes */
          tSCC skip[] = "skip";                 /*  4 bytes */
          tSCC run[] = "run";                   /*  3 bytes */
          /* total bytes to add to machine sum:    49 - see fixincl.tpl */

          const char **papz_machs = p_fixd->papz_machs;
          char *pz;
          char *pz_sep = "";
          tCC *pz_if_true;
          tCC *pz_if_false;
          char cmd_buf[ MACH_LIST_SIZE_LIMIT ]; /* size lim from fixincl.tpl */

          /* Start the case statement */

          sprintf (cmd_buf, case_fmt, pz_machine);
          pz = cmd_buf + strlen (cmd_buf);

          /*  Determine if a match means to apply the fix or not apply it */

          if (p_fixd->fd_flags & FD_MACH_IFNOT)
            {
              pz_if_true  = skip;
              pz_if_false = run;
            }
          else
            {
              pz_if_true  = run;
              pz_if_false = skip;
            }

          /*  Emit all the machine names.  If there are more than one,
              then we will insert " | \\\n" between the names  */

          for (;;)
            {
              const char* pz_mach = *(papz_machs++);

              if (pz_mach == (const char*) NULL)
                break;
              sprintf (pz, "%s%s", pz_sep, pz_mach);
              pz += strlen (pz);
              pz_sep = " | \\\n";
            }

          /* Now emit the match and not-match actions and the esac */

          sprintf (pz, esac_fmt, pz_if_true, pz_if_false);

          /*  Run the script.
              The result will start either with 's' or 'r'.  */

          {
            int skip;
            pz = run_shell (cmd_buf);
            skip = (*pz == 's');
            free ( (void*)pz );
            if (skip)
              {
                p_fixd->fd_flags |= FD_SKIP_TEST;
                continue;
              }
           }
        }

      /* FOR every test for the fixup, ...  */

      while (--test_ct >= 0)
        {
          switch (p_test->type)
            {
            case TT_EGREP:
            case TT_NEGREP:
              /*  You might consider putting the following under #ifdef.
                  The number of re's used is computed by autogen.
                  So, it is static and known at compile time.  */

              if (--re_ct < 0)
                {
                  fputs ("out of RE's\n", stderr);
                  exit (EXIT_FAILURE);
                }

              p_test->p_test_regex = p_re++;
	      compile_re (p_test->pz_test_text, p_test->p_test_regex, 0,
			  "select test", p_fixd->fix_name);
	    }
          p_test++;
        }
    }
  while (p_fixd++, --fix_ct > 0);
}


/* * * * * * * * * * * * *

   create_file  Create the output modified file.
   Input:    the name of the file to create
   Returns:  a file pointer to the new, open file  */

#if defined(S_IRUSR) && defined(S_IWUSR) && \
    defined(S_IRGRP) && defined(S_IROTH)

#   define S_IRALL (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#else
#   define S_IRALL 0644
#endif

#if defined(S_IRWXU) && defined(S_IRGRP) && defined(S_IXGRP) && \
    defined(S_IROTH) && defined(S_IXOTH)

#   define S_DIRALL (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH)
#else
#   define S_DIRALL 0755
#endif


FILE *
create_file ()
{
  int fd;
  FILE *pf;
  char fname[MAXPATHLEN];

  sprintf (fname, "%s/%s", pz_dest_dir, pz_curr_file + find_base_len);

  fd = open (fname, O_WRONLY | O_CREAT | O_TRUNC, S_IRALL);

  /*  We may need to create the directories needed... */
  if ((fd < 0) && (errno == ENOENT))
    {
      char *pz_dir = strchr (fname + 1, '/');
      struct stat stbf;

      while (pz_dir != (char *) NULL)
        {
          *pz_dir = NUL;
          if (stat (fname, &stbf) < 0)
            {
              mkdir (fname, S_IFDIR | S_DIRALL);
            }

          *pz_dir = '/';
          pz_dir = strchr (pz_dir + 1, '/');
        }

      /*  Now, lets try the open again... */
      fd = open (fname, O_WRONLY | O_CREAT | O_TRUNC, S_IRALL);
    }
  if (fd < 0)
    {
      fprintf (stderr, "Error %d (%s) creating %s\n",
               errno, strerror (errno), fname);
      exit (EXIT_FAILURE);
    }
  if (NOT_SILENT)
    fprintf (stderr, "Fixed:  %s\n", pz_curr_file);
  pf = fdopen (fd, "w");

#ifdef LATER
  {
    static const char hdr[] =
    "/*  DO NOT EDIT THIS FILE.\n\n"
    "    It has been auto-edited by fixincludes from /usr/include/%s\n"
    "    This had to be done to correct non-standard usages in the\n"
    "    original, manufacturer supplied header file.  */\n\n";

    fprintf (pf, hdr, pz_curr_file);
  }
#endif
  return pf;
}


/* * * * * * * * * * * * *

  test_test   make sure a shell-style test expression passes.
  Input:  a pointer to the descriptor of the test to run and
          the name of the file that we might want to fix
  Result: APPLY_FIX or SKIP_FIX, depending on the result of the
          shell script we run.  */

int
test_test (p_test, pz_test_file)
     tTestDesc *p_test;
     char*      pz_test_file;
{
  tSCC cmd_fmt[] =
"file=%s\n\
if ( test %s ) > /dev/null 2>&1\n\
then echo TRUE\n\
else echo FALSE\n\
fi";

  char *pz_res;
  int res = SKIP_FIX;

  static char cmd_buf[4096];

  sprintf (cmd_buf, cmd_fmt, pz_test_file, p_test->pz_test_text);
  pz_res = run_shell (cmd_buf);
  if (*pz_res == 'T')
    res = APPLY_FIX;
  free ((void *) pz_res);
  return res;
}


/* * * * * * * * * * * * *

  egrep_test   make sure an egrep expression is found in the file text.
  Input:  a pointer to the descriptor of the test to run and
          the pointer to the contents of the file under suspicion
  Result: APPLY_FIX if the pattern is found, SKIP_FIX otherwise

  The caller may choose to reverse meaning if the sense of the test
  is inverted.  */

int
egrep_test (pz_data, p_test)
     char *pz_data;
     tTestDesc *p_test;
{
#ifdef DEBUG
  if (p_test->p_test_regex == 0)
    fprintf (stderr, "fixincl ERROR RE not compiled:  `%s'\n",
             p_test->pz_test_text);
#endif
  if (regexec (p_test->p_test_regex, pz_data, 0, 0, 0) == 0)
    return APPLY_FIX;
  return SKIP_FIX;
}


/* * * * * * * * * * * * *

  quoted_file_exists  Make sure that a file exists before we emit
  the file name.  If we emit the name, our invoking shell will try
  to copy a non-existing file into the destination directory.  */

int
quoted_file_exists (pz_src_path, pz_file_path, pz_file)
     char* pz_src_path;
     char* pz_file_path;
     char* pz_file;
{
  char z[ MAXPATHLEN ];
  char* pz;
  sprintf (z, "%s/%s/", pz_src_path, pz_file_path);
  pz = z + strlen ( z );

  for (;;) {
    char ch = *pz_file++;
    if (! ISGRAPH( ch ))
      return 0;
    if (ch == '"')
      break;
    *pz++ = ch;
  }
  *pz = '\0';
  {
    struct stat s;
    if (stat (z, &s) != 0)
      return 0;
    return S_ISREG( s.st_mode );
  }
}


/* * * * * * * * * * * * *
 *
   extract_quoted_files

   The syntax, `#include "file.h"' specifies that the compiler is to
   search the local directory of the current file before the include
   list.  Consequently, if we have modified a header and stored it in
   another directory, any files that are included by that modified
   file in that fashion must also be copied into this new directory.
   This routine finds those flavors of #include and for each one found
   emits a triple of:

    1.  source directory of the original file
    2.  the relative path file name of the #includ-ed file
    3.  the full destination path for this file

   Input:  the text of the file, the file name and a pointer to the
           match list where the match information was stored.
   Result: internally nothing.  The results are written to stdout
           for interpretation by the invoking shell  */


void
extract_quoted_files (pz_data, pz_fixed_file, p_re_match)
     char *pz_data;
     const char *pz_fixed_file;
     regmatch_t *p_re_match;
{
  char *pz_dir_end = strrchr (pz_fixed_file, '/');
  char *pz_incl_quot = pz_data;

  if (VLEVEL( VERB_APPLIES ))
    fprintf (stderr, "Quoted includes in %s\n", pz_fixed_file);

  /*  Set "pz_fixed_file" to point to the containing subdirectory of the source
      If there is none, then it is in our current directory, ".".   */

  if (pz_dir_end == (char *) NULL)
    pz_fixed_file = ".";
  else
    *pz_dir_end = '\0';

  for (;;)
    {
      pz_incl_quot += p_re_match->rm_so;

      /*  Skip forward to the included file name */
      while (ISSPACE (*pz_incl_quot))
        pz_incl_quot++;
      /* ISSPACE() may evaluate is argument more than once!  */
      while (++pz_incl_quot, ISSPACE (*pz_incl_quot))
        ;
      pz_incl_quot += sizeof ("include") - 1;
      while (*pz_incl_quot++ != '"')
        ;

      if (quoted_file_exists (pz_src_dir, pz_fixed_file, pz_incl_quot))
        {
          /* Print the source directory and the subdirectory
             of the file in question.  */
          printf ("%s  %s/", pz_src_dir, pz_fixed_file);
          pz_dir_end = pz_incl_quot;

          /* Append to the directory the relative path of the desired file */
          while (*pz_incl_quot != '"')
            putc (*pz_incl_quot++, stdout);

          /* Now print the destination directory appended with the
             relative path of the desired file */
          printf ("  %s/%s/", pz_dest_dir, pz_fixed_file);
          while (*pz_dir_end != '"')
            putc (*pz_dir_end++, stdout);

          /* End of entry */
          putc ('\n', stdout);
        }

      /* Find the next entry */
      if (regexec (&incl_quote_re, pz_incl_quot, 1, p_re_match, 0) != 0)
        break;
    }
}


/* * * * * * * * * * * * *

    Somebody wrote a *_fix subroutine that we must call.
    */

int
internal_fix (read_fd, p_fixd)
  int read_fd;
  tFixDesc* p_fixd;
{
  int fd[2];

  if (pipe( fd ) != 0)
    {
      fprintf (stderr, "Error %d on pipe(2) call\n", errno );
      exit (EXIT_FAILURE);
    }

  for (;;)
    {
      pid_t childid = fork();

      switch (childid)
        {
        case -1:
          break;

        case 0:
          close (fd[0]);
          goto do_child_task;

        default:
          /*
           *  Parent process
           */
          close (read_fd);
          close (fd[1]);
          return fd[0];
        }

      /*
       *  Parent in error
       */
      fprintf (stderr, z_fork_err, errno, strerror (errno),
               p_fixd->fix_name);
      {
        static int failCt = 0;
        if ((errno != EAGAIN) || (++failCt > 10))
          exit (EXIT_FAILURE);
        sleep (1);
      }
    } do_child_task:;

  /*
   *  Close our current stdin and stdout
   */
  close (STDIN_FILENO);
  close (STDOUT_FILENO);
  UNLOAD_DATA();

  /*
   *  Make the fd passed in the stdin, and the write end of
   *  the new pipe become the stdout.
   */
  fcntl (fd[1], F_DUPFD, STDOUT_FILENO);
  fcntl (read_fd, F_DUPFD, STDIN_FILENO);
  fdopen (STDIN_FILENO, "r");
  fdopen (STDOUT_FILENO, "w");

  apply_fix (p_fixd->patch_args[0], pz_curr_file);
  exit (0);
}


/* * * * * * * * * * * * *

    This loop should only cycle for 1/2 of one loop.
    "chain_open" starts a process that uses "read_fd" as
    its stdin and returns the new fd this process will use
    for stdout.  */

int
start_fixer (read_fd, p_fixd, pz_fix_file)
  int read_fd;
  tFixDesc* p_fixd;
  char* pz_fix_file;
{
  tCC* pz_cmd_save;
  char* pz_cmd;

  if ((p_fixd->fd_flags & FD_SUBROUTINE) != 0)
    return internal_fix (read_fd, p_fixd);

  if ((p_fixd->fd_flags & FD_SHELL_SCRIPT) == 0)
    pz_cmd = (char*)NULL;
  else
    {
      tSCC z_cmd_fmt[] = "file='%s'\n%s";
      pz_cmd = (char*)malloc (strlen (p_fixd->patch_args[2])
                               + sizeof( z_cmd_fmt )
                               + strlen( pz_fix_file ));
      if (pz_cmd == (char*)NULL)
        {
          fputs ("allocation failure\n", stderr);
          exit (EXIT_FAILURE);
        }
      sprintf (pz_cmd, z_cmd_fmt, pz_fix_file, p_fixd->patch_args[2]);
      pz_cmd_save = p_fixd->patch_args[2];
      p_fixd->patch_args[2] = pz_cmd;
    }

  /*  Start a fix process, handing off the  previous read fd for its
      stdin and getting a new fd that reads from the fix process' stdout.
      We normally will not loop, but we will up to 10 times if we keep
      getting "EAGAIN" errors.

      */
  for (;;)
    {
      static int failCt = 0;
      int fd;

      fd = chain_open (read_fd,
                       (t_pchar *) p_fixd->patch_args,
                       (process_chain_head == -1)
                       ? &process_chain_head : (pid_t *) NULL);

      if (fd != -1)
        {
          read_fd = fd;
          break;
        }

      fprintf (stderr, z_fork_err, errno, strerror (errno),
               p_fixd->fix_name);

      if ((errno != EAGAIN) || (++failCt > 10))
        exit (EXIT_FAILURE);
      sleep (1);
    }

  /*  IF we allocated a shell script command,
      THEN free it and restore the command format to the fix description */
  if (pz_cmd != (char*)NULL)
    {
      free ((void*)pz_cmd);
      p_fixd->patch_args[2] = pz_cmd_save;
    }

  return read_fd;
}


/* * * * * * * * * * * * *

   Process the potential fixes for a particular include file.
   Input:  the original text of the file and the file's name
   Result: none.  A new file may or may not be created.  */

t_bool
fix_applies (p_fixd)
  tFixDesc *p_fixd;
{
#ifdef DEBUG
  static const char z_failed[] = "not applying %s to %s - test %d failed\n";
#endif
  const char *pz_fname = pz_curr_file;
  const char *pz_scan = p_fixd->file_list;
  int test_ct;
  tTestDesc *p_test;

  if (p_fixd->fd_flags & FD_SKIP_TEST)
    return BOOL_FALSE;

  /*  IF there is a file name restriction,
      THEN ensure the current file name matches one in the pattern  */

  if (pz_scan != (char *) NULL)
    {
      size_t name_len;

      while ((pz_fname[0] == '.') && (pz_fname[1] == '/'))
        pz_fname += 2;
      name_len = strlen (pz_fname);

      for (;;)
        {
          pz_scan = strstr (pz_scan + 1, pz_fname);
          /*  IF we can't match the string at all,
              THEN bail  */
          if (pz_scan == (char *) NULL) {
#ifdef DEBUG
            if (VLEVEL( VERB_EVERYTHING ))
              fprintf (stderr, "file %s not in list for %s\n",
                       pz_fname, p_fixd->fix_name );
#endif
            return BOOL_FALSE;
          }

          /*  IF the match is surrounded by the '|' markers,
              THEN we found a full match -- time to run the tests  */

          if ((pz_scan[-1] == '|') && (pz_scan[name_len] == '|'))
            break;
        }
    }

  /*  FOR each test, see if it fails.
      IF it does fail, then we go on to the next test */

  for (p_test = p_fixd->p_test_desc, test_ct = p_fixd->test_ct;
       test_ct-- > 0;
       p_test++)
    {
      switch (p_test->type)
        {
        case TT_TEST:
          if (test_test (p_test, pz_curr_file) != APPLY_FIX) {
#ifdef DEBUG
            if (VLEVEL( VERB_EVERYTHING ))
              fprintf (stderr, z_failed, p_fixd->fix_name, pz_fname,
                       p_fixd->test_ct - test_ct);
#endif
            return BOOL_FALSE;
          }
          break;

        case TT_EGREP:
          if (egrep_test (pz_curr_data, p_test) != APPLY_FIX) {
#ifdef DEBUG
            if (VLEVEL( VERB_EVERYTHING ))
              fprintf (stderr, z_failed, p_fixd->fix_name, pz_fname,
                       p_fixd->test_ct - test_ct);
#endif
            return BOOL_FALSE;
          }
          break;

        case TT_NEGREP:
          if (egrep_test (pz_curr_data, p_test) == APPLY_FIX) {
#ifdef DEBUG
            if (VLEVEL( VERB_EVERYTHING ))
              fprintf (stderr, z_failed, p_fixd->fix_name, pz_fname,
                       p_fixd->test_ct - test_ct);
#endif
            /*  Negated sense  */
            return BOOL_FALSE;
          }
          break;

        case TT_FUNCTION:
          if (run_test (p_test->pz_test_text, pz_curr_file, pz_curr_data)
              != APPLY_FIX) {
#ifdef DEBUG
            if (VLEVEL( VERB_EVERYTHING ))
              fprintf (stderr, z_failed, p_fixd->fix_name, pz_fname,
                       p_fixd->test_ct - test_ct);
#endif
            return BOOL_FALSE;
          }
          break;
        }
    }

  return BOOL_TRUE;
}


/* * * * * * * * * * * * *

   Write out a replacement file  */

void
write_replacement (p_fixd)
  tFixDesc *p_fixd;
{
   const char* pz_text = p_fixd->patch_args[0];

   if ((pz_text == (char*)NULL) || (*pz_text == NUL))
     return;

   {
     FILE* out_fp = create_file (pz_curr_file);
     fputs (pz_text, out_fp);
     fclose (out_fp);
   }
}


/* * * * * * * * * * * * *

    We have work to do.  Read back in the output
    of the filtering chain.  Compare each byte as we read it with
    the contents of the original file.  As soon as we find any
    difference, we will create the output file, write out all
    the matched text and then copy any remaining data from the
    output of the filter chain.
    */
void
test_for_changes (read_fd)
  int read_fd;
{
  FILE *in_fp = fdopen (read_fd, "r");
  FILE *out_fp = (FILE *) NULL;
  char *pz_cmp = pz_curr_data;

#ifdef DO_STATS
  fixed_ct++;
#endif
  for (;;)
    {
      int ch;

      ch = getc (in_fp);
      if (ch == EOF)
        break;

      /*  IF we are emitting the output
          THEN emit this character, too.
      */
      if (out_fp != (FILE *) NULL)
        putc (ch, out_fp);

      /*  ELSE if this character does not match the original,
          THEN now is the time to start the output.
      */
      else if (ch != *pz_cmp)
        {
          out_fp = create_file (pz_curr_file);

#ifdef DO_STATS
          altered_ct++;
#endif
          /*  IF there are matched data, write the matched part now. */
          if (pz_cmp != pz_curr_data)
            fwrite (pz_curr_data, (size_t)(pz_cmp - pz_curr_data), 1, out_fp);

          /*  Emit the current unmatching character */
          putc (ch, out_fp);
        }
      else
        /*  ELSE the character matches.  Advance the compare ptr */
        pz_cmp++;
    }

  /*  IF we created the output file, ... */
  if (out_fp != (FILE *) NULL)
    {
      regmatch_t match;

      /* Close the file and see if we have to worry about
         `#include "file.h"' constructs.  */
      fclose (out_fp);
      if (regexec (&incl_quote_re, pz_curr_data, 1, &match, 0) == 0)
        extract_quoted_files (pz_curr_data, pz_curr_file, &match);
    }

  fclose (in_fp);
  close (read_fd);  /* probably redundant, but I'm paranoid */
}


/* * * * * * * * * * * * *

   Process the potential fixes for a particular include file.
   Input:  the original text of the file and the file's name
   Result: none.  A new file may or may not be created.  */

void
process ()
{
  static char env_current_file[1024];
  tFixDesc *p_fixd = fixDescList;
  int todo_ct = FIX_COUNT;
  int read_fd = -1;
  int num_children = 0;

  if (access (pz_curr_file, R_OK) != 0)
    {
      int erno = errno;
      fprintf (stderr, "Cannot access %s from %s\n\terror %d (%s)\n",
               pz_curr_file, getcwd ((char *) NULL, MAXPATHLEN),
               erno, strerror (erno));
      return;
    }

  pz_curr_data = load_file (pz_curr_file);
  if (pz_curr_data == (char *) NULL)
    return;

#ifdef DO_STATS
  process_ct++;
#endif
  if (VLEVEL( VERB_PROGRESS ) && have_tty)
    fprintf (stderr, "%6d %-50s   \r", data_map_size, pz_curr_file );

  process_chain_head = NOPROCESS;

  /* For every fix in our fix list, ...  */
  for (; todo_ct > 0; p_fixd++, todo_ct--)
    {
      if (! fix_applies (p_fixd))
        continue;

      if (VLEVEL( VERB_APPLIES ))
        fprintf (stderr, "Applying %-24s to %s\n",
                 p_fixd->fix_name, pz_curr_file);

      if (p_fixd->fd_flags & FD_REPLACEMENT)
        {
          write_replacement (p_fixd);
          UNLOAD_DATA();
          return;
        }

      /*  IF we do not have a read pointer,
          THEN this is the first fix for the current file.
          Open the source file.  That will be used as stdin for
          the first fix.  Any subsequent fixes will use the
          stdout descriptor of the previous fix for its stdin.  */

      if (read_fd == -1)
        {
          read_fd = open (pz_curr_file, O_RDONLY);
          if (read_fd < 0)
            {
              fprintf (stderr, "Error %d (%s) opening %s\n", errno,
                       strerror (errno), pz_curr_file);
              exit (EXIT_FAILURE);
            }

          /*  Ensure we do not get duplicate output */

          fflush (stdout);
        }

      read_fd = start_fixer (read_fd, p_fixd, pz_curr_file);
      num_children++;
    }

  /*  IF we have a read-back file descriptor,
      THEN check for changes and write output if changed.   */

  if (read_fd >= 0)
    {
      test_for_changes (read_fd);
#ifdef DO_STATS
      apply_ct += num_children;
#endif
      /* Wait for child processes created by chain_open()
         to avoid leaving zombies.  */
      do  {
        wait ((int *) NULL);
      } while (--num_children > 0);
    }

  UNLOAD_DATA();
}
