
/* Install modified versions of certain ANSI-incompatible system header
   files which are fixed to work correctly with ANSI C and placed in a
   directory that GNU C will search.

   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.

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

#include "auto-host.h"
#include "gansidecl.h"
#include "system.h"
#include <signal.h>

#include "gnu-regex.h"
#include "server.h"

static const char program_id[] = "fixincl version 1.0";

#define MINIMUM_MAXIMUM_LINES   128

/* If this particular system's header files define the macro `MAXPATHLEN',
   we happily take advantage of it; otherwise we use a value which ought
   to be large enough.  */
#ifndef MAXPATHLEN
# define MAXPATHLEN     4096
#endif
#define NAME_TABLE_SIZE (MINIMUM_MAXIMUM_LINES * MAXPATHLEN)

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif

char *file_name_buf;

#define tSCC static const char
#define tCC  const char
#define tSC  static char

typedef int t_success;

#define FAILURE         (-1)
#define SUCCESS           0
#define PROBLEM           1

#define SUCCEEDED(p)    ((p) == SUCCESS)
#define SUCCESSFUL(p)   SUCCEEDED (p)
#define FAILED(p)       ((p) < SUCCESS)
#define HADGLITCH(p)    ((p) > SUCCESS)

#define NUL             '\0'

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
  TT_TEST, TT_EGREP, TT_NEGREP
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

    */
#define FD_MACH_ONLY      0x0000
#define FD_MACH_IFNOT     0x0001
#define FD_SHELL_SCRIPT   0x0002
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
char *pz_find_base = NULL;
int find_base_len = 0;

pid_t process_chain_head = (pid_t) -1;

const char incl_quote_pat[] = "^[ \t]*#[ \t]*include[ \t]*\"[^/]";
regex_t incl_quote_re;

char *load_file  _P_((const char *));
void process  _P_((char *, const char *));
void run_compiles ();
void wait_for_pid _P_(( pid_t ));
void initialize ();

#include "fixincl.x"

/* * * * * * * * * * * * * * * * * * *
 *
 *  MAIN ROUTINE
 */
int
main (argc, argv)
     int argc;
     char **argv;
{
  static const char gnu_lib_mark[] =
    "This file is part of the GNU C Library";

#ifndef NO_BOGOSITY_LIMITS
# define BOGUS_LIMIT    MINIMUM_MAXIMUM_LINES
  size_t loop_ct;
#endif

  char *apz_names[BOGUS_LIMIT];
  size_t file_name_ct;

  /* Before anything else, ensure we can allocate our file name buffer. */
  file_name_buf = (char *) malloc (NAME_TABLE_SIZE);
  if (file_name_buf == (char *) NULL)
    {
      fprintf (stderr, "fixincl cannot allocate 0x%08X bytes\n",
               NAME_TABLE_SIZE);
      exit (EXIT_FAILURE);
    }

  switch (argc)
    {
    case 1:
      break;

    case 2:
      if (strcmp (argv[1], "-v") == 0)
        {
          static const char zFmt[] = "echo '%s'";

          /* The 'version' option is really used to test that:
               1.  The program loads correctly (no missing libraries)
               2.  we can correctly run our server shell process
               3.  that we can compile all the regular expressions.
           */
          run_compiles ();
          sprintf (file_name_buf, zFmt, program_id);
          fputs (file_name_buf + 5, stdout);
          exit (strcmp (run_shell (file_name_buf), program_id));
        }
      freopen (argv[1], "r", stdin);
      break;

    default:
      fputs ("fixincl ERROR:  too many command line arguments\n", stderr);
      exit (EXIT_FAILURE);
    }

  initialize ();

#ifndef NO_BOGOSITY_LIMITS
  /*  Some systems only allow so many calls to fork(2).
      This is inadequate for this program.  Consequently,
      we must let a grandfather process spawn children
      that then spawn all the processes that do the real work.
      */
  for (;;)
    {
      file_name_ct = 0;

      {
        char *pz_buf = file_name_buf;

        /* Only the parent process can read from stdin without confusing
           the world. (How does the child tell the parent to skip
           forward?  Pipes and files behave differently.)  */

        while (  (file_name_ct < BOGUS_LIMIT)
              && (pz_buf < (file_name_buf + NAME_TABLE_SIZE - MAXPATHLEN)))
          {
            if (fgets (pz_buf, MAXPATHLEN, stdin) == (char *) NULL)
              break;
            while (isspace (*pz_buf))
              pz_buf++;
            if ((*pz_buf == '\0') || (*pz_buf == '#'))
              continue;
            apz_names[file_name_ct++] = pz_buf;
            pz_buf += strlen (pz_buf);
            while (isspace (pz_buf[-1]))
              pz_buf--;
            *pz_buf++ = '\0';
          }
      }

      /*  IF we did not get any files this time thru
          THEN we must be done.  */
      if (file_name_ct == 0)
        return EXIT_SUCCESS;

      fflush (stdout);
      fflush (stderr);

      {
        pid_t child = fork ();
        if (child == NULLPROCESS)
          break;

        if (child == NOPROCESS)
          {
            fprintf (stderr, "Error %d (%s) forking in main\n",
                     errno, strerror (errno));
            exit (EXIT_FAILURE);
          }

#ifdef DEBUG
        fprintf (stderr, "Waiting for %d to complete %d files\n",
                 child, file_name_ct);
#endif

        wait_for_pid( child, file_name_ct );
      }
    }
#else
 /*#*/ error "NON-BOGUS LIMITS NOT SUPPORTED?!?!"
#endif

  /*  For every file specified in stdandard in
      (except as throttled for bogus reasons)...
      */
  for (loop_ct = 0; loop_ct < file_name_ct; loop_ct++)
    {
      char *pz_data;
      char *pz_file_name = apz_names[loop_ct];

      if (access (pz_file_name, R_OK) != 0)
        {
          int erno = errno;
          fprintf (stderr, "Cannot access %s from %s\n\terror %d (%s)\n",
                   pz_file_name, getcwd ((char *) NULL, MAXPATHLEN),
                   erno, strerror (erno));
        }
      else if (pz_data = load_file (pz_file_name), (pz_data != (char *) NULL))
        {
          if (strstr (pz_data, gnu_lib_mark) == (char *) NULL)
            process (pz_data, pz_file_name);
          free ((void *) pz_data);
        }
    }

  return EXIT_SUCCESS;
}


/* * * * * * * * * * * * */

void
initialize()
{
  static const char var_not_found[] =
    "fixincl ERROR:  %s environment variable not defined\n";

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
    static const char var[] = "FIND_BASE";
    pz_find_base = getenv (var);
    if (pz_find_base == (char *) NULL)
      {
        fprintf (stderr, var_not_found, var);
        exit (EXIT_FAILURE);
      }
    find_base_len = strlen( pz_find_base );
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

  /*
     Make sure that if we opened a server process, we close it now.
     This is the grandparent process.  We don't need the server anymore
     and our children should make their own.  */

  close_server ();
  (void)wait ( (int*)NULL );
}

/* * * * * * * * * * * * *
 
   wait_for_pid  -  Keep calling `wait(2)' until it returns
   the process id we are looking for.  Not every system has
   `waitpid(2)'.  We also ensure that the children exit with success. */

void
wait_for_pid(child, file_name_ct)
     pid_t child;
     int file_name_ct;
{
  for (;;) {
    int status;
    pid_t dead_kid = wait (&status);

    if (dead_kid == child)
      {
        if (! WIFEXITED( status ))
          {
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
          fprintf (stderr, "Error %d (%s) waiting for %d to finish\n",
                   errno, strerror( errno ), child );
          /* FALLTHROUGH */

        case ECHILD: /* no children to wait for?? */
          return;
        }
      }
  } done_waiting:;
}


/* * * * * * * * * * * * *
 
   load_file loads all the contents of a file into malloc-ed memory.
   Its argument is the name of the file to read in; the returned
   result is the NUL terminated contents of the file.  The file
   is presumed to be an ASCII text file containing no NULs.  */
char *
load_file (pz_file_name)
     const char *pz_file_name;
{
  char *pz_data;
  size_t file_size;

  {
    struct stat stbf;
    
    if (stat (pz_file_name, &stbf) != 0)
      {
        fprintf (stderr, "error %d (%s) stat-ing %s\n",
                 errno, strerror (errno), pz_file_name);
        return (char *) NULL;
      }
    file_size = stbf.st_size;
  }
  if (file_size == 0)
    return (char *) NULL;

  pz_data = (char *) malloc ((file_size + 16) & ~0x00F);
  if (pz_data == (char *) NULL)
    {
      fprintf (stderr, "error:  could not malloc %d bytes\n",
               file_size);
      exit (EXIT_FAILURE);
    }

  {
    FILE *fp = fopen (pz_file_name, "r");
    size_t size_left = file_size;
    char *read_ptr = pz_data;

    if (fp == (FILE *) NULL)
      {
        fprintf (stderr, "error %d (%s) opening %s\n", errno,
                 strerror (errno), pz_file_name);
        free ((void *) pz_data);
        return (char *) NULL;
      }

    do
      {
        size_t sizeRead = fread ((void *) read_ptr, 1, size_left, fp);

        if (sizeRead == 0)
          {
            if (feof (fp))
              break;

            if (ferror (fp))
              {
                int err = errno;
                if (err != EISDIR)
                  fprintf (stderr, "error %d (%s) reading %s\n", err,
                           strerror (err), pz_file_name);
                free ((void *) pz_data);
                fclose (fp);
                return (char *) NULL;
              }
          }

        read_ptr += sizeRead;
        size_left -= sizeRead;
      }
    while (size_left != 0);

    *read_ptr = '\0';
    fclose (fp);
  }
  return pz_data;
}


/* * * * * * * * * * * * *
 
   run_compiles   run all the regexp compiles for all the fixes once.
 */
void
run_compiles ()
{
  tSCC z_bad_comp[] = "fixincl ERROR:  cannot compile %s regex for %s\n\
\texpr = `%s'\n\terror %s\n";
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

  /*  Make sure re_compile_pattern does not stumble across invalid
      data */

  memset ( (void*)p_re, '\0', REGEX_COUNT * sizeof (regex_t) );
  memset ( (void*)&incl_quote_re, '\0', sizeof (regex_t) );

  /*  The patterns we search for are all egrep patterns.
      In the shell version of this program, we invoke egrep
      with the supplied pattern.  Here, we will run
      re_compile_pattern, but it must be using the same rules.  */

  re_set_syntax (RE_SYNTAX_EGREP);
  pz_err = re_compile_pattern (incl_quote_pat, sizeof (incl_quote_pat)-1,
                              &incl_quote_re);
  if (pz_err != (char *) NULL)
    {
      fprintf (stderr, z_bad_comp, "quoted include", "run_compiles",
               incl_quote_pat, pz_err);
      exit (EXIT_FAILURE);
    }

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
          const char **papz_machs = p_fixd->papz_machs;
          char *pz = file_name_buf;
          char *pz_sep = "";
          tCC *pz_if_true;
          tCC *pz_if_false;
          tSCC skip[] = "skip";
          tSCC run[] = "run";

          /*  Construct a shell script that looks like this:

              case our-cpu-platform-os in
              tests-cpu-platform-os-pattern )
                  echo run ;;
              * )
                  echo skip ;;
              esac

              where 'run' and 'skip' may be reversed, depending on
              the sense of the test.  */

          sprintf (pz, "case %s in\n", pz_machine);
          pz += strlen (pz);

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

          /*  FOR any additional machine names to test for,
              insert the " | \\\n" glue and the machine pattern.  */

          for (;;)
            {
              const char* pz_mach = *(papz_machs++);

              if (pz_mach == (const char*) NULL)
                break;
              sprintf (pz, "%s  %s", pz_sep, pz_mach);
              pz += strlen (pz);
              pz_sep = " | \\\n";
            }
          sprintf (pz, " )\n    echo %s ;;\n  * )\n    echo %s ;;\nesac",
                   pz_if_true, pz_if_false);

          /*  Run the script.
              The result will start either with 's' or 'r'.  */

          {
            int skip;
            pz = run_shell (file_name_buf);
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
              pz_err = re_compile_pattern (p_test->pz_test_text,
                                          strlen (p_test->pz_test_text),
                                          p_test->p_test_regex);
              if (pz_err != (char *) NULL)
                {
                  fprintf (stderr, z_bad_comp, "select test", p_fixd->fix_name,
                           p_test->pz_test_text, pz_err);
                  exit (EXIT_FAILURE);
                }
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

#define S_IRALL	(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

FILE *
create_file (pz_file_name)
     const char *pz_file_name;
{
  int fd;
  FILE *pf;
  char fname[MAXPATHLEN];

#ifdef DEBUG
  if (strncmp( pz_file_name, pz_find_base, find_base_len ) != 0)
    {
      fprintf (stderr, "Error:  input file %s does not match %s/*\n",
	       pz_file_name, pz_find_base );
      exit (1);
    }
#endif

  sprintf (fname, "%s/%s", pz_dest_dir, pz_file_name + find_base_len);

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
              mkdir (fname, S_IFDIR | S_IRWXU | S_IRGRP | S_IXGRP
                     | S_IROTH | S_IXOTH);
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
  fprintf (stderr, "Fixed:  %s\n", pz_file_name);
  pf = fdopen (fd, "w");

#ifdef LATER
  {
    static const char hdr[] =
    "/*  DO NOT EDIT THIS FILE.\n\n"
    "    It has been auto-edited by fixincludes from /usr/include/%s\n"
    "    This had to be done to correct non-standard usages in the\n"
    "    original, manufacturer supplied header file.  */\n\n";

    fprintf (pf, hdr, pz_file_name);
  }
#endif
  return pf;
}


/* * * * * * * * * * * * *

  test_test   make sure a shell-style test expression passes.
  Input:  a pointer to the descriptor of the test to run and
          the name of the file that we might want to fix
  Result: SUCCESS or FAILURE, depending on the result of the
          shell script we run.  */

t_success
test_test (p_test, pz_file_name)
     tTestDesc *p_test;
     char*      pz_file_name;
{
  tSCC cmd_fmt[] =
"file=%s\n\
if ( test %s ) > /dev/null 2>&1\n\
then echo TRUE\n\
else echo FALSE\n\
fi";

  char *pz_res;
  t_success res = FAILURE;

  static char cmd_buf[4096];

  sprintf (cmd_buf, cmd_fmt, pz_file_name, p_test->pz_test_text);
  pz_res = run_shell (cmd_buf);
  if (*pz_res == 'T')
    res = SUCCESS;
  free ((void *) pz_res);
  return res;
}


/* * * * * * * * * * * * *
 
  egrep_test   make sure an egrep expression is found in the file text.
  Input:  a pointer to the descriptor of the test to run and
          the pointer to the contents of the file under suspicion
  Result: SUCCESS if the pattern is found, FAILURE otherwise

  The caller may choose 'FAILURE' as 'SUCCESS' if the sense of the test
  is inverted.  */

t_success
egrep_test (pz_data, p_test)
     char *pz_data;
     tTestDesc *p_test;
{
  regmatch_t match;

#ifndef NO_BOGOSITY
  if (p_test->p_test_regex == 0)
    fprintf (stderr, "fixincl ERROR RE not compiled:  `%s'\n",
             p_test->pz_test_text);
#endif
  if (regexec (p_test->p_test_regex, pz_data, 1, &match, 0) == 0)
    return SUCCESS;
  return FAILURE;
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
    if (! isgraph( ch ))
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
extract_quoted_files (pz_data, pz_file_name, p_re_match)
     char *pz_data;
     const char *pz_file_name;
     regmatch_t *p_re_match;
{
  char *pz_dir_end = strrchr (pz_file_name, '/');
  char *pz_incl_quot = pz_data;

  fprintf (stderr, "Quoted includes in %s\n", pz_file_name);

  /*  Set "pz_file_name" to point to the containing subdirectory of the source
      If there is none, then it is in our current directory, ".".   */

  if (pz_dir_end == (char *) NULL)
    pz_file_name = ".";
  else
    *pz_dir_end = '\0';

  for (;;)
    {
      pz_incl_quot += p_re_match->rm_so;

      /*  Skip forward to the included file name */
      while (isspace (*pz_incl_quot))
        pz_incl_quot++;
      while (isspace (*++pz_incl_quot))
        ;
      pz_incl_quot += sizeof ("include") - 1;
      while (*pz_incl_quot++ != '"')
        ;

      if (quoted_file_exists (pz_src_dir, pz_file_name, pz_incl_quot))
        {
          /* Print the source directory and the subdirectory
             of the file in question.  */
          printf ("%s  %s/", pz_src_dir, pz_file_name);
          pz_dir_end = pz_incl_quot;

          /* Append to the directory the relative path of the desired file */
          while (*pz_incl_quot != '"')
            putc (*pz_incl_quot++, stdout);

          /* Now print the destination directory appended with the
             relative path of the desired file */
          printf ("  %s/%s/", pz_dest_dir, pz_file_name);
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

    This loop should only cycle for 1/2 of one loop.
    "chain_open" starts a process that uses "read_fd" as
    its stdin and returns the new fd this process will use
    for stdout.  */

int
start_fixer (read_fd, p_fixd, pz_file_name)
  int read_fd;
  tFixDesc* p_fixd;
  char* pz_file_name;
{
  tSCC z_err[] = "Error %d (%s) starting filter process for %s\n";
  tCC* pz_cmd_save;
  char* pz_cmd;

  if ((p_fixd->fd_flags & FD_SHELL_SCRIPT) == 0)
    pz_cmd = (char*)NULL;
  else
    {
      tSCC z_cmd_fmt[] = "file='%s'\n%s";
      pz_cmd = (char*)xmalloc (strlen (p_fixd->patch_args[2])
                               + sizeof( z_cmd_fmt )
                               + strlen( pz_file_name ));
      sprintf (pz_cmd, z_cmd_fmt, pz_file_name, p_fixd->patch_args[2]);
      pz_cmd_save = p_fixd->patch_args[2];
      p_fixd->patch_args[2] = pz_cmd;
    }

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

      fprintf (stderr, z_err, errno, strerror (errno),
               p_fixd->fix_name);

      if ((errno != EAGAIN) || (++failCt > 10))
        exit (EXIT_FAILURE);
      sleep (1);
    }

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

void
process (pz_data, pz_file_name)
     char *pz_data;
     const char *pz_file_name;
{
  static char env_current_file[1024];
  tFixDesc *p_fixd = fixDescList;
  int todo_ct = FIX_COUNT;
  int read_fd = -1;
  int num_children = 0;

  process_chain_head = NOPROCESS;
  fprintf (stderr, "%-50s   \r", pz_file_name );
  /* For every fix in our fix list, ...  */
  for (; todo_ct > 0; p_fixd++, todo_ct--)
    {
      tTestDesc *p_test;
      int test_ct;

      if (p_fixd->fd_flags & FD_SKIP_TEST)
        continue;

      /*  IF there is a file name restriction,
          THEN ensure the current file name matches one in the pattern  */

      if (p_fixd->file_list != (char *) NULL)
        {
          const char *pz_fname = pz_file_name;
          const char *pz_scan = p_fixd->file_list;
          size_t name_len;

          while ((pz_fname[0] == '.') && (pz_fname[1] == '/'))
            pz_fname += 2;
          name_len = strlen (pz_fname);

          for (;;)
            {
              pz_scan = strstr (pz_scan + 1, pz_fname);
              /*  IF we can't match the string at all,
                  THEN bail  */
              if (pz_scan == (char *) NULL)
                goto next_fix;

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
#ifdef DEBUG_TEST
          static const char z_test_fail[] =
            "%16s test %2d failed for %s\n";
#endif
          switch (p_test->type)
            {
            case TT_TEST:
              if (!SUCCESSFUL (test_test (p_test, pz_file_name)))
                {
#ifdef DEBUG_TEST
                  fprintf (stderr, z_test_fail, p_fixd->fix_name,
                           p_fixd->test_ct - test_ct, pz_file_name);
#endif
                  goto next_fix;
                }
              break;

            case TT_EGREP:
              if (!SUCCESSFUL (egrep_test (pz_data, p_test)))
                {
#ifdef DEBUG_TEST
                  fprintf (stderr, z_test_fail, p_fixd->fix_name,
                           p_fixd->test_ct - test_ct, pz_file_name);
#endif
                  goto next_fix;
                }
              break;

            case TT_NEGREP:
              if (SUCCESSFUL (egrep_test (pz_data, p_test)))
                {
#ifdef DEBUG_TEST
                  fprintf (stderr, z_test_fail, p_fixd->fix_name,
                           p_fixd->test_ct - test_ct, pz_file_name);
#endif
                  goto next_fix;
                }
              break;
            }
        }

      fprintf (stderr, "Applying %-24s to %s\n",
               p_fixd->fix_name, pz_file_name);

      /*  IF we do not have a read pointer,
          THEN this is the first fix for the current file.
          Open the source file.  That will be used as stdin for
          the first fix.  Any subsequent fixes will use the
          stdout descriptor of the previous fix as its stdin.  */

      if (read_fd == -1)
        {
          read_fd = open (pz_file_name, O_RDONLY);
          if (read_fd < 0)
            {
              fprintf (stderr, "Error %d (%s) opening %s\n", errno,
                       strerror (errno), pz_file_name);
              exit (EXIT_FAILURE);
            }
        }

      read_fd = start_fixer (read_fd, p_fixd, pz_file_name);
      num_children++;

    next_fix:
      ;
    }

  /*  IF after all the tests we did not start any patch programs,
      THEN quit now.   */

  if (read_fd < 0)
    return;

  /*  OK.  We have work to do.  Read back in the output
      of the filtering chain.  Compare each byte as we read it with
      the contents of the original file.  As soon as we find any
      difference, we will create the output file, write out all
      the matched text and then copy any remaining data from the
      output of the filter chain.
      */
  {
    FILE *in_fp = fdopen (read_fd, "r");
    FILE *out_fp = (FILE *) NULL;
    char *pz_cmp = pz_data;

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
            out_fp = create_file (pz_file_name);

            /*  IF there are matched data, write it all now. */
            if (pz_cmp != pz_data)
              {
                char c = *pz_cmp;
                
                *pz_cmp = NUL;
                fputs (pz_data, out_fp);
                *pz_cmp = c;
              }

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
        if (regexec (&incl_quote_re, pz_data, 1, &match, 0) == 0)
          extract_quoted_files (pz_data, pz_file_name, &match);
      }
    fclose (in_fp);
  }
  close (read_fd);  /* probably redundant, but I'm paranoid */

  /* Wait for child processes created by chain_open()
     to avoid creating zombies.  */
  while (--num_children >= 0)
    wait ((int *) NULL);
}
