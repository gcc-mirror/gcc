/*
 * $Id: fixincl.c,v 1.2 1998/12/16 21:19:03 law Exp $
 *
 * Install modified versions of certain ANSI-incompatible system header
 * files which are fixed to work correctly with ANSI C and placed in a
 * directory that GNU C will search.
 *
 * See README-fixinc for more information.
 *
 *  fixincl is free software.
 *  
 *  You may redistribute it and/or modify it under the terms of the
 *  GNU General Public License, as published by the Free Software
 *  Foundation; either version 2, or (at your option) any later version.
 *  
 *  fixincl is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *  See the GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with fixincl.  See the file "COPYING".  If not,
 *  write to:  The Free Software Foundation, Inc.,
 *             59 Temple Place - Suite 330,
 *             Boston,  MA  02111-1307, USA.
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>

#include "regex.h"
#include "server.h"

#define tSCC static const char
#define tCC  const char
#define tSC  static char

typedef int tSuccess;

#define FAILURE  ((tSuccess)-1)
#define SUCCESS  ((tSuccess) 0)
#define PROBLEM  ((tSuccess) 1)

#define SUCCEEDED( p )     ((p) == SUCCESS)
#define SUCCESSFUL( p )    SUCCEEDED( p )
#define FAILED( p )        ((p) <  SUCCESS)
#define HADGLITCH( p )     ((p) >  SUCCESS)

#define NUL '\0'

typedef enum
  {
    TT_TEST, TT_EGREP, TT_NEGREP
  }
teTestType;

typedef struct test_desc tTestDesc;

struct test_desc
  {
    teTestType type;
    const char *pzTest;
    regex_t *pTestRegex;
  };

typedef struct patch_desc tPatchDesc;

#define FD_MACH_ONLY      0x0000
#define FD_MACH_IFNOT     0x0001
#define FD_SKIP_TEST      0x8000

typedef struct fix_desc tFixDesc;
struct fix_desc
  {
    const char*   pzFixName;     /* Name of the fix */
    const char*   pzFileList;    /* List of files it applies to */
    const char**  papzMachs;     /* List of machine/os-es it applies to */
    regex_t*      pListRegex;
    int           testCt;
    int           fdFlags;
    tTestDesc*    pTestDesc;
    const char**  papzPatchArgs;
  };

char *pzDestDir   = (char *) NULL;
char *pzSrcDir    = (char *) NULL;
char *pzMachine   = (char *) NULL;

pid_t chainHead = (pid_t) - 1;

const char zInclQuote[] = "^[ \t]*#[ \t]*include[ \t]*\"[^/]";
regex_t inclQuoteRegex;

char zFileNameBuf[0x8000];

char *loadFile (const char *pzFile);
void process (char *data, const char *dir, const char *file);
void runCompiles (void);

#include "fixincl.x"


int
main (argc, argv)
     int argc;
     char **argv;
{
  static const char zGnuLib[] =
  "This file is part of the GNU C Library";
  static const char zVarNotFound[] =
      "fixincl ERROR:  %s environment variable not defined\n";

#ifndef NO_BOGOSITY_LIMITS
# define BOGUS_LIMIT 256
  size_t loopCt;
#endif

  char *apzNames[BOGUS_LIMIT];
  size_t fileNameCt;

  if (argc != 1)
    {
      if (argc != 2)
        {
          fputs ("fixincl ERROR:  too many command line arguments\n", stderr);
          exit (EXIT_FAILURE);
        }

      if (strcmp (argv[1], "-v") == 0)
        {
          fputs ("$Id: fixincl.c,v 1.2 1998/12/16 21:19:03 law Exp $\n", stderr);
          exit (EXIT_SUCCESS);
        }

      freopen (argv[1], "r", stdin);
    }

  {
    static const char zVar[] = "TARGET_MACHINE";
    pzMachine = getenv( zVar );
    if (pzMachine == (char *)NULL)
      {
        fprintf( stderr, zVarNotFound, zVar );
        exit (EXIT_FAILURE);
      }
  }

  {
    static const char zVar[] = "DESTDIR";
    pzDestDir = getenv( zVar );
    if (pzDestDir == (char *)NULL)
      {
        fprintf( stderr, zVarNotFound, zVar );
        exit (EXIT_FAILURE);
      }
  }

  {
    static const char zVar[] = "SRCDIR";
    pzSrcDir = getenv( zVar );
    if (pzSrcDir == (char *)NULL)
      {
        fprintf( stderr, zVarNotFound, zVar );
        exit (EXIT_FAILURE);
      }
  }

  runCompiles ();

  signal ( SIGQUIT, SIG_IGN );
  signal ( SIGIOT,  SIG_IGN );
  signal ( SIGPIPE, SIG_IGN );
  signal ( SIGALRM, SIG_IGN );
  signal ( SIGTERM, SIG_IGN );
  signal ( SIGCHLD, SIG_IGN );

#ifndef NO_BOGOSITY_LIMITS
  for (;;)
    {
      char *pzBuf;
      pid_t child;

      /*
       *  Only the parent process can read from stdin without
       *  confusing the world.  (How does the child tell the
       *  parent to skip forward?  Pipes and files behave differently.)
       */
      for (fileNameCt = 0, pzBuf = zFileNameBuf;
           (fileNameCt < BOGUS_LIMIT)
           && (pzBuf
               < (zFileNameBuf + sizeof (zFileNameBuf) - MAXPATHLEN));
        )
        {

          if (fgets (pzBuf, MAXPATHLEN, stdin) == (char *) NULL)
            break;
          while (isspace (*pzBuf))
            pzBuf++;
          apzNames[fileNameCt++] = pzBuf;
          pzBuf += strlen (pzBuf);
          while (isspace (pzBuf[-1]))
            pzBuf--;
          *pzBuf++ = '\0';
        }

      if (fileNameCt == 0)
        return EXIT_SUCCESS;

      child = fork ();
      if (child == NULLPROCESS)
        break;

      if (child == NOPROCESS)
        {
          fprintf (stderr, "Error %d (%s) forking in main\n",
                   errno, strerror (errno));
          exit (EXIT_FAILURE);
        }

      waitpid (child, (int *) NULL, 0);
    }
#else
#error "NON-BOGUS LIMITS NOT SUPPORTED?!?!"
#endif

  /*
   *  For every file specified in stdandard in
   *  (except as throttled for bogus reasons)...
   */
  for (loopCt = 0; loopCt < fileNameCt; loopCt++)
    {
      char *pzData;
      char *pzFile = apzNames[loopCt];

      if (access (pzFile, R_OK) != 0)
        {
          int erno = errno;
          fprintf (stderr, "Cannot access %s from %s\n\terror %d (%s)\n",
                   pzFile, getcwd ((char *) NULL, MAXPATHLEN),
                   erno, strerror (erno));
        }
      else if (pzData = loadFile (pzFile),
               (pzData != (char *) NULL))
        {

          if (strstr (pzData, zGnuLib) == (char *) NULL)
            process (pzData, pzDestDir, pzFile);

          free ((void *) pzData);
        }
    }

  return EXIT_SUCCESS;
}


char *
loadFile (pzFile)
     const char *pzFile;
{
  char *pzDta;
  size_t fileSize;

  {
    struct stat stbf;
    if (stat (pzFile, &stbf) != 0)
      {
        fprintf (stderr, "error %d (%s) stat-ing %s\n",
                 errno, strerror (errno), pzFile);
        return (char *) NULL;
      }
    fileSize = stbf.st_size;
  }
  if (fileSize == 0)
    return (char *) NULL;

  pzDta = (char *) malloc ((fileSize + 16) & ~0x00F);
  if (pzDta == (char *) NULL)
    {
      fprintf (stderr, "error:  could not malloc %d bytes\n",
               fileSize);
      exit (EXIT_FAILURE);
    }

  {
    FILE *fp = fopen (pzFile, "r");
    size_t sizeLeft = fileSize;
    char *readPtr = pzDta;

    if (fp == (FILE *) NULL)
      {
        fprintf (stderr, "error %d (%s) opening %s\n", errno,
                 strerror (errno), pzFile);
        free ((void *) pzDta);
        return (char *) NULL;
      }

    do
      {
        size_t sizeRead = fread ((void *) readPtr, 1, sizeLeft, fp);

        if (sizeRead == 0)
          {
            if (feof (fp))
              break;

            if (ferror (fp))
              {
                fprintf (stderr, "error %d (%s) reading %s\n", errno,
                         strerror (errno), pzFile);
                free ((void *) pzDta);
                fclose (fp);
                return (char *) NULL;
              }
          }

        readPtr += sizeRead;
        sizeLeft -= sizeRead;
      }
    while (sizeLeft != 0);

    *readPtr = '\0';
    fclose (fp);
    return pzDta;
  }
}


void
runCompiles ()
{
  tSCC zBadComp[] = "fixincl ERROR:  cannot compile %s regex for %s\n"
    "\texpr = `%s'\n" "\terror %s\n";
  tFixDesc *pFD = fixDescList;
  int fixCt = FIX_COUNT;
  tTestDesc *pTD;
  int tstCt;
  int reCt = REGEX_COUNT;
  const char *pzErr;
  regex_t *pRegex = (regex_t *) malloc (REGEX_COUNT * sizeof (regex_t));

  if (pRegex == (regex_t *) NULL)
    {
      fprintf (stderr, "fixincl ERROR:  cannot allocate %d bytes for regex\n",
               REGEX_COUNT * sizeof (regex_t));
      exit (EXIT_FAILURE);
    }

  re_set_syntax (RE_SYNTAX_EGREP);
  pzErr = re_compile_pattern (zInclQuote, strlen (zInclQuote),
                              &inclQuoteRegex);
  if (pzErr != (char *) NULL)
    {
      fprintf (stderr, zBadComp, "quoted include", "runCompiles",
               zInclQuote, pzErr);
      exit (EXIT_FAILURE);
    }

  /*
   *  FOR every fixup, ...
   */
  do
    {
      pTD = pFD->pTestDesc;
      tstCt = pFD->testCt;

      if (pFD->papzMachs != (const char**)NULL) {
        const char** papzMachs = pFD->papzMachs;
        char*        pz = zFileNameBuf;
        char*        pzSep = "";
        tCC*         pzIfTrue;
        tCC*         pzIfFalse;
        tSCC         zSkip[] = "skip";
        tSCC         zRun[]  = "run";

        sprintf( pz, "case %s in\n", pzMachine );
        pz += strlen( pz );

        if (pFD->fdFlags & FD_MACH_IFNOT) {
          pzIfTrue  = zSkip;
          pzIfFalse = zRun;
        } else {
          pzIfTrue  = zRun;
          pzIfFalse = zSkip;
        }

        for (;;) {
          const char* pzMach = *(papzMachs++);
          if (pzMach == (const char*)NULL)
            break;
          sprintf( pz, "%s  %s", pzSep, pzMach );
          pz += strlen( pz );
          pzSep = " | \\\n";
        }
        sprintf( pz, " )\n    echo %s ;;\n  * )\n    echo %s ;;\nesac",
                 pzIfTrue, pzIfFalse );
        pz = runShell( zFileNameBuf );
        if (*pz == 's') {
          pFD->fdFlags |= FD_SKIP_TEST;
          continue;
        }
      }

      /*
       *  FOR every test for the fixup, ...
       */
      while (--tstCt >= 0)
        {
          switch (pTD->type)
            {
            case TT_EGREP:
            case TT_NEGREP:
              if (--reCt < 0)
                {
                  fputs ("out of RE's\n", stderr);
                  exit (EXIT_FAILURE);
                }

              pTD->pTestRegex = pRegex++;
              pzErr = re_compile_pattern (pTD->pzTest,
                                          strlen (pTD->pzTest),
                                          pTD->pTestRegex);
              if (pzErr != (char *) NULL)
                {
                  fprintf (stderr, zBadComp, "select test", pFD->pzFixName,
                           pTD->pzTest, pzErr);
                  exit (EXIT_FAILURE);
                }
            }
          pTD++;
        }
    }
  while (pFD++, --fixCt > 0);
}


FILE *
createFile (pzFile)
     const char *pzFile;
{
  int fd;
  FILE *pf;
  char fname[MAXPATHLEN];

  sprintf (fname, "%s/%s", pzDestDir, pzFile);
  unlink (fname);

  fd = open (fname, O_WRONLY | O_CREAT);

  if ((fd < 0) && (errno == ENOENT))
    {
      char *pzDir = strchr (fname + 1, '/');
      struct stat stbf;

      while (pzDir != (char *) NULL)
        {
          *pzDir = NUL;
          if (stat (fname, &stbf) < 0)
            {
              mkdir (fname, S_IFDIR | S_IRWXU | S_IRGRP | S_IXGRP
                     | S_IROTH | S_IXOTH);
            }

          *pzDir = '/';
          pzDir = strchr (pzDir + 1, '/');
        }
      fd = open (fname, O_WRONLY | O_CREAT);
    }
  if (fd < 0)
    {
      fprintf (stderr, "Error %d (%s) creating %s\n",
               errno, strerror (errno), fname);
      exit (EXIT_FAILURE);
    }
  fprintf (stderr, "Fixed:  %s\n", pzFile);
  pf = fdopen (fd, "w");

#ifdef LATER
  {
    static const char zHdr[] =
    "/*\n"
    " *  DO NOT EDIT THIS FILE.\n"
    " *\n"
    " *  It has been auto-edited by fixincludes from /usr/include/%s\n"
    " *  This had to be done to correct non-standard usages in the\n"
    " *  original, manufacturer supplied header file.\n"
    " */\n\n";

    fprintf (pf, zHdr, pzFile);
  }
#endif
  return pf;
}

tSuccess
testTest (pTest, pzFile)
     tTestDesc *pTest;
     char*      pzFile;
{
  char *pzRes;
  tSuccess res = FAILURE;

  static char zCmdBuf[4096];
  tSCC zCmdFmt[] = "file=%s\nif ( test %s ) > /dev/null 2>&1\n"
  "then echo TRUE\n" "else echo FALSE\n" "fi";

  sprintf (zCmdBuf, zCmdFmt, pzFile, pTest->pzTest);
  pzRes = runShell (zCmdBuf);
  if (*pzRes == 'T')
    res = SUCCESS;
  free ((void *) pzRes);
  return res;
}


tSuccess
egrepTest (pzDta, pTest)
     char *pzDta;
     tTestDesc *pTest;
{
  regmatch_t match;
#ifndef NO_BOGOSITY
  if (pTest->pTestRegex == 0)
    fprintf (stderr, "fixincl ERROR RE not compiled:  `%s'\n", pTest->pzTest);
#endif
  if (regexec (pTest->pTestRegex, pzDta, 1, &match, 0) == 0)
    return SUCCESS;
  return FAILURE;
}



void
extractQuotedFiles (pzDta, pzFile, pMatch)
     char *pzDta;
     const char *pzFile;
     regmatch_t *pMatch;
{
  char *pzDirEnd = strrchr (pzFile, '/');
  char *pzInclQuot = pzDta;

  fprintf (stderr, "Quoted includes in %s\n", pzFile);

  /*
   *  Set "pzFile" to point to the containing subdirectory of the source
   *  If there is none, then it is in our current direcory, ".".
   */
  if (pzDirEnd == (char *) NULL)
    pzFile = ".";
  else
    *pzDirEnd = '\0';

  for (;;)
    {
      pzInclQuot += pMatch->rm_so;

      /*
       *  Skip forward to the included file name
       */
      while (isspace (*pzInclQuot))
        pzInclQuot++;
      while (isspace (*++pzInclQuot));
      pzInclQuot += sizeof ("include") - 1;
      while (*pzInclQuot++ != '"');

      /*
       *  Print the source directory and the subdirectory of the file
       *  in question.
       */
      printf ("%s  %s/", pzSrcDir, pzFile);
      pzDirEnd = pzInclQuot;

      /*
       *  Append to the directory the relative path of the desired file
       */
      while (*pzInclQuot != '"')
        putc (*pzInclQuot++, stdout);

      /*
       *  Now print the destination directory appended with the relative
       *  path of the desired file
       */
      printf ("  %s/%s/", pzDestDir, pzFile);
      while (*pzDirEnd != '"')
        putc (*pzDirEnd++, stdout);

      /*
       *  End of entry
       */
      putc ('\n', stdout);

      /*
       *  Find the next entry
       */
      if (regexec (&inclQuoteRegex, pzInclQuot, 1, pMatch, 0) != 0)
        break;
    }
}


/*
 *  Process the potential fixes for a particular include file
 */
void
process (pzDta, pzDir, pzFile)
     char *pzDta;
     const char *pzDir;
     const char *pzFile;
{
  static char zEnvFile[1024] =
    {"file="};
  tFixDesc *pFD = fixDescList;
  int todoCt = FIX_COUNT;
  tFdPair fdp =
    {-1, -1};

  /*
   *  IF this is the first time through,
   *  THEN put the 'file' environment variable into the environment.
   *       This is used by some of the subject shell scripts and tests.
   */
  if (zEnvFile[5] == NUL)
    putenv (zEnvFile);

  /*
   *  Ghastly as it is, this actually updates the value of the variable:
   *
   *    putenv(3C)             C Library Functions             putenv(3C)
   *
   *    DESCRIPTION
   *         putenv() makes the value of the  environment  variable  name
   *         equal  to value by altering an existing variable or creating
   *         a new one.  In either case, the string pointed to by  string
   *         becomes part of the environment, so altering the string will
   *         change the environment.  string points to a  string  of  the
   *         form  ``name=value.''  The space used by string is no longer
   *         used once a new string-defining name is passed to putenv().
   */
  strcpy (zEnvFile + 5, pzFile);
  chainHead = NOPROCESS;

  /*
   *  For every fix in our fix list, ...
   */
  for (; todoCt > 0; pFD++, todoCt--)
    {
      tTestDesc *pTD;
      int tstCt;
      tSuccess egrepRes;

      if (pFD->fdFlags & FD_SKIP_TEST)
        continue;

      /*
       *  IF there is a file name restriction,
       *  THEN ensure the current file name matches one in the pattern
       */
      if (pFD->pzFileList != (char *) NULL)
        {
          const char *pzFil = pzFile;
          const char *pzScn = pFD->pzFileList;
          size_t nmLen;

          while ((pzFil[0] == '.') && (pzFil[1] == '/'))
            pzFil += 2;
          nmLen = strlen (pzFil);

          for (;;)
            {
              pzScn = strstr (pzScn + 1, pzFil);
              if (pzScn == (char *) NULL)
                goto nextFix;

              if ((pzScn[-1] == '|') && (pzScn[nmLen] == '|'))
                break;
            }
        }

      egrepRes = PROBLEM;

      /*
       *  IF there are no tests
       *  THEN we always run the fixup
       */
      for (pTD = pFD->pTestDesc, tstCt = pFD->testCt;
           tstCt-- > 0;
           pTD++)
        {
          switch (pTD->type)
            {
            case TT_TEST:
              /*
               *  IF *any* of the shell tests fail,
               *  THEN do not process the fix.
               */
              if (!SUCCESSFUL (testTest (pTD, pzFile)))
                goto nextFix;
              break;

            case TT_EGREP:
              /*
               *  IF       we have not had a successful egrep test
               *    *AND*  this test does not pass,
               *  THEN mark the egrep test as failing.  It starts
               *       out as a "PROBLEM", meaning that if we do not
               *       encounter any egrep tests, then we will let it pass.
               */
              if ((!SUCCESSFUL (egrepRes))
                  && (!SUCCESSFUL (egrepTest (pzDta, pTD))))

                egrepRes = FAILURE;

              break;

            case TT_NEGREP:
              /*
               *  IF *any* of the negative egrep tests fail,
               *  THEN do not process the fix.
               */
              if (SUCCESSFUL (egrepTest (pzDta, pTD)))
                goto nextFix;
              break;
            }
        }

      /*
       *  IF there were no egrep tests *OR* at least one passed, ...
       */
      if (!FAILED (egrepRes))
        {
          fprintf (stderr, "Applying %-32s to %s\n",
                   pFD->pzFixName, pzFile);

          if (fdp.readFd == -1)
            {
              fdp.readFd = open (pzFile, O_RDONLY);
              if (fdp.readFd < 0)
                {
                  fprintf (stderr, "Error %d (%s) opening %s\n", errno,
                           strerror (errno), pzFile);
                  exit (EXIT_FAILURE);
                }
            }

          for (;;)
            {
              int newFd = chainOpen (fdp.readFd,
                                     (tpChar *) pFD->papzPatchArgs,
                                     (chainHead == -1)
                                     ? &chainHead : (pid_t *) NULL);
              if (newFd != -1)
                {
                  fdp.readFd = newFd;
                  break;
                }

              fprintf (stderr, "Error %d (%s) starting filter process "
                       "for %s\n", errno, strerror (errno),
                       pFD->pzFixName);

              if (errno != EAGAIN)
                exit (EXIT_FAILURE);
              sleep (1);
            }
        }

    nextFix:;
    }

  /*
   *  IF after all the tests we did not start any patch programs,
   *  THEN quit now.
   */
  if (fdp.readFd < 0)
    return;

  {
    FILE *inFp = fdopen (fdp.readFd, "r");
    FILE *oFp = (FILE *) NULL;
    char *pzCmp = pzDta;

    for (;;)
      {
        int ch;

        ch = getc (inFp);
        if (ch == EOF)
          break;

        if (oFp != (FILE *) NULL)
          putc (ch, oFp);

        else if (ch != *pzCmp)
          {
            oFp = createFile (pzFile);
            if (pzCmp != pzDta)
              {
                char c = *pzCmp;
                *pzCmp = NUL;
                fputs (pzDta, oFp);
                *pzCmp = c;
              }
            putc (ch, oFp);

          }
        else
          pzCmp++;
      }

    if (oFp != (FILE *) NULL)
      {
        regmatch_t match;

        fchmod (fileno (oFp), S_IRUSR | S_IRGRP | S_IROTH);
        fclose (oFp);
        if (regexec (&inclQuoteRegex, pzDta, 1, &match, 0) == 0)
          extractQuotedFiles (pzDta, pzFile, &match);
      }

    fclose (inFp);
  }

  close (fdp.readFd);
}
