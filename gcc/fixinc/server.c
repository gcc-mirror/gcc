
/*
 *  $Id: server.c,v 1.2 1998/12/16 21:19:16 law Exp $
 *
 *  Server Handling copyright 1992-1998 Bruce Korb
 *
 *  Server Handling is free software.
 *  You may redistribute it and/or modify it under the terms of the
 *  GNU General Public License, as published by the Free Software
 *  Foundation; either version 2, or (at your option) any later version.
 *
 *  Server Handling is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Server Handling.  See the file "COPYING".  If not,
 *  write to:  The Free Software Foundation, Inc.,
 *             59 Temple Place - Suite 330,
 *             Boston,  MA  02111-1307, USA.
 *
 * As a special exception, Bruce Korb gives permission for additional
 * uses of the text contained in his release of ServerHandler.
 *
 * The exception is that, if you link the ServerHandler library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the ServerHandler library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by Bruce Korb under
 * the name ServerHandler.  If you copy code from other sources under the
 * General Public License into a copy of ServerHandler, as the General Public
 * License permits, the exception does not apply to the code that you add
 * in this way.  To avoid misleading anyone as to the status of such
 * modified files, you must delete this exception notice from them.
 *
 * If you write modifications of your own for ServerHandler, it is your
 * choice whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/param.h>

#include "server.h"

#ifdef DEBUG
#define STATIC
#else
#define STATIC static
#endif
#ifndef tSCC
#define tSCC static const char
#endif
#ifndef NUL
#define NUL '\0'
#endif

STATIC bool readPipeTimeout;

STATIC tpChar defArgs[] =
{(char *) NULL, "-p", (char *) NULL};
STATIC tpfPair serverPair =
{(FILE *) NULL, (FILE *) NULL};
STATIC pid_t serverId = NULLPROCESS;
/*
 *  Arbitrary text that should not be found in the shell output.
 *  It must be a single line and appear verbatim at the start of
 *  the terminating output line.
 */
tSCC zDone[] = "ShElL-OuTpUt-HaS-bEeN-cOmPlEtEd";
STATIC tpChar pCurDir = (char *) NULL;

/*
 *  chainOpen
 *
 *  Given an FD for an inferior process to use as stdin,
 *  start that process and return a NEW FD that that process
 *  will use for its stdout.  Requires the argument vector
 *  for the new process and, optionally, a pointer to a place
 *  to store the child's process id.
 */
int
chainOpen (stdinFd, ppArgs, pChild)
     int stdinFd;
     tpChar *ppArgs;
     pid_t *pChild;
{
  tFdPair stdoutPair =
  {-1, -1};
  pid_t chId;
  char *pzCmd;

  /*
   *  Create a pipe it will be the child process' stdout,
   *  and the parent will read from it.
   */
  if ((pipe ((int *) &stdoutPair) < 0))
    {
      if (pChild != (pid_t *) NULL)
	*pChild = NOPROCESS;
      return -1;
    }

  /*
   *  If we did not get an arg list, use the default
   */
  if (ppArgs == (tpChar *) NULL)
    ppArgs = defArgs;

  /*
   *  If the arg list does not have a program,
   *  assume the "SHELL" from the environment, or, failing
   *  that, then sh.  Set argv[0] to whatever we decided on.
   */
  if (pzCmd = *ppArgs,
      (pzCmd == (char *) NULL) || (*pzCmd == '\0'))
    {

      pzCmd = getenv ("SHELL");
      if (pzCmd == (char *) NULL)
	pzCmd = "sh";
    }
#ifdef DEBUG_PRINT
  printf ("START:  %s\n", pzCmd);
  {
    int idx = 0;
    while (ppArgs[++idx] != (char *) NULL)
      printf ("  ARG %2d:  %s\n", idx, ppArgs[idx]);
  }
#endif
  /*
   *  Call fork() and see which process we become
   */
  chId = fork ();
  switch (chId)
    {
    case NOPROCESS:		/* parent - error in call */
      close (stdoutPair.readFd);
      close (stdoutPair.writeFd);
      if (pChild != (pid_t *) NULL)
	*pChild = NOPROCESS;
      return -1;

    default:			/* parent - return opposite FD's */
      if (pChild != (pid_t *) NULL)
	*pChild = chId;
#ifdef DEBUG_PRINT
      printf ("for pid %d:  stdin from %d, stdout to %d\n"
	      "for parent:  read from %d\n",
	      chId, stdinFd, stdoutPair.writeFd, stdoutPair.readFd);
#endif
      close (stdinFd);
      close (stdoutPair.writeFd);
      return stdoutPair.readFd;

    case NULLPROCESS:		/* child - continue processing */
      break;
    }

  /*
   *  Close the pipe end handed back to the parent process
   */
  close (stdoutPair.readFd);

  /*
   *  Close our current stdin and stdout
   */
  close (STDIN_FILENO);
  close (STDOUT_FILENO);

  /*
   *  Make the fd passed in the stdin, and the write end of
   *  the new pipe become the stdout.
   */
  fcntl (stdoutPair.writeFd, F_DUPFD, STDOUT_FILENO);
  fcntl (stdinFd, F_DUPFD, STDIN_FILENO);

  if (*ppArgs == (char *) NULL)
    *ppArgs = pzCmd;

  execvp (pzCmd, ppArgs);
  fprintf (stderr, "Error %d:  Could not execvp( '%s', ... ):  %s\n",
	   errno, pzCmd, strerror (errno));
  exit (EXIT_PANIC);
}


/*
 *  p2open
 *
 *  Given a pointer to an argument vector, start a process and
 *  place its stdin and stdout file descriptors into an fd pair
 *  structure.  The "writeFd" connects to the inferior process
 *  stdin, and the "readFd" connects to its stdout.  The calling
 *  process should write to "writeFd" and read from "readFd".
 *  The return value is the process id of the created process.
 */
pid_t
p2open (pPair, ppArgs)
     tFdPair *pPair;
     tpChar *ppArgs;
{
  pid_t chId;

  /*
   *  Create a bi-directional pipe.  Writes on 0 arrive on 1
   *  and vice versa, so the parent and child processes will
   *  read and write to opposite FD's.
   */
  if (pipe ((int *) pPair) < 0)
    return NOPROCESS;

  pPair->readFd = chainOpen (pPair->readFd, ppArgs, &chId);
  if (chId == NOPROCESS)
    close (pPair->writeFd);

  return chId;
}


/*
 *  p2fopen
 *
 *  Identical to "p2open()", except that the "fd"'s are "fdopen(3)"-ed
 *  into file pointers instead.
 */
pid_t
p2fopen (pfPair, ppArgs)
     tpfPair *pfPair;
     tpChar *ppArgs;
{
  tFdPair fdPair;
  pid_t chId = p2open (&fdPair, ppArgs);

  if (chId == NOPROCESS)
    return chId;

  pfPair->pfRead = fdopen (fdPair.readFd, "r");
  pfPair->pfWrite = fdopen (fdPair.writeFd, "w");
  return chId;
}


/*
 *  loadData
 *
 *  Read data from a file pointer (a pipe to a process in this context)
 *  until we either get EOF or we get a marker line back.
 *  The read data are stored in a malloc-ed string that is truncated
 *  to size at the end.  Input is assumed to be an ASCII string.
 */
STATIC char *
loadData (fp)
     FILE *fp;
{
  char *pzText;
  size_t textSize;
  char *pzScan;
  char zLine[1024];

  textSize = sizeof (zLine) * 2;
  pzScan = \
    pzText = malloc (textSize);

  if (pzText == (char *) NULL)
    return pzText;

  for (;;)
    {
      size_t usedCt;

      alarm (10);
      readPipeTimeout = BOOL_FALSE;
      if (fgets (zLine, sizeof (zLine), fp) == (char *) NULL)
	break;

      if (strncmp (zLine, zDone, sizeof (zDone) - 1) == 0)
	break;

      strcpy (pzScan, zLine);
      pzScan += strlen (zLine);
      usedCt = (size_t) (pzScan - pzText);

      if (textSize - usedCt < sizeof (zLine))
	{

	  size_t off = (size_t) (pzScan - pzText);
	  void *p;
	  textSize += 4096;
	  p = realloc ((void *) pzText, textSize);
	  if (p == (void *) NULL)
	    {
	      fprintf (stderr, "Failed to get 0x%08X bytes\n", textSize);
	      free ((void *) pzText);
	      return (char *) NULL;
	    }

	  pzText = (char *) p;
	  pzScan = pzText + off;
	}
    }

  alarm (0);
  if (readPipeTimeout)
    {
      free ((void *) pzText);
      return (char *) NULL;
    }

  while ((pzScan > pzText) && isspace (pzScan[-1]))
    pzScan--;
  *pzScan = NUL;
  return realloc ((void *) pzText, strlen (pzText) + 1);
}


/*
 *  SHELL SERVER PROCESS CODE
 */

#ifdef DONT_HAVE_SIGSEND
typedef enum
{
  P_ALL, P_PID, P_GID, P_UID, P_PGID, P_SID, P_CID
}
idtype_t;
typedef long id_t;

STATIC int
sigsend (idtype, id, sig)
     idtype_t idtype;
     id_t id;
     int sig;
{
  switch (idtype)
    {
    case P_PID:
      kill ((pid_t) id, sig);
      break;

    case P_ALL:
    case P_GID:
    case P_UID:
    case P_PGID:
    case P_SID:
    case P_CID:
      errno = EINVAL;
      return -1;
      /*NOTREACHED */
    }

  return 0;
}
#endif /* HAVE_SIGSEND */


STATIC void
closeServer ()
{
  kill( (pid_t) serverId, SIGKILL);
  serverId = NULLPROCESS;
  fclose (serverPair.pfRead);
  fclose (serverPair.pfWrite);
  serverPair.pfRead = serverPair.pfWrite = (FILE *) NULL;
}


struct sigaction savePipeAction;
struct sigaction saveAlrmAction;
struct sigaction currentAction;

STATIC void
sigHandler (signo)
     int signo;
{
  closeServer ();
  readPipeTimeout = BOOL_TRUE;
}


STATIC void
serverSetup ()
{
#ifndef SA_SIGINFO
#  define SA_SIGINFO 0
#else
  currentAction.sa_sigaction =
#endif
  currentAction.sa_handler   = sigHandler;
  currentAction.sa_flags     = SA_SIGINFO;
  sigemptyset( &currentAction.sa_mask );

  sigaction( SIGPIPE, &currentAction, &savePipeAction );
  sigaction( SIGALRM, &currentAction, &saveAlrmAction );
  atexit( &closeServer );

  fputs ("trap : INT\n", serverPair.pfWrite);
  fflush (serverPair.pfWrite);
  pCurDir = getcwd ((char *) NULL, MAXPATHLEN + 1);
}


char *
runShell (pzCmd)
     const char *pzCmd;
{
  tSCC zNil[] = "";

  /*
   *  IF the shell server process is not running yet,
   *  THEN try to start it.
   */
  if (serverId == NULLPROCESS)
    {
      serverId = p2fopen (&serverPair, defArgs);
      if (serverId > 0)
	serverSetup ();
    }

  /*
   *  IF it is still not running,
   *  THEN return the nil string.
   */
  if (serverId <= 0)
    return (char *) zNil;

  /*
   *  Make sure the process will pay attention to us,
   *  send the supplied command, and then
   *  have it output a special marker that we can find.
   */
  fprintf (serverPair.pfWrite, "\\cd %s\n%s\n\necho\necho %s\n",
	   pCurDir, pzCmd, zDone);
  fflush (serverPair.pfWrite);
  if (serverId == NULLPROCESS)
    return (char *) NULL;

  /*
   *  Now try to read back all the data.  If we fail due to either
   *  a sigpipe or sigalrm (timeout), we will return the nil string.
   */
  {
    char *pz = loadData (serverPair.pfRead);
    if (pz == (char *) NULL)
      {
	fprintf (stderr, "CLOSING SHELL SERVER - command failure:\n\t%s\n",
		 pzCmd);
	closeServer ();
	pz = (char *) zNil;
      }
    return pz;
  }
}
