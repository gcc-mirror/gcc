
/*
 *  server.c  Set up and handle communications with a server process.
 *
 *  Server Handling copyright 1992-1999 The Free Software Foundation
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
 * As a special exception, The Free Software Foundation gives
 * permission for additional uses of the text contained in his release
 * of ServerHandler.
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
 * This exception applies only to the code released by The Free
 * Software Foundation under the name ServerHandler.  If you copy code
 * from other sources under the General Public License into a copy of
 * ServerHandler, as the General Public License permits, the exception
 * does not apply to the code that you add in this way.  To avoid
 * misleading anyone as to the status of such modified files, you must
 * delete this exception notice from them.
 *
 * If you write modifications of your own for ServerHandler, it is your
 * choice whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */
#include "auto-host.h"

#include "gansidecl.h"
#include "system.h"
#include <signal.h>

#include "server.h"

/* If this particular system's header files define the macro `MAXPATHLEN',
   we happily take advantage of it; otherwise we use a value which ought
   to be large enough.  */
#ifndef MAXPATHLEN
# define MAXPATHLEN     4096
#endif

#ifndef STDIN_FILENO
# define STDIN_FILENO	0
#endif
#ifndef STDOUT_FILENO
# define STDOUT_FILENO	1
#endif

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

#if !defined(volatile) && !defined(HAVE_VOLATILE)
# define volatile
#endif

STATIC volatile t_bool read_pipe_timeout;

static t_pchar def_args[] =
{ (char *) NULL, (char *) NULL };
STATIC t_pf_pair server_pair =
{ (FILE *) NULL, (FILE *) NULL };
STATIC pid_t server_id = NULLPROCESS;
/*
 *  Arbitrary text that should not be found in the shell output.
 *  It must be a single line and appear verbatim at the start of
 *  the terminating output line.
 */
tSCC z_done[] = "ShElL-OuTpUt-HaS-bEeN-cOmPlEtEd";
STATIC t_pchar p_cur_dir = (char *) NULL;

/*
 *  load_data
 *
 *  Read data from a file pointer (a pipe to a process in this context)
 *  until we either get EOF or we get a marker line back.
 *  The read data are stored in a malloc-ed string that is truncated
 *  to size at the end.  Input is assumed to be an ASCII string.
 */
static char *
load_data (fp)
     FILE *fp;
{
  char *pz_text;
  size_t text_size;
  char *pz_scan;
  char z_line[1024];
  t_bool got_done = BOOL_FALSE;

  text_size = sizeof (z_line) * 2;
  pz_scan = pz_text = malloc (text_size);

  if (pz_text == (char *) NULL)
    return (char *) NULL;

  for (;;)
    {
      size_t used_ct;

      alarm (10);
      read_pipe_timeout = BOOL_FALSE;
      if (fgets (z_line, sizeof (z_line), fp) == (char *) NULL)
        break;

      if (strncmp (z_line, z_done, sizeof (z_done) - 1) == 0)
	{
	  got_done = BOOL_TRUE;
	  break;
	}

      strcpy (pz_scan, z_line);
      pz_scan += strlen (z_line);
      used_ct = (size_t) (pz_scan - pz_text);

      if (text_size - used_ct < sizeof (z_line))
        {
          size_t off = (size_t) (pz_scan - pz_text);
          void *p;
	  
          text_size += 4096;
          p = realloc ((void *) pz_text, text_size);
          if (p == (void *) NULL)
            {
              fprintf (stderr, "Failed to get 0x%08lX bytes\n",
                      (long) text_size);
              free ((void *) pz_text);
              return (char *) NULL;
            }
          pz_text = (char *) p;
          pz_scan = pz_text + off;
        }
    }

  alarm (0);
  if (read_pipe_timeout || ! got_done)
    {
      free ((void *) pz_text);
      return (char *) NULL;
    }

  while ((pz_scan > pz_text) && ISSPACE (pz_scan[-1]))
    pz_scan--;
  *pz_scan = NUL;
  return realloc ((void *) pz_text, strlen (pz_text) + 1);
}


/*
 *  close_server
 *
 *  Make certain the server process is dead, close the 
 *  pipes to it and from it, finally NULL out the file pointers
 */
void
close_server ()
{
  if (server_id != NULLPROCESS)
    {
      kill ((pid_t) server_id, SIGKILL);
      server_id = NULLPROCESS;
      fclose (server_pair.pf_read);
      fclose (server_pair.pf_write);
      server_pair.pf_read = server_pair.pf_write = (FILE *) NULL;
    }
}

/*
 *  sig_handler really only handles the timeout and pipe signals.
 *  This ensures that we do not wait forever on a request
 *  to our server, and also that if the server dies, we do not
 *  die from a sigpipe problem.
 */
static void
sig_handler (signo)
     int signo;
{
#ifdef DEBUG
  /* FIXME: this is illegal to do in a signal handler.  */
  fprintf (stderr,
          "fixincl ERROR:  sig_handler: killed pid %ld due to %s\n",
          (long) server_id, signo == SIGPIPE ? "SIGPIPE" : "SIGALRM");
#endif
  close_server ();
  read_pipe_timeout = BOOL_TRUE;
}


/*
 *  server_setup  Establish the signal handler for PIPE and ALARM.
 *  Also establishes the current directory to give to the
 *  server process at the start of every server command.
 */
static void
server_setup ()
{
  static int atexit_done = 0;
  
  if (atexit_done++ == 0)
    atexit (close_server);

  signal (SIGPIPE, sig_handler);
  signal (SIGALRM, sig_handler);

  fputs ("trap : 1\n", server_pair.pf_write);
  fflush (server_pair.pf_write);
  p_cur_dir = getcwd ((char *) NULL, MAXPATHLEN + 1);
}


/*
 *  run_shell
 *
 *  Run a shell command on the server.  The command string
 *  passed in is wrapped inside the sequence:
 *
 *     cd <original directory>
 *     <command string>
 *     echo
 *     echo <end-of-command-marker>
 *
 *  This ensures that all commands start at a known place in
 *  the directory structure, that any incomplete output lines
 *  are completed and that our special marker sequence appears on
 *  a line by itself.  We have chosen a marker that is
 *  excessively unlikely to be reproduced in normal output:
 *
 *     "ShElL-OuTpUt-HaS-bEeN-cOmPlEtEd"
 */
char *
run_shell (pz_cmd)
     const char *pz_cmd;
{
  t_bool retry = BOOL_TRUE;

 do_retry:
  /*  IF the shell server process is not running yet,
      THEN try to start it.  */
  if (server_id == NULLPROCESS)
    {
      server_id = proc2_fopen (&server_pair, def_args);
      if (server_id > 0)
        server_setup ();
    }

  /*  IF it is still not running, THEN return the nil string.  */
  if (server_id <= 0)
    {
      char *pz = (char *) malloc (1);
      
      if (pz != (char *) NULL)
        *pz = '\0';
      return pz;
    }

  /*  Make sure the process will pay attention to us, send the
     supplied command, and then have it output a special marker that
     we can find.  */
  fprintf (server_pair.pf_write, "cd %s\n%s\n\necho\necho %s\n",
           p_cur_dir, pz_cmd, z_done);
  fflush (server_pair.pf_write);

  /*  IF the server died and we received a SIGPIPE,
      THEN return an empty string.  */
  if (server_id == NULLPROCESS)
    {
      char *pz = (char *) malloc (1);
      
      if (pz != (char *) NULL)
        *pz = '\0';
      return pz;
    }

  /*  Now try to read back all the data.  If we fail due to either a
     sigpipe or sigalrm (timeout), we will return the nil string.  */
  {
    char *pz = load_data (server_pair.pf_read);
    
    if (pz == (char *) NULL)
      {
	close_server ();

	if (retry)
	  {
	    retry = BOOL_FALSE;
	    goto do_retry;
	  }

        fprintf (stderr, "CLOSING SHELL SERVER - command failure:\n\t%s\n",
                 pz_cmd);
        pz = (char *) malloc (1);
        if (pz != (char *) NULL)
          *pz = '\0';
      }
    return pz;
  }
}
