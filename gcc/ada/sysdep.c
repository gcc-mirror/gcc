/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                S Y S D E P                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                            $Revision: 1.5.10.1 $
 *                                                                          *
 *          Copyright (C) 1992-2001 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains system dependent symbols that are referenced in the
   GNAT Run Time Library */

#ifdef __vxworks
#include "vxWorks.h"
#endif
#ifdef IN_RTS
#define POSIX
#include "tconfig.h"
#include "tsystem.h"
#include <fcntl.h>
#include <sys/stat.h>
#include "time.h"
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"

/*
   mode_read_text
   open text file for reading
   rt for DOS and Windows NT, r for Unix

   mode_write_text
   truncate to zero length or create text file for writing
   wt for DOS and Windows NT, w for Unix

   mode_append_text
   append; open or create text file for writing at end-of-file
   at for DOS and Windows NT, a for Unix

   mode_read_binary
   open binary file for reading
   rb for DOS and Windows NT, r for Unix

   mode_write_binary
   truncate to zero length or create binary file for writing
   wb for DOS and Windows NT, w for Unix

   mode_append_binary
   append; open or create binary file for writing at end-of-file
   ab for DOS and Windows NT, a for Unix

   mode_read_text_plus
   open text file for update (reading and writing)
   r+t for DOS and Windows NT, r+ for Unix

   mode_write_text_plus
   truncate to zero length or create text file for update
   w+t for DOS and Windows NT, w+ for Unix

   mode_append_text_plus
   append; open or create text file for update, writing at end-of-file
   a+t for DOS and Windows NT, a+ for Unix

   mode_read_binary_plus
   open binary file for update (reading and writing)
   r+b for DOS and Windows NT, r+ for Unix

   mode_write_binary_plus
   truncate to zero length or create binary file for update
   w+b for DOS and Windows NT, w+ for Unix

   mode_append_binary_plus
   append; open or create binary file for update, writing at end-of-file
   a+b for DOS and Windows NT, a+ for Unix

   Notes:

   (1) Opening a file with read mode fails if the file does not exist or
   cannot be read.

   (2) Opening a file with append mode causes all subsequent writes to the
   file to be forced to the then current end-of-file, regardless of
   intervening calls to the fseek function.

   (3) When a file is opened with update mode, both input and output may be
   performed on the associated stream.  However, output may not be directly
   followed by input without an intervening call to the fflush function or
   to a file positioning function (fseek, fsetpos, or rewind), and input
   may not be directly followed by output without an intervening call to a
   file positioning function, unless the input operation encounters
   end-of-file.

   The other target dependent declarations here are for the two functions
   __gnat_set_binary_mode and __gnat_set_text_mode:

      void __gnat_set_binary_mode (int handle);
      void __gnat_set_text_mode   (int handle);

   These functions have no effect in Unix (or similar systems where there is
   no distinction between binary and text files), but in DOS (and similar
   systems where text mode does CR/LF translation), these functions allow
   the mode of the stream with the given handle (fileno can be used to get
   the handle of a stream) to be changed dynamically. The returned result
   is 0 if no error occurs and -1 if an error occurs.

   Finally there is a boolean (character) variable

      char __gnat_text_translation_required;

   which is zero (false) in Unix mode, and one (true) in DOS mode, with a
   true value indicating that text translation is required on text files
   and that fopen supports the trailing t and b modifiers.

*/

#if defined(WINNT) || defined (MSDOS) || defined (__EMX__)
const char *mode_read_text = "rt";
const char *mode_write_text = "wt";
const char *mode_append_text = "at";
const char *mode_read_binary = "rb";
const char *mode_write_binary = "wb";
const char *mode_append_binary = "ab";
const char *mode_read_text_plus = "r+t";
const char *mode_write_text_plus = "w+t";
const char *mode_append_text_plus = "a+t";
const char *mode_read_binary_plus = "r+b";
const char *mode_write_binary_plus = "w+b";
const char *mode_append_binary_plus = "a+b";
const char __gnat_text_translation_required = 1;

void
__gnat_set_binary_mode (handle)
     int handle;
{
  _setmode (handle, O_BINARY);
}

void
__gnat_set_text_mode (handle)
     int handle;
{
  _setmode (handle, O_TEXT);
}

#ifdef __MINGW32__
#include <windows.h>

/* Return the name of the tty.   Under windows there is no name for
   the tty, so this function, if connected to a tty, returns the generic name
   "console".  */

char *
__gnat_ttyname (filedes)
     int filedes;
{
  if (isatty (filedes))
    return "console";
  else
    return NULL;
}

/* This function is needed to fix a bug under Win95/98. Under these plateforms
   doing :
                ch1 = getch();
		ch2 = fgetc (stdin);

   will put the same character into ch1 and ch2. It seem that the character
   read by getch() is not correctly removed from the buffer. Even a
   fflush(stdin) does not fix the bug. This bug does not appear under Window
   NT. So we have two version of this routine below one for 95/98 and one for
   NT/2000 version of Windows. There is also a special routine (winflushinit)
   that will be called only the first time to check which version of Windows
   we are running running on to set the right routine to use.

   This problem occurs when using Text_IO.Get_Line after Text_IO.Get_Immediate
   for example.

   Calling FlushConsoleInputBuffer just after getch() fix the bug under 
   95/98. */

static void winflush_init PARAMS ((void));

static void winflush_95 PARAMS ((void));

static void winflush_nt PARAMS ((void));

/* winflusfunction is set first to the winflushinit function which will check
   the OS version 95/98 or NT/2000 */

static void (*winflush_function) PARAMS ((void)) = winflush_init;

/* This function does the runtime check of the OS version and then sets
   winflush_function to the appropriate function and then call it. */ 

static void
winflush_init ()
{ 
  DWORD dwVersion = GetVersion();

  if (dwVersion < 0x80000000)                /* Windows NT/2000 */
    winflush_function = winflush_nt;
  else                                       /* Windows 95/98   */
    winflush_function = winflush_95;

  (*winflush_function)();      /* Perform the 'flush' */

}

static void winflush_95 ()
{ 
  FlushConsoleInputBuffer (GetStdHandle (STD_INPUT_HANDLE));
}

static void winflush_nt ()
{
  /* Does nothing as there is no problem under NT.  */
}
#endif

#else

const char *mode_read_text = "r";
const char *mode_write_text = "w";
const char *mode_append_text = "a";
const char *mode_read_binary = "r";
const char *mode_write_binary = "w";
const char *mode_append_binary = "a";
const char *mode_read_text_plus = "r+";
const char *mode_write_text_plus = "w+";
const char *mode_append_text_plus = "a+";
const char *mode_read_binary_plus = "r+";
const char *mode_write_binary_plus = "w+";
const char *mode_append_binary_plus = "a+";
const char __gnat_text_translation_required = 0;

/* These functions do nothing in non-DOS systems. */

void
__gnat_set_binary_mode (handle)
     int handle ATTRIBUTE_UNUSED;
{
}

void
__gnat_set_text_mode (handle)
     int handle ATTRIBUTE_UNUSED;
{
}
char *
__gnat_ttyname (filedes)
     int filedes;
{
#ifndef __vxworks
  extern char *ttyname PARAMS ((int));

  return ttyname (filedes);

#else
  return "";

#endif
}
#endif

#if defined (linux) || defined (sun) || defined (sgi) || defined (__EMX__) \
  || (defined (__osf__) && ! defined (__alpha_vxworks)) || defined (WINNT) \
  || defined (__MACHTEN__) || defined (__CYGWIN__)
#include <termios.h>

#else
#if defined (VMS)
extern char *decc$ga_stdscr;
static int initted = 0;
#endif
#endif

/* Implements the common processing for getc_immediate and
   getc_immediate_nowait. */

extern void getc_immediate		PARAMS ((FILE *, int *, int *));
extern void getc_immediate_nowait	PARAMS ((FILE *, int *, int *, int *));
extern void getc_immediate_common	PARAMS ((FILE *, int *, int *,
						 int *, int));

/* Called by Get_Immediate (Foo); */

void
getc_immediate (stream, ch, end_of_file)
     FILE *stream;
     int *ch;
     int *end_of_file;
{
  int avail;

  getc_immediate_common (stream, ch, end_of_file, &avail, 1);
}

/* Called by Get_Immediate (Foo, Available); */

void
getc_immediate_nowait (stream, ch, end_of_file, avail)
     FILE *stream;
     int *ch;
     int *end_of_file;
     int *avail;
{
  getc_immediate_common (stream, ch, end_of_file, avail, 0);
}

/* Called by getc_immediate () and getc_immediate_nowait () */

void
getc_immediate_common (stream, ch, end_of_file, avail, waiting)
     FILE *stream;
     int *ch;
     int *end_of_file;
     int *avail;
     int waiting;
{
#if defined (linux) || defined (sun) || defined (sgi) || defined (__EMX__) \
    || (defined (__osf__) && ! defined (__alpha_vxworks)) \
    || defined (__CYGWIN__) || defined (__MACHTEN__) 
  char c;
  int nread;
  int good_one = 0;
  int eof_ch = 4; /* Ctrl-D */
  int fd = fileno (stream);
  struct termios otermios_rec, termios_rec;

  if (isatty (fd))
    {
      tcgetattr (fd, &termios_rec);
      memcpy (&otermios_rec, &termios_rec, sizeof (struct termios));
      while (! good_one)
        {
          /* Set RAW mode */
          termios_rec.c_lflag = termios_rec.c_lflag & ~ICANON;
#if defined(sgi) || defined (sun) || defined (__EMX__) || defined (__osf__) \
      || defined (linux) || defined (__MACHTEN__)
          eof_ch = termios_rec.c_cc[VEOF];

          /* If waiting (i.e. Get_Immediate (Char)), set MIN = 1 and wait for
             a character forever. This doesn't seem to effect Ctrl-Z or
             Ctrl-C processing except on OS/2 where Ctrl-C won't work right
             unless we do a read loop. Luckily we can delay a bit between
             iterations. If not waiting (i.e. Get_Immediate (Char, Available)),
             don't wait for anything but timeout immediately. */
#ifdef __EMX__
          termios_rec.c_cc[VMIN] = 0;
          termios_rec.c_cc[VTIME] = waiting;
#else
          termios_rec.c_cc[VMIN] = waiting;
          termios_rec.c_cc[VTIME] = 0;
#endif
#endif
          tcsetattr (fd, TCSANOW, &termios_rec);

          /* Read() is used here instead of fread(), because fread() doesn't
             work on Solaris5 and Sunos4 in this situation.  Maybe because we
             are mixing calls that use file descriptors and streams. */

          nread = read (fd, &c, 1);
          if (nread > 0)
            {
              /* On Unix terminals, Ctrl-D (EOT) is an End of File. */
              if (c == eof_ch)
                {
                  *avail = 0;
                  *end_of_file = 1;
                  good_one = 1;
                }

              /* Everything else is ok */
              else if (c != eof_ch)
                {
                  *avail = 1;
                  *end_of_file = 0;
                  good_one = 1;
                }
            }

          else if (! waiting)
            {
              *avail = 0;
              *end_of_file = 0;
              good_one = 1;
            }
          else
            {
              good_one = 0;
            }
        }

      tcsetattr (fd, TCSANOW, &otermios_rec);
      *ch = c;
    }

  else
#else
#if defined (VMS)
  int fd = fileno (stream);

  if (isatty (fd))
    {
      if (initted == 0)
	{
	  decc$bsd_initscr ();
	  initted = 1;
	}
      decc$bsd_cbreak ();
      *ch = decc$bsd_wgetch (decc$ga_stdscr);

      if (*ch == 4)
	*end_of_file = 1;
      else
	*end_of_file = 0;

      *avail = 1;
      decc$bsd_nocbreak ();
    }
  else
#else
#if defined (__MINGW32__)
  int fd = fileno (stream);
  int char_waiting;
  int eot_ch = 4; /* Ctrl-D */

  if (isatty (fd))
    {
      if (waiting)
	{
	  *ch = getch();
	  (*winflush_function)();

	  if (*ch == eot_ch)
	    *end_of_file = 1;
	  else
	    *end_of_file = 0;

	  *avail = 1;
	}
      else /* ! waiting */
	{
	  char_waiting = kbhit();

	  if (char_waiting == 1)
	    {
	      *avail = 1;
	      *ch = getch();
	      (*winflush_function)();

	      if (*ch == eot_ch)
		*end_of_file = 1;
	      else
		*end_of_file = 0;
	    }
	  else
	    {
	      *avail = 0;
	      *end_of_file = 0;
	    }
	}
    }
  else
#endif
#endif
#endif
    {
      /* If we're not on a terminal, then we don't need any fancy processing.
	 Also this is the only thing that's left if we're not on one of the
	 supported systems. */
      *ch = fgetc (stream);
      if (feof (stream))
        {
          *end_of_file = 1;
          *avail = 0;
        }
      else
        {
          *end_of_file = 0;
          *avail = 1;
        }
    }
}

/* The following definitions are provided in NT to support Windows based
   Ada programs.  */

#ifdef WINNT
#include <windows.h>

/* Provide functions to echo the values passed to WinMain (windows bindings
   will want to import these).  We use the same names as the routines used
   by AdaMagic for compatibility.  */

char *rts_get_hInstance     PARAMS ((void));
char *rts_get_hPrevInstance PARAMS ((void));
char *rts_get_lpCommandLine PARAMS ((void));
int   rts_get_nShowCmd      PARAMS ((void));

char *
rts_get_hInstance () 
{ 
  return GetModuleHandleA (0); 
}

char *
rts_get_hPrevInstance () 
{ 
  return 0; 
}

char *
rts_get_lpCommandLine () 
{ 
  return GetCommandLineA (); 
}

int   
rts_get_nShowCmd () 
{ 
  return 1; 
}

#endif /* WINNT */
#ifdef VMS

/* This gets around a problem with using the old threads library on VMS 7.0. */

#include <time.h>

extern long get_gmtoff PARAMS ((void));

long
get_gmtoff ()
{
  time_t t;
  struct tm *ts;

  t = time ((time_t) 0);
  ts = localtime (&t);
  return ts->tm_gmtoff;
}
#endif

/* Definition of __gnat_locatime_r used by a-calend.adb */

#if defined (_AIX) || defined (__EMX__)
#define Lock_Task system__soft_links__lock_task
extern void (*Lock_Task) PARAMS ((void));

#define Unlock_Task system__soft_links__unlock_task
extern void (*Unlock_Task) PARAMS ((void));

/* Provide reentrant version of localtime on Aix and OS/2. Note that AiX does
   provide localtime_r, but in the library libc_r which doesn't get included
   systematically, so we can't use it. */

exrern void struct tm *__gnat_localtime_r PARAMS ((const time_t *,
						   struct tm *));

struct tm *
__gnat_localtime_r (timer, tp)
     const time_t *timer;
     struct tm *tp;
{
  struct tm *tmp;

  (*Lock_Task) ();
  tmp = localtime (timer);
  memcpy (tp, tmp, sizeof (struct tm));
  (*Unlock_Task) ();
  return tp;
}

#else
#if defined (__Lynx__) && defined (___THREADS_POSIX4ad4__)

/* As of LynxOS 3.1.0a patch level 040, LynuxWorks changes the
   prototype to the C library function localtime_r from the POSIX.4
   Draft 9 to the POSIX 1.c version. Before this change the following
   spec is required. Only use when ___THREADS_POSIX4ad4__ is defined,
   the Lynx convention when building against the legacy API. */

extern struct tm *__gnat_localtime_r PARAMS ((const time_t *, struct tm *));

struct tm *
__gnat_localtime_r (timer, tp)
     const time_t *timer;
     struct tm *tp;
{
  localtime_r (tp, timer);
  return NULL;
}

#else
#if defined (VMS) || defined (__MINGW32__)

/* __gnat_localtime_r is not needed on NT and VMS */

#else

/* All other targets provide a standard localtime_r */

extern struct tm *__gnat_localtime_r PARAMS ((const time_t *, struct tm *));

struct tm *
__gnat_localtime_r (timer, tp)
     const time_t *timer;
     struct tm *tp;
{
  return (struct tm *) localtime_r (timer, tp);
}
#endif
#endif
#endif
