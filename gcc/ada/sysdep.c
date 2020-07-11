/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                S Y S D E P                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 1992-2020, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains system dependent symbols that are referenced in the
   GNAT Run Time Library */

#ifdef __vxworks
#include "vxWorks.h"
#include "ioLib.h"
#if ! defined (VTHREADS)
#include "dosFsLib.h"
#endif
#if ! defined (__RTP__) && (! defined (VTHREADS) || defined (__VXWORKSMILS__))
# include "nfsLib.h"
#endif
#include "selectLib.h"
#include "version.h"
#if defined (__RTP__)
#  include "vwModNum.h"
#endif /* __RTP__ */
#endif

#ifdef __ANDROID__
#undef __linux__
#endif

#ifdef IN_RTS
#define POSIX
#include "runtime.h"
#include <string.h>
#include <unistd.h>

#include <fcntl.h>
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

#include <time.h>
#include <errno.h>

#if defined (__sun__) && !defined (__vxworks)
/* The declaration is present in <time.h> but conditionalized
   on a couple of macros we don't define.  */
extern struct tm *localtime_r(const time_t *, struct tm *);
#endif

#include "adaint.h"

/* Don't use macros versions of this functions on VxWorks since they cause
   imcompatible changes in some VxWorks versions */
#ifdef __vxworks
#undef getchar
#undef putchar
#undef feof
#undef ferror
#undef fileno
#endif

/*
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

   The other target dependent declarations here are for the three functions
   __gnat_set_binary_mode, __gnat_set_text_mode and __gnat_set_mode:

      void __gnat_set_binary_mode (int handle);
      void __gnat_set_text_mode   (int handle);
      void __gnat_set_mode        (int handle, int mode);

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

#if defined (WINNT) || defined (__CYGWIN__) || defined (__DJGPP__)

const char __gnat_text_translation_required = 1;

#ifdef __CYGWIN__
#define WIN_SETMODE setmode
#include <io.h>
#else
#define WIN_SETMODE _setmode
#endif

#if defined (__DJGPP__)
#include <io.h>
#define _setmode setmode
#endif /* __DJGPP__ */

void
__gnat_set_binary_mode (int handle)
{
  WIN_SETMODE (handle, O_BINARY);
}

void
__gnat_set_text_mode (int handle)
{
  WIN_SETMODE (handle, O_TEXT);
}

#if defined (__CYGWIN__) || defined (__DJGPP__)
void
__gnat_set_mode (int handle, int mode)
{
  /*  the values here must be synchronized with
      System.File_Control_Block.Content_Encodding:

      None         = 0
      Default_Text = 1
      Text         = 2
      U8text       = 3
      Wtext        = 4
      U16text      = 5  */

 switch (mode) {
    case 0 : setmode(handle, O_BINARY);          break;
    case 1 : setmode(handle, O_TEXT);            break;
    case 2 : setmode(handle, O_TEXT);            break;
    case 3 : setmode(handle, O_TEXT);            break;
    case 4 : setmode(handle, O_BINARY);          break;
    case 5 : setmode(handle, O_BINARY);          break;
 }
}
#else
void
__gnat_set_mode (int handle, int mode)
{
  /*  the values here must be synchronized with
      System.File_Control_Block.Content_Encodding:

      None         = 0
      Default_Text = 1
      Text         = 2
      U8text       = 3
      Wtext        = 4
      U16text      = 5  */

 switch (mode) {
    case 0 : WIN_SETMODE (handle, _O_BINARY);          break;
    case 1 : WIN_SETMODE (handle, __gnat_current_ccs_encoding); break;
    case 2 : WIN_SETMODE (handle, _O_TEXT);            break;
    case 3 : WIN_SETMODE (handle, _O_U8TEXT);          break;
    case 4 : WIN_SETMODE (handle, _O_WTEXT);           break;
    case 5 : WIN_SETMODE (handle, _O_U16TEXT);         break;
 }
}
#endif

#ifdef __CYGWIN__

char *
__gnat_ttyname (int filedes)
{
  extern char *ttyname (int);

  return ttyname (filedes);
}

#endif /* __CYGWIN__ */

#if defined (__CYGWIN__) || defined (__MINGW32__)
#include <windows.h>

int __gnat_is_windows_xp (void);

int
__gnat_is_windows_xp (void)
{
  static int is_win_xp=0, is_win_xp_checked=0;

  if (!is_win_xp_checked)
    {
      OSVERSIONINFO version;

      is_win_xp_checked = 1;

      memset (&version, 0, sizeof (version));
      version.dwOSVersionInfoSize = sizeof (version);

      is_win_xp = GetVersionEx (&version)
        && version.dwPlatformId == VER_PLATFORM_WIN32_NT
        && (version.dwMajorVersion > 5
            || (version.dwMajorVersion == 5 && version.dwMinorVersion >= 1));
    }
  return is_win_xp;
}

/* Get the bounds of the stack.  The stack pointer is supposed to be
   initialized to BASE when a thread is created and the stack can be extended
   to LIMIT before reaching a guard page.
   Note: for the main thread, the system automatically extend the stack, so
   LIMIT is only the current limit.  */

void
__gnat_get_stack_bounds (void **base, void **limit)
{
  NT_TIB *tib;

  /* We know that the first field of the TEB is the TIB.  */
  tib = (NT_TIB *)NtCurrentTeb ();

  *base = tib->StackBase;
  *limit = tib->StackLimit;
}

#endif /* __CYGWIN__ || __MINGW32__ */

#ifdef __MINGW32__

/* Return the name of the tty.   Under windows there is no name for
   the tty, so this function, if connected to a tty, returns the generic name
   "console".  */

char *
__gnat_ttyname (int filedes)
{
  if (isatty (filedes))
    return "console";
  else
    return NULL;
}

#endif /* __MINGW32__ */

#else

const char __gnat_text_translation_required = 0;

/* These functions do nothing in non-DOS systems. */

void
__gnat_set_binary_mode (int handle ATTRIBUTE_UNUSED)
{
}

void
__gnat_set_text_mode (int handle ATTRIBUTE_UNUSED)
{
}

void
__gnat_set_mode (int handle ATTRIBUTE_UNUSED, int mode ATTRIBUTE_UNUSED)
{
}

char *
__gnat_ttyname (int filedes ATTRIBUTE_UNUSED)
{
#if defined (__vxworks)
  return "";
#else
  extern char *ttyname (int);

  return ttyname (filedes);
#endif /* defined (__vxworks) */
}
#endif

#if defined (__linux__) || defined (__sun__) \
  || defined (WINNT) \
  || defined (__MACHTEN__) || defined (__hpux__) || defined (_AIX) \
  || (defined (__svr4__) && defined (__i386__)) || defined (__Lynx__) \
  || defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
  || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
  || defined (__QNX__)

# ifdef __MINGW32__
#  if OLD_MINGW
#   include <termios.h>
#  else
#   include <conio.h>  /* for getch(), kbhit() */
#  endif
# else
#  include <termios.h>
# endif

#endif

/* Implements the common processing for getc_immediate and
   getc_immediate_nowait. */

extern void getc_immediate (FILE *, int *, int *);
extern void getc_immediate_nowait (FILE *, int *, int *, int *);
extern void getc_immediate_common (FILE *, int *, int *, int *, int);

/* Called by Get_Immediate (Foo); */

void
getc_immediate (FILE *stream, int *ch, int *end_of_file)
{
  int avail;

  getc_immediate_common (stream, ch, end_of_file, &avail, 1);
}

/* Called by Get_Immediate (Foo, Available); */

void
getc_immediate_nowait (FILE *stream, int *ch, int *end_of_file, int *avail)
{
  getc_immediate_common (stream, ch, end_of_file, avail, 0);
}

/* Called by getc_immediate () and getc_immediate_nowait () */

void
getc_immediate_common (FILE *stream,
                       int *ch,
                       int *end_of_file,
                       int *avail,
                       int waiting ATTRIBUTE_UNUSED)
{
#if defined (__linux__) || defined (__sun__) \
    || defined (__CYGWIN32__) || defined (__MACHTEN__) || defined (__hpux__) \
    || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
    || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
    || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
    || defined (__QNX__)
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

      /* Set RAW mode, with no echo */
      termios_rec.c_lflag = termios_rec.c_lflag & ~ICANON & ~ECHO;

#if defined (__linux__) || defined (__sun__) \
    || defined (__MACHTEN__) || defined (__hpux__) \
    || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
    || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
    || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
    || defined (__QNX__)
      eof_ch = termios_rec.c_cc[VEOF];

      /* If waiting (i.e. Get_Immediate (Char)), set MIN = 1 and wait for
         a character forever. This doesn't seem to effect Ctrl-Z or
         Ctrl-C processing.
         If not waiting (i.e. Get_Immediate (Char, Available)),
         don't wait for anything but timeout immediately. */
      termios_rec.c_cc[VMIN] = waiting;
      termios_rec.c_cc[VTIME] = 0;
#endif
      tcsetattr (fd, TCSANOW, &termios_rec);

      while (! good_one)
        {
          /* Read is used here instead of fread, because fread doesn't
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
	    good_one = 0;
        }

      tcsetattr (fd, TCSANOW, &otermios_rec);
      *ch = c;
    }

  else
#elif defined (__MINGW32__)
  int fd = fileno (stream);
  int char_waiting;
  int eot_ch = 4; /* Ctrl-D */

  if (isatty (fd))
    {
      if (waiting)
	{
	  *ch = getch ();

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
	      *ch = getch ();

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
#elif defined (__vxworks)
  /* Bit masks of file descriptors to read from.  */
  struct fd_set readFds;
  /* Timeout before select returns if nothing can be read.  */
  struct timeval timeOut;
  char c;
  int fd = fileno (stream);
  int nread;
  int option;
  int readable;
  int status;
  int width;

  if (isatty (fd))
    {
      /* If we do not want to wait, we have to set up fd in RAW mode. This
	 should be done outside this function as setting fd in RAW mode under
	 vxWorks flushes the buffer of fd. If the RAW mode was set here, the
	 buffer would be empty and we would always return that no character
	 is available */
      if (! waiting)
	{
	  /* Initialization of timeOut for its use with select.  */
	  timeOut.tv_sec  = 0;
	  timeOut.tv_usec = 0;

	  /* Initialization of readFds for its use with select;
	     FD is the only file descriptor to be monitored */
	  FD_ZERO (&readFds);
	  FD_SET (fd, &readFds);
	  width = 2;

	  /* We do all this processing to emulate a non blocking read.  */
	  readable = select (width, &readFds, NULL, NULL, &timeOut);
	  if (readable == ERROR)
	    *avail = -1, *end_of_file = -1;
	  /* No character available in input.  */
	  else if (readable == 0)
	    *avail = 0, *end_of_file = 0;
	  else
	    {
	      nread = read (fd, &c, 1);
	      if (nread > 0)
		*avail = 1, *end_of_file = 0;
	      /* End Of File. */
	      else if (nread == 0)
		*avail = 0, *end_of_file = 1;
	      /* Error.  */
	      else
		*avail = -1, *end_of_file = -1;
	    }
	}

      /* We have to wait until we get a character */
      else
	{
	  *avail = -1;
	  *end_of_file = -1;

	  /* Save the current mode of FD.  */
	  option = ioctl (fd, FIOGETOPTIONS, 0);

	  /* Set FD in RAW mode.  */
	  status = ioctl (fd, FIOSETOPTIONS, OPT_RAW);
	  if (status != -1)
	    {
	      nread = read (fd, &c, 1);
	      if (nread > 0)
		*avail = 1, *end_of_file = 0;
	      /* End of file.  */
	      else if (nread == 0)
		*avail = 0, *end_of_file = 1;
	      /* Else there is an ERROR.  */
	    }

	  /* Revert FD to its previous mode. */
	  status = ioctl (fd, FIOSETOPTIONS, option);
	}

      *ch = c;
    }
  else
#endif
    {
      /* If we're not on a terminal, then we don't need any fancy processing.
	 Also this is the only thing that's left if we're not on one of the
	 supported systems; which means that for non supported systems,
         get_immediate may wait for a carriage return on terminals. */
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

char *rts_get_hInstance (void);
char *rts_get_hPrevInstance (void);
char *rts_get_lpCommandLine (void);
int   rts_get_nShowCmd (void);

char *
rts_get_hInstance (void)
{
  return (char *)GetModuleHandleA (0);
}

char *
rts_get_hPrevInstance (void)
{
  return 0;
}

char *
rts_get_lpCommandLine (void)
{
  return GetCommandLineA ();
}

int
rts_get_nShowCmd (void)
{
  return 1;
}

#endif /* WINNT */

/* This value is returned as the time zone offset when a valid value
   cannot be determined. It is simply a bizarre value that will never
   occur. It is 3 days plus 73 seconds (offset is in seconds). */

long __gnat_invalid_tzoff = 259273;

/* Definition of __gnat_localtime_r used by a-calend.adb */

#if defined (__MINGW32__)

/* Reentrant localtime for Windows. */

extern void
__gnat_localtime_tzoff (const time_t *, const int *, long *);

static const unsigned long long w32_epoch_offset = 11644473600ULL;
void
__gnat_localtime_tzoff (const time_t *timer, const int *is_historic, long *off)
{
  TIME_ZONE_INFORMATION tzi;

  DWORD tzi_status;

  tzi_status = GetTimeZoneInformation (&tzi);

  /* Cases where we simply want to extract the offset of the current time
     zone, regardless of the date. A value of "0" for flag "is_historic"
     signifies that the date is NOT historic, see the
     body of Ada.Calendar.UTC_Time_Offset. */

  if (*is_historic == 0) {
    *off = tzi.Bias;

    /* The system is operating in the range covered by the StandardDate
       member. */
    if (tzi_status == TIME_ZONE_ID_STANDARD) {
       *off = *off + tzi.StandardBias;
    }

    /* The system is operating in the range covered by the DaylightDate
       member. */
    else if (tzi_status == TIME_ZONE_ID_DAYLIGHT) {
       *off = *off + tzi.DaylightBias;
    }

    *off = *off * -60;
  }

  /* Time zone offset calculations for a historic or future date */

  else {
    union
    {
      FILETIME ft_time;
      unsigned long long ull_time;
    } utc_time, local_time;

    SYSTEMTIME utc_sys_time, local_sys_time;
    BOOL status;

    /* First convert unix time_t structure to windows FILETIME format.  */
    utc_time.ull_time = ((unsigned long long) *timer + w32_epoch_offset)
                        * 10000000ULL;

    /* If GetTimeZoneInformation does not return a value between 0 and 2 then
       it means that we were not able to retrieve timezone information. Note
       that we cannot use here FileTimeToLocalFileTime as Windows will use in
       always in this case the current timezone setting. As suggested on MSDN
       we use the following three system calls to get the right information.
       Note also that starting with Windows Vista new functions are provided
       to get timezone settings that depend on the year. We cannot use them as
       we still support Windows XP and Windows 2003.  */

    status = tzi_status <= 2
      && FileTimeToSystemTime (&utc_time.ft_time, &utc_sys_time)
      && SystemTimeToTzSpecificLocalTime (&tzi, &utc_sys_time, &local_sys_time)
      && SystemTimeToFileTime (&local_sys_time, &local_time.ft_time);

    /* An error has occurred, return invalid_tzoff */

    if (!status) {
      *off = __gnat_invalid_tzoff;
    }
    else {
      if (local_time.ull_time > utc_time.ull_time) {
        *off = (long) ((local_time.ull_time - utc_time.ull_time)
               / 10000000ULL);
      }
      else {
        *off = - (long) ((utc_time.ull_time - local_time.ull_time)
               / 10000000ULL);
      }
    }
  }
}

#elif defined (__Lynx__)

/* On Lynx, all time values are treated in GMT */

/* As of LynxOS 3.1.0a patch level 040, LynuxWorks changes the
   prototype to the C library function localtime_r from the POSIX.4
   Draft 9 to the POSIX 1.c version. Before this change the following
   spec is required. Only use when ___THREADS_POSIX4ad4__ is defined,
   the Lynx convention when building against the legacy API. */

extern void
__gnat_localtime_tzoff (const time_t *, const int *, long *);

void
__gnat_localtime_tzoff (const time_t *timer, const int *is_historic, long *off)
{
  *off = 0;
}

#else

/* Other targets except Lynx and Windows provide a standard localtime_r */

#define Lock_Task system__soft_links__lock_task
extern void (*Lock_Task) (void);

#define Unlock_Task system__soft_links__unlock_task
extern void (*Unlock_Task) (void);

extern void
__gnat_localtime_tzoff (const time_t *, const int *, long *);

void
__gnat_localtime_tzoff (const time_t *timer ATTRIBUTE_UNUSED,
			const int *is_historic ATTRIBUTE_UNUSED,
			long *off ATTRIBUTE_UNUSED)
{
  struct tm tp ATTRIBUTE_UNUSED;

/* AIX, HPUX, Sun Solaris */
#if defined (_AIX) || defined (__hpux__) || defined (__sun__)
{
  (*Lock_Task) ();

  localtime_r (timer, &tp);
  *off = (long) -timezone;

  (*Unlock_Task) ();

  /* Correct the offset if Daylight Saving Time is in effect */

  if (tp.tm_isdst > 0)
    *off = *off + 3600;
}

/* VxWorks */
#elif defined (__vxworks)
#include <stdlib.h>
{
  (*Lock_Task) ();

  localtime_r (timer, &tp);

  /* Try to read the environment variable TIMEZONE. The variable may not have
     been initialize, in that case return an offset of zero (0) for UTC. */

  char *tz_str = getenv ("TIMEZONE");

  if ((tz_str == NULL) || (*tz_str == '\0'))
    *off = 0;
  else
  {
    char *tz_start, *tz_end;

    /* The format of the data contained in TIMEZONE is N::U:S:E where N is the
       name of the time zone, U are the minutes difference from UTC, S is the
       start of DST in mmddhh and E is the end of DST in mmddhh. Extracting
       the value of U involves setting two pointers, one at the beginning and
       one at the end of the value. The end pointer is then set to null in
       order to delimit a string slice for atol to process. */

    tz_start = index (tz_str, ':') + 2;
    tz_end = index (tz_start, ':');
    *tz_end = '\0';

    /* The Ada layer expects an offset in seconds. Note that we must reverse
       the sign of the result since west is positive and east is negative on
       VxWorks targets. */

    *off = -atol (tz_start) * 60;

    /* Correct the offset if Daylight Saving Time is in effect */

    if (tp.tm_isdst > 0)
      *off = *off + 3600;
  }

  (*Unlock_Task) ();
}

/* Darwin, Free BSD, Linux, where component tm_gmtoff is present in
   struct tm */

#elif defined (__APPLE__) || defined (__FreeBSD__) || defined (__linux__) \
  || defined (__GLIBC__) || defined (__DragonFly__) || defined (__OpenBSD__) \
  || defined (__DJGPP__) || defined (__QNX__)
{
  localtime_r (timer, &tp);
  *off = tp.tm_gmtoff;
}

/* Default: treat all time values in GMT */

#else
  *off = 0;

#endif  /* defined(_AIX) ... */
}

#endif

#ifdef __vxworks

#include <taskLib.h>

/* __gnat_get_task_options is used by s-taprop.adb only for VxWorks. This
   function returns the options to be set when creating a new task. It fetches
   the options assigned to the current task (parent), so offering some user
   level control over the options for a task hierarchy. It forces VX_FP_TASK
   because it is almost always required. On processors with the SPE
   category, VX_SPE_TASK should be used instead to enable the SPE. */
extern int __gnat_get_task_options (void);

int
__gnat_get_task_options (void)
{
  int options;

  /* Get the options for the task creator */
  taskOptionsGet (taskIdSelf (), &options);

  /* Force VX_FP_TASK or VX_SPE_TASK as needed */
#if defined (__SPE__)
  options |= VX_SPE_TASK;
#else
  options |= VX_FP_TASK;
#endif

  /* Mask those bits that are not under user control */
#ifdef VX_USR_TASK_OPTIONS
  /* O810-007, TSR 00043679:
     Workaround a bug in Vx-7 where VX_DEALLOC_TCB == VX_PRIVATE_UMASK and:
     - VX_DEALLOC_TCB is an internal option not to be used by users
     - VX_PRIVATE_UMASK as a user-definable option
     This leads to VX_USR_TASK_OPTIONS allowing 0x8000 as VX_PRIVATE_UMASK but
     taskCreate refusing this option (VX_DEALLOC_TCB is not allowed)

     Note that the same error occurs in both RTP and Kernel mode, but
     VX_DEALLOC_TCB is not defined in the RTP headers, so we need to
     explicitely check if VX_PRIVATE_UMASK has value 0x8000
  */
# if defined (VX_PRIVATE_UMASK) && (0x8000 == VX_PRIVATE_UMASK)
  options &= ~VX_PRIVATE_UMASK;
# endif
  options &= VX_USR_TASK_OPTIONS;
#endif
  return options;
}

#endif

int
__gnat_is_file_not_found_error (int errno_val)
 {
    /* WARNING: Do not rewrite this as a switch/case statement.
     * Some of the "cases" are duplicated in some versions of
     * Vxworks, notably VxWorks7r2 SR0610.  */
    if (errno_val == ENOENT)
      return 1;
#ifdef __vxworks
    /* In the case of VxWorks, we also have to take into account various
     * filesystem-specific variants of this error.
     */
#if ! defined (VTHREADS) && (_WRS_VXWORKS_MAJOR < 7)
    else if (errno_val == S_dosFsLib_FILE_NOT_FOUND)
      return 1;
#endif
#if ! defined (__RTP__) && (! defined (VTHREADS) || defined (__VXWORKSMILS__))
    else if (errno_val ==  S_nfsLib_NFSERR_NOENT)
      return 1;
#endif
#if defined (__RTP__)
    /* An RTP can return an NFS file not found, and the NFS bits must
       first be masked on to check the errno.  */
    else if (errno_val == (M_nfsStat | ENOENT))
      return 1;
#endif
#endif
    else
      return 0;
}

#if defined (__linux__)

/* Note well: If this code is modified, it should be tested by hand,
   because automated testing doesn't exercise it.
*/

/* HAVE_CAPABILITY is supposed to be defined if sys/capability.h exists on the
   system where this is being compiled. If this macro is defined, we #include
   the header. Otherwise we have the relevant declarations textually here.
*/

#if defined (HAVE_CAPABILITY)
#include <sys/capability.h>
#else

/* HAVE_CAPABILITY is not defined, so sys/capability.h does might not exist. */

typedef struct _cap_struct *cap_t;
typedef enum {
    CAP_CLEAR=0,
    CAP_SET=1
} cap_flag_value_t;
#define CAP_SYS_NICE         23
typedef enum {
    CAP_EFFECTIVE=0,                        /* Specifies the effective flag */
    CAP_PERMITTED=1,                        /* Specifies the permitted flag */
    CAP_INHERITABLE=2                     /* Specifies the inheritable flag */
} cap_flag_t;

typedef int cap_value_t;

extern cap_t   cap_get_proc(void);
extern int     cap_get_flag(cap_t, cap_value_t, cap_flag_t, cap_flag_value_t *);
extern int     cap_free(void *);

#endif

/* __gnat_has_cap_sys_nice returns 1 if the current process has the
   CAP_SYS_NICE capability. This capability is necessary to use the
   Ceiling_Locking policy. Returns 0 otherwise. Note that this is
   defined only for Linux.
*/

/* Define these as weak symbols, so if support for capabilities is not present,
   programs can still link. On Ubuntu, support for capabilities can be
   installed via "sudo apt-get --assume-yes install libcap-dev".
   In addition, the user must link with "-lcap", or else these
   symbols will be 0, and __gnat_has_cap_sys_nice will return 0.
*/

static cap_t cap_get_proc_weak(void)
  __attribute__ ((weakref ("cap_get_proc")));
static int cap_get_flag_weak(cap_t, cap_value_t, cap_flag_t, cap_flag_value_t *)
  __attribute__ ((weakref ("cap_get_flag")));
static int cap_free_weak(void *)
  __attribute__ ((weakref ("cap_free")));

int
__gnat_has_cap_sys_nice () {
  /* If the address of cap_get_proc_weak is 0, this means support for
     capabilities is not present, so we return 0. */
  if (&cap_get_proc_weak == 0)
    return 0;

  cap_t caps = cap_get_proc_weak();
  if (caps == NULL)
    return 0;

  cap_flag_value_t value;

  if (cap_get_flag_weak(caps, CAP_SYS_NICE, CAP_EFFECTIVE, &value) == -1)
    return 0;

  if (cap_free_weak(caps) == -1)
    return 0;

  if (value == CAP_SET)
    return 1;

  return 0;
}
#endif

#ifdef __ANDROID__

/* Provide extern symbols for sig* as needed by the tasking run-time, instead
   of static inline functions.  */

#include <signal.h>

int
_sigismember (sigset_t *set, int signum)
{
  return sigismember (set, signum);
}

int
_sigaddset (sigset_t *set, int signum)
{
  return sigaddset (set, signum);
}

int
_sigdelset (sigset_t *set, int signum)
{
  return sigdelset (set, signum);
}

int
_sigemptyset (sigset_t *set)
{
  return sigemptyset (set);
}

int
_sigfillset (sigset_t *set)
{
  return sigfillset (set);
}

#include <unistd.h>
int
_getpagesize (void)
{
  return getpagesize ();
}
#endif

int
__gnat_name_case_equivalence ()
{
  /*  the values here must be synchronized with Ada.Directories.Name_Case_Kind:

      Unknown          = 0
      Case_Sensitive   = 1
      Case_Insensitive = 2
      Case_Preserving  = 3  */

#if defined (__APPLE__) || defined (WIN32)
  return 3;
#else
  return 1;
#endif
}
