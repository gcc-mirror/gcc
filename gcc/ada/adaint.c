/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2015, Free Software Foundation, Inc.         *
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

/* This file contains those routines named by Import pragmas in
   packages in the GNAT hierarchy (especially GNAT.OS_Lib) and in
   package Osint.  Many of the subprograms in OS_Lib import standard
   library calls directly. This file contains all other routines.  */

/* Ensure access to errno is thread safe.  */
#define _REENTRANT
#define _THREAD_SAFE

/* Use 64 bit Large File API */
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif
#define _FILE_OFFSET_BITS 64

#ifdef __vxworks

/* No need to redefine exit here.  */
#undef exit

/* We want to use the POSIX variants of include files.  */
#define POSIX
#include "vxWorks.h"

#if defined (__mips_vxworks)
#include "cacheLib.h"
#endif /* __mips_vxworks */

/* If SMP, access vxCpuConfiguredGet */
#ifdef _WRS_CONFIG_SMP
#include <vxCpuLib.h>
#endif /* _WRS_CONFIG_SMP */

/* We need to know the VxWorks version because some file operations
   (such as chmod) are only available on VxWorks 6.  */
#include "version.h"

#endif /* VxWorks */

#if defined (__APPLE__)
#include <unistd.h>
#endif

#if defined (__hpux__)
#include <sys/param.h>
#include <sys/pstat.h>
#endif

#ifdef __PikeOS__
#define __BSD_VISIBLE 1
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#if defined (__vxworks) || defined (__ANDROID__)
/* S_IREAD and S_IWRITE are not defined in VxWorks or Android */
#ifndef S_IREAD
#define S_IREAD  (S_IRUSR | S_IRGRP | S_IROTH)
#endif

#ifndef S_IWRITE
#define S_IWRITE (S_IWUSR)
#endif
#endif

/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#define xrealloc(V,S) realloc (V,S)
#else
#include "config.h"
#include "system.h"
#include "version.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined (__MINGW32__) || defined (__CYGWIN__)

#include "mingw32.h"

/* Current code page and CCS encoding to use, set in initialize.c.  */
UINT CurrentCodePage;
UINT CurrentCCSEncoding;

#include <sys/utime.h>

/* For isalpha-like tests in the compiler, we're expected to resort to
   safe-ctype.h/ISALPHA.  This isn't available for the runtime library
   build, so we fallback on ctype.h/isalpha there.  */

#ifdef IN_RTS
#include <ctype.h>
#define ISALPHA isalpha
#endif

#elif defined (__Lynx__)

/* Lynx utime.h only defines the entities of interest to us if
   defined (VMOS_DEV), so ... */
#define VMOS_DEV
#include <utime.h>
#undef VMOS_DEV

#else
#include <utime.h>
#endif

/* wait.h processing */
#ifdef __MINGW32__
# if OLD_MINGW
#  include <sys/wait.h>
# endif
#elif defined (__vxworks) && defined (__RTP__)
# include <wait.h>
#elif defined (__Lynx__)
/* ??? We really need wait.h and it includes resource.h on Lynx.  GCC
   has a resource.h header as well, included instead of the lynx
   version in our setup, causing lots of errors.  We don't really need
   the lynx contents of this file, so just workaround the issue by
   preventing the inclusion of the GCC header from doing anything.  */
# define GCC_RESOURCE_H
# include <sys/wait.h>
#elif defined (__PikeOS__)
/* No wait() or waitpid() calls available.  */
#else
/* Default case.  */
#include <sys/wait.h>
#endif

#if defined (_WIN32)

#include <process.h>
#include <signal.h>
#include <dir.h>
#include <windows.h>
#include <accctrl.h>
#include <aclapi.h>
#include <tlhelp32.h>
#undef DIR_SEPARATOR
#define DIR_SEPARATOR '\\'

#else
#include <utime.h>
#endif

#include "adaint.h"

/* Define symbols O_BINARY and O_TEXT as harmless zeroes if they are not
   defined in the current system. On DOS-like systems these flags control
   whether the file is opened/created in text-translation mode (CR/LF in
   external file mapped to LF in internal file), but in Unix-like systems,
   no text translation is required, so these flags have no effect.  */

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_TEXT
#define O_TEXT 0
#endif

#ifndef HOST_EXECUTABLE_SUFFIX
#define HOST_EXECUTABLE_SUFFIX ""
#endif

#ifndef HOST_OBJECT_SUFFIX
#define HOST_OBJECT_SUFFIX ".o"
#endif

#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Check for cross-compilation.  */
#if defined (CROSS_COMPILE) || defined (CROSS_DIRECTORY_STRUCTURE)
#define IS_CROSS 1
int __gnat_is_cross_compiler = 1;
#else
#undef IS_CROSS
int __gnat_is_cross_compiler = 0;
#endif

char __gnat_dir_separator = DIR_SEPARATOR;

char __gnat_path_separator = PATH_SEPARATOR;

/* The GNAT_LIBRARY_TEMPLATE contains a list of expressions that define
   the base filenames that libraries specified with -lsomelib options
   may have. This is used by GNATMAKE to check whether an executable
   is up-to-date or not. The syntax is

     library_template ::= { pattern ; } pattern NUL
     pattern          ::= [ prefix ] * [ postfix ]

   These should only specify names of static libraries as it makes
   no sense to determine at link time if dynamic-link libraries are
   up to date or not. Any libraries that are not found are supposed
   to be up-to-date:

     * if they are needed but not present, the link
       will fail,

     * otherwise they are libraries in the system paths and so
       they are considered part of the system and not checked
       for that reason.

   ??? This should be part of a GNAT host-specific compiler
       file instead of being included in all user applications
       as well. This is only a temporary work-around for 3.11b.  */

#ifndef GNAT_LIBRARY_TEMPLATE
#define GNAT_LIBRARY_TEMPLATE "lib*.a"
#endif

const char *__gnat_library_template = GNAT_LIBRARY_TEMPLATE;

#if defined (__vxworks)
#define GNAT_MAX_PATH_LEN PATH_MAX

#else

#if defined (__MINGW32__)
#include "mingw32.h"

#if OLD_MINGW
#include <sys/param.h>
#endif

#else
#include <sys/param.h>
#endif

#ifdef MAXPATHLEN
#define GNAT_MAX_PATH_LEN MAXPATHLEN
#else
#define GNAT_MAX_PATH_LEN 256
#endif

#endif

/* Used for runtime check that Ada constant File_Attributes_Size is no
   less than the actual size of struct file_attributes (see Osint
   initialization). */
int __gnat_size_of_file_attributes = sizeof (struct file_attributes);

void __gnat_stat_to_attr (int fd, char* name, struct file_attributes* attr);

/* The __gnat_max_path_len variable is used to export the maximum
   length of a path name to Ada code. max_path_len is also provided
   for compatibility with older GNAT versions, please do not use
   it. */

int __gnat_max_path_len = GNAT_MAX_PATH_LEN;
int max_path_len = GNAT_MAX_PATH_LEN;

/* Control whether we can use ACL on Windows.  */

int __gnat_use_acl = 1;

/* The following macro HAVE_READDIR_R should be defined if the
   system provides the routine readdir_r.
   ... but we never define it anywhere???  */
#undef HAVE_READDIR_R

#define MAYBE_TO_PTR32(argv) argv

static const char ATTR_UNSET = 127;

/* Reset the file attributes as if no system call had been performed */

void
__gnat_reset_attributes (struct file_attributes* attr)
{
  attr->exists     = ATTR_UNSET;
  attr->error      = EINVAL;

  attr->writable   = ATTR_UNSET;
  attr->readable   = ATTR_UNSET;
  attr->executable = ATTR_UNSET;

  attr->regular    = ATTR_UNSET;
  attr->symbolic_link = ATTR_UNSET;
  attr->directory = ATTR_UNSET;

  attr->timestamp = (OS_Time)-2;
  attr->file_length = -1;
}

int
__gnat_error_attributes (struct file_attributes *attr) {
  return attr->error;
}

OS_Time
__gnat_current_time (void)
{
  time_t res = time (NULL);
  return (OS_Time) res;
}

/* Return the current local time as a string in the ISO 8601 format of
   "YYYY-MM-DD HH:MM:SS.SS". The returned string is 22 + 1 (NULL) characters
   long. */

void
__gnat_current_time_string (char *result)
{
  const char *format = "%Y-%m-%d %H:%M:%S";
  /* Format string necessary to describe the ISO 8601 format */

  const time_t t_val = time (NULL);

  strftime (result, 22, format, localtime (&t_val));
  /* Convert the local time into a string following the ISO format, copying
     at most 22 characters into the result string. */

  result [19] = '.';
  result [20] = '0';
  result [21] = '0';
  /* The sub-seconds are manually set to zero since type time_t lacks the
     precision necessary for nanoseconds. */
}

void
__gnat_to_gm_time (OS_Time *p_time, int *p_year, int *p_month, int *p_day,
		   int *p_hours, int *p_mins, int *p_secs)
{
  struct tm *res;
  time_t time = (time_t) *p_time;

#ifdef _WIN32
  /* On Windows systems, the time is sometimes rounded up to the nearest
     even second, so if the number of seconds is odd, increment it.  */
  if (time & 1)
    time++;
#endif

  res = gmtime (&time);
  if (res)
    {
      *p_year = res->tm_year;
      *p_month = res->tm_mon;
      *p_day = res->tm_mday;
      *p_hours = res->tm_hour;
      *p_mins = res->tm_min;
      *p_secs = res->tm_sec;
    }
  else
    *p_year = *p_month = *p_day = *p_hours = *p_mins = *p_secs = 0;
}

void
__gnat_to_os_time (OS_Time *p_time, int year, int month, int day,
		   int hours, int mins, int secs)
{
  struct tm v;

  v.tm_year  = year;
  v.tm_mon   = month;
  v.tm_mday  = day;
  v.tm_hour  = hours;
  v.tm_min   = mins;
  v.tm_sec   = secs;
  v.tm_isdst = -1;

  /* returns -1 of failing, this is s-os_lib Invalid_Time */

  *p_time = (OS_Time) mktime (&v);
}

/* Place the contents of the symbolic link named PATH in the buffer BUF,
   which has size BUFSIZ.  If PATH is a symbolic link, then return the number
   of characters of its content in BUF.  Otherwise, return -1.
   For systems not supporting symbolic links, always return -1.  */

int
__gnat_readlink (char *path ATTRIBUTE_UNUSED,
		 char *buf ATTRIBUTE_UNUSED,
		 size_t bufsiz ATTRIBUTE_UNUSED)
{
#if defined (_WIN32) \
  || defined(__vxworks) || defined (__PikeOS__)
  return -1;
#else
  return readlink (path, buf, bufsiz);
#endif
}

/* Creates a symbolic link named NEWPATH which contains the string OLDPATH.
   If NEWPATH exists it will NOT be overwritten.
   For systems not supporting symbolic links, always return -1.  */

int
__gnat_symlink (char *oldpath ATTRIBUTE_UNUSED,
		char *newpath ATTRIBUTE_UNUSED)
{
#if defined (_WIN32) \
  || defined(__vxworks) || defined (__PikeOS__)
  return -1;
#else
  return symlink (oldpath, newpath);
#endif
}

/* Try to lock a file, return 1 if success.  */

#if defined (__vxworks) \
  || defined (_WIN32) || defined (__PikeOS__)

/* Version that does not use link. */

int
__gnat_try_lock (char *dir, char *file)
{
  int fd;
#ifdef __MINGW32__
  TCHAR wfull_path[GNAT_MAX_PATH_LEN];
  TCHAR wfile[GNAT_MAX_PATH_LEN];
  TCHAR wdir[GNAT_MAX_PATH_LEN];

  S2WSC (wdir, dir, GNAT_MAX_PATH_LEN);
  S2WSC (wfile, file, GNAT_MAX_PATH_LEN);

  /* ??? the code below crash on MingW64 for obscure reasons, a ticket
     has been opened here:

     https://sourceforge.net/p/mingw-w64/bugs/414/

     As a workaround an equivalent set of code has been put in place below.

  _stprintf (wfull_path, _T("%s%c%s"), wdir, _T(DIR_SEPARATOR), wfile);
  */

  _tcscpy (wfull_path, wdir);
  _tcscat (wfull_path, L"\\");
  _tcscat (wfull_path, wfile);

  fd = _topen (wfull_path, O_CREAT | O_EXCL, 0600);
#else
  char full_path[256];

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  fd = open (full_path, O_CREAT | O_EXCL, 0600);
#endif

  if (fd < 0)
    return 0;

  close (fd);
  return 1;
}

#else

/* Version using link(), more secure over NFS.  */
/* See TN 6913-016 for discussion ??? */

int
__gnat_try_lock (char *dir, char *file)
{
  char full_path[256];
  char temp_file[256];
  GNAT_STRUCT_STAT stat_result;
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  sprintf (temp_file, "%s%cTMP-%ld-%ld",
           dir, DIR_SEPARATOR, (long)getpid(), (long)getppid ());

  /* Create the temporary file and write the process number.  */
  fd = open (temp_file, O_CREAT | O_WRONLY, 0600);
  if (fd < 0)
    return 0;

  close (fd);

  /* Link it with the new file.  */
  link (temp_file, full_path);

  /* Count the references on the old one. If we have a count of two, then
     the link did succeed. Remove the temporary file before returning.  */
  __gnat_stat (temp_file, &stat_result);
  unlink (temp_file);
  return stat_result.st_nlink == 2;
}
#endif

/* Return the maximum file name length.  */

int
__gnat_get_maximum_file_name_length (void)
{
  return -1;
}

/* Return nonzero if file names are case sensitive.  */

static int file_names_case_sensitive_cache = -1;

int
__gnat_get_file_names_case_sensitive (void)
{
  if (file_names_case_sensitive_cache == -1)
    {
      const char *sensitive = getenv ("GNAT_FILE_NAME_CASE_SENSITIVE");

      if (sensitive != NULL
          && (sensitive[0] == '0' || sensitive[0] == '1')
          && sensitive[1] == '\0')
        file_names_case_sensitive_cache = sensitive[0] - '0';
      else
	{
	  /* By default, we suppose filesystems aren't case sensitive on
	     Windows and Darwin (but they are on arm-darwin).  */
#if defined (WINNT) \
  || (defined (__APPLE__) && !(defined (__arm__) || defined (__arm64__)))
	  file_names_case_sensitive_cache = 0;
#else
	  file_names_case_sensitive_cache = 1;
#endif
	}
    }
  return file_names_case_sensitive_cache;
}

/* Return nonzero if environment variables are case sensitive.  */

int
__gnat_get_env_vars_case_sensitive (void)
{
#if defined (WINNT)
 return 0;
#else
 return 1;
#endif
}

char
__gnat_get_default_identifier_character_set (void)
{
  return '1';
}

/* Return the current working directory.  */

void
__gnat_get_current_dir (char *dir, int *length)
{
#if defined (__MINGW32__)
  TCHAR wdir[GNAT_MAX_PATH_LEN];

  _tgetcwd (wdir, *length);

  WS2SC (dir, wdir, GNAT_MAX_PATH_LEN);

#else
   getcwd (dir, *length);
#endif

   *length = strlen (dir);

   if (dir [*length - 1] != DIR_SEPARATOR)
     {
       dir [*length] = DIR_SEPARATOR;
       ++(*length);
     }
   dir[*length] = '\0';
}

/* Return the suffix for object files.  */

void
__gnat_get_object_suffix_ptr (int *len, const char **value)
{
  *value = HOST_OBJECT_SUFFIX;

  if (*value == 0)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for executable files.  */

void
__gnat_get_executable_suffix_ptr (int *len, const char **value)
{
  *value = HOST_EXECUTABLE_SUFFIX;
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for debuggable files. Usually this is the same as the
   executable extension.  */

void
__gnat_get_debuggable_suffix_ptr (int *len, const char **value)
{
  *value = HOST_EXECUTABLE_SUFFIX;

  if (*value == 0)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Returns the OS filename and corresponding encoding.  */

void
__gnat_os_filename (char *filename ATTRIBUTE_UNUSED,
		    char *w_filename ATTRIBUTE_UNUSED,
		    char *os_name, int *o_length,
		    char *encoding ATTRIBUTE_UNUSED, int *e_length)
{
#if defined (_WIN32) && ! defined (__vxworks) && ! defined (IS_CROSS)
  WS2SC (os_name, (TCHAR *)w_filename, (DWORD)*o_length);
  *o_length = strlen (os_name);
  strcpy (encoding, "encoding=utf8");
  *e_length = strlen (encoding);
#else
  strcpy (os_name, filename);
  *o_length = strlen (filename);
  *e_length = 0;
#endif
}

/* Delete a file.  */

int
__gnat_unlink (char *path)
{
#if defined (__MINGW32__) && ! defined (__vxworks) && ! defined (IS_CROSS)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    return _tunlink (wpath);
  }
#else
  return unlink (path);
#endif
}

/* Rename a file.  */

int
__gnat_rename (char *from, char *to)
{
#if defined (__MINGW32__) && ! defined (__vxworks) && ! defined (IS_CROSS)
  {
    TCHAR wfrom[GNAT_MAX_PATH_LEN], wto[GNAT_MAX_PATH_LEN];

    S2WSC (wfrom, from, GNAT_MAX_PATH_LEN);
    S2WSC (wto, to, GNAT_MAX_PATH_LEN);
    return _trename (wfrom, wto);
  }
#else
  return rename (from, to);
#endif
}

/* Changing directory.  */

int
__gnat_chdir (char *path)
{
#if defined (__MINGW32__) && ! defined (__vxworks) && ! defined (IS_CROSS)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    return _tchdir (wpath);
  }
#else
  return chdir (path);
#endif
}

/* Removing a directory.  */

int
__gnat_rmdir (char *path)
{
#if defined (__MINGW32__) && ! defined (__vxworks) && ! defined (IS_CROSS)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    return _trmdir (wpath);
  }
#elif defined (VTHREADS)
  /* rmdir not available */
  return -1;
#else
  return rmdir (path);
#endif
}

#if defined (_WIN32) || defined (__linux__) || defined (__sun__) \
  || defined (__FreeBSD__) || defined(__DragonFly__)
#define HAS_TARGET_WCHAR_T
#endif

#ifdef HAS_TARGET_WCHAR_T
#include <wchar.h>
#endif

int
__gnat_fputwc(int c, FILE *stream)
{
#ifdef HAS_TARGET_WCHAR_T
  return fputwc ((wchar_t)c, stream);
#else
  return fputc (c, stream);
#endif
}

FILE *
__gnat_fopen (char *path, char *mode, int encoding ATTRIBUTE_UNUSED)
{
#if defined (_WIN32) && ! defined (__vxworks) && ! defined (IS_CROSS)
  TCHAR wpath[GNAT_MAX_PATH_LEN];
  TCHAR wmode[10];

  S2WS (wmode, mode, 10);

  if (encoding == Encoding_Unspecified)
    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
  else if (encoding == Encoding_UTF8)
    S2WSU (wpath, path, GNAT_MAX_PATH_LEN);
  else
    S2WS (wpath, path, GNAT_MAX_PATH_LEN);

  return _tfopen (wpath, wmode);

#else
  return GNAT_FOPEN (path, mode);
#endif
}

FILE *
__gnat_freopen (char *path,
		char *mode,
		FILE *stream,
		int encoding ATTRIBUTE_UNUSED)
{
#if defined (_WIN32) && ! defined (__vxworks) && ! defined (IS_CROSS)
  TCHAR wpath[GNAT_MAX_PATH_LEN];
  TCHAR wmode[10];

  S2WS (wmode, mode, 10);

  if (encoding == Encoding_Unspecified)
    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
  else if (encoding == Encoding_UTF8)
    S2WSU (wpath, path, GNAT_MAX_PATH_LEN);
  else
    S2WS (wpath, path, GNAT_MAX_PATH_LEN);

  return _tfreopen (wpath, wmode, stream);
#else
  return freopen (path, mode, stream);
#endif
}

int
__gnat_open_read (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (__vxworks)
  fd = open (path, O_RDONLY | o_fmode, 0444);
#elif defined (__MINGW32__)
 {
   TCHAR wpath[GNAT_MAX_PATH_LEN];

   S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
   fd = _topen (wpath, O_RDONLY | o_fmode, 0444);
 }
#else
  fd = GNAT_OPEN (path, O_RDONLY | o_fmode);
#endif

  return fd < 0 ? -1 : fd;
}

#if defined (__MINGW32__)
#define PERM (S_IREAD | S_IWRITE)
#else
#define PERM (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#endif

int
__gnat_open_rw (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_RDWR | o_fmode, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_RDWR | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_open_create (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_create_output_file (char *path)
{
  int fd;
#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_create_output_file_new (char *path)
{
  int fd;
#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT | O_EXCL, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT | O_EXCL, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_open_append (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/*  Open a new file.  Return error (-1) if the file already exists.  */

int
__gnat_open_new (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
  }
#else
  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/* Open a new temp file.  Return error (-1) if the file already exists.  */

int
__gnat_open_new_temp (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  strcpy (path, "GNAT-XXXXXX");

#if (defined (__FreeBSD__) || defined (__NetBSD__) || defined (__OpenBSD__) \
  || defined (__linux__) || defined (__GLIBC__) || defined (__ANDROID__) \
  || defined (__DragonFly__)) && !defined (__vxworks)
  return mkstemp (path);
#elif defined (__Lynx__)
  mktemp (path);
#else
  if (mktemp (path) == NULL)
    return -1;
#endif

  if (fmode)
    o_fmode = O_TEXT;

  fd = GNAT_OPEN (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
  return fd < 0 ? -1 : fd;
}

int
__gnat_open (char *path, int fmode)
{
  int fd;

#if defined (__MINGW32__)
  {
    TCHAR wpath[GNAT_MAX_PATH_LEN];

    S2WSC (wpath, path, GNAT_MAX_PATH_LEN);
    fd = _topen (wpath, fmode, PERM);
  }
#else
  fd = GNAT_OPEN (path, fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/****************************************************************
 ** Perform a call to GNAT_STAT or GNAT_FSTAT, and extract as much information
 ** as possible from it, storing the result in a cache for later reuse
 ****************************************************************/

void
__gnat_stat_to_attr (int fd, char* name, struct file_attributes* attr)
{
  GNAT_STRUCT_STAT statbuf;
  int ret, error;

  if (fd != -1) {
    /* GNAT_FSTAT returns -1 and sets errno for failure */
    ret = GNAT_FSTAT (fd, &statbuf);
    error = ret ? errno : 0;

  } else {
    /* __gnat_stat returns errno value directly */
    error = __gnat_stat (name, &statbuf);
    ret = error ? -1 : 0;
  }

  /*
   * A missing file is reported as an attr structure with error == 0 and
   * exists == 0.
   */

  if (error == 0 || error == ENOENT)
    attr->error = 0;
  else
    attr->error = error;

  attr->regular   = (!ret && S_ISREG (statbuf.st_mode));
  attr->directory = (!ret && S_ISDIR (statbuf.st_mode));

  if (!attr->regular)
    attr->file_length = 0;
  else
    /* st_size may be 32 bits, or 64 bits which is converted to long. We
       don't return a useful value for files larger than 2 gigabytes in
       either case. */
    attr->file_length = statbuf.st_size;  /* all systems */

  attr->exists = !ret;

#if !defined (_WIN32)
  /* on Windows requires extra system call, see __gnat_is_readable_file_attr */
  attr->readable   = (!ret && (statbuf.st_mode & S_IRUSR));
  attr->writable   = (!ret && (statbuf.st_mode & S_IWUSR));
  attr->executable = (!ret && (statbuf.st_mode & S_IXUSR));
#endif

  if (ret != 0) {
     attr->timestamp = (OS_Time)-1;
  } else {
     attr->timestamp = (OS_Time)statbuf.st_mtime;
  }
}

/****************************************************************
 ** Return the number of bytes in the specified file
 ****************************************************************/

__int64
__gnat_file_length_attr (int fd, char* name, struct file_attributes* attr)
{
  if (attr->file_length == -1) {
    __gnat_stat_to_attr (fd, name, attr);
  }

  return attr->file_length;
}

__int64
__gnat_file_length (int fd)
{
  struct file_attributes attr;
  __gnat_reset_attributes (&attr);
  return __gnat_file_length_attr (fd, NULL, &attr);
}

long
__gnat_file_length_long (int fd)
{
  struct file_attributes attr;
  __gnat_reset_attributes (&attr);
  return (long)__gnat_file_length_attr (fd, NULL, &attr);
}

__int64
__gnat_named_file_length (char *name)
{
  struct file_attributes attr;
  __gnat_reset_attributes (&attr);
  return __gnat_file_length_attr (-1, name, &attr);
}

/* Create a temporary filename and put it in string pointed to by
   TMP_FILENAME.  */

void
__gnat_tmp_name (char *tmp_filename)
{
#if defined (__MINGW32__)
  {
    char *pname;
    char prefix[25];

    /* tempnam tries to create a temporary file in directory pointed to by
       TMP environment variable, in c:\temp if TMP is not set, and in
       directory specified by P_tmpdir in stdio.h if c:\temp does not
       exist. The filename will be created with the prefix "gnat-".  */

    sprintf (prefix, "gnat-%d-", (int)getpid());
    pname = (char *) _tempnam ("c:\\temp", prefix);

    /* if pname is NULL, the file was not created properly, the disk is full
       or there is no more free temporary files */

    if (pname == NULL)
      *tmp_filename = '\0';

    /* If pname start with a back slash and not path information it means that
       the filename is valid for the current working directory.  */

    else if (pname[0] == '\\')
      {
	strcpy (tmp_filename, ".\\");
	strcat (tmp_filename, pname+1);
      }
    else
      strcpy (tmp_filename, pname);

    free (pname);
  }

#elif defined (__linux__) || defined (__FreeBSD__) || defined (__NetBSD__) \
  || defined (__OpenBSD__) || defined (__GLIBC__) || defined (__ANDROID__) \
  || defined (__DragonFly__)
#define MAX_SAFE_PATH 1000
  char *tmpdir = getenv ("TMPDIR");

  /* If tmpdir is longer than MAX_SAFE_PATH, revert to default value to avoid
     a buffer overflow.  */
  if (tmpdir == NULL || strlen (tmpdir) > MAX_SAFE_PATH)
#ifdef __ANDROID__
    strcpy (tmp_filename, "/cache/gnat-XXXXXX");
#else
    strcpy (tmp_filename, "/tmp/gnat-XXXXXX");
#endif
  else
    sprintf (tmp_filename, "%s/gnat-XXXXXX", tmpdir);

  close (mkstemp(tmp_filename));
#elif defined (__vxworks) && !defined (VTHREADS)
  int index;
  char *pos;
  char *savepos;
  static ushort_t seed = 0; /* used to generate unique name */

  /* Generate a unique name.  */
  strcpy (tmp_filename, "tmp");

  index = 5;
  savepos = pos = tmp_filename + strlen (tmp_filename) + index;
  *pos = '\0';

  while (1)
    {
      FILE *f;
      ushort_t t;

      /* Fill up the name buffer from the last position.  */
      seed++;
      for (t = seed; 0 <= --index; t >>= 3)
        *--pos = '0' + (t & 07);

      /* Check to see if its unique, if not bump the seed and try again.  */
      f = fopen (tmp_filename, "r");
      if (f == NULL)
        break;
      fclose (f);
      pos = savepos;
      index = 5;
    }
#else
  tmpnam (tmp_filename);
#endif
}

/*  Open directory and returns a DIR pointer.  */

DIR* __gnat_opendir (char *name)
{
#if defined (__MINGW32__)
  TCHAR wname[GNAT_MAX_PATH_LEN];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN);
  return (DIR*)_topendir (wname);

#else
  return opendir (name);
#endif
}

/* Read the next entry in a directory.  The returned string points somewhere
   in the buffer.  */

#if defined (__sun__)
/* For Solaris, be sure to use the 64-bit version, otherwise NFS reads may
   fail with EOVERFLOW if the server uses 64-bit cookies.  */
#define dirent dirent64
#define readdir readdir64
#endif

char *
__gnat_readdir (DIR *dirp, char *buffer, int *len)
{
#if defined (__MINGW32__)
  struct _tdirent *dirent = _treaddir ((_TDIR*)dirp);

  if (dirent != NULL)
    {
      WS2SC (buffer, dirent->d_name, GNAT_MAX_PATH_LEN);
      *len = strlen (buffer);

      return buffer;
    }
  else
    return NULL;

#elif defined (HAVE_READDIR_R)
  /* If possible, try to use the thread-safe version.  */
  if (readdir_r (dirp, buffer) != NULL)
    {
      *len = strlen (((struct dirent*) buffer)->d_name);
      return ((struct dirent*) buffer)->d_name;
    }
  else
    return NULL;

#else
  struct dirent *dirent = (struct dirent *) readdir (dirp);

  if (dirent != NULL)
    {
      strcpy (buffer, dirent->d_name);
      *len = strlen (buffer);
      return buffer;
    }
  else
    return NULL;

#endif
}

/* Close a directory entry.  */

int __gnat_closedir (DIR *dirp)
{
#if defined (__MINGW32__)
  return _tclosedir ((_TDIR*)dirp);

#else
  return closedir (dirp);
#endif
}

/* Returns 1 if readdir is thread safe, 0 otherwise.  */

int
__gnat_readdir_is_thread_safe (void)
{
#ifdef HAVE_READDIR_R
  return 1;
#else
  return 0;
#endif
}

#if defined (_WIN32)
/* Number of seconds between <Jan 1st 1601> and <Jan 1st 1970>.  */
static const unsigned long long w32_epoch_offset = 11644473600ULL;

/* Returns the file modification timestamp using Win32 routines which are
   immune against daylight saving time change. It is in fact not possible to
   use fstat for this purpose as the DST modify the st_mtime field of the
   stat structure.  */

static time_t
win32_filetime (HANDLE h)
{
  union
  {
    FILETIME ft_time;
    unsigned long long ull_time;
  } t_write;

  /* GetFileTime returns FILETIME data which are the number of 100 nanosecs
     since <Jan 1st 1601>. This function must return the number of seconds
     since <Jan 1st 1970>.  */

  if (GetFileTime (h, NULL, NULL, &t_write.ft_time))
    return (time_t) (t_write.ull_time / 10000000ULL - w32_epoch_offset);
  return (time_t) 0;
}

/* As above but starting from a FILETIME.  */
static void
f2t (const FILETIME *ft, __time64_t *t)
{
  union
  {
    FILETIME ft_time;
    unsigned long long ull_time;
  } t_write;

  t_write.ft_time = *ft;
  *t = (__time64_t) (t_write.ull_time / 10000000ULL - w32_epoch_offset);
}
#endif

/* Return a GNAT time stamp given a file name.  */

OS_Time
__gnat_file_time_name_attr (char* name, struct file_attributes* attr)
{
   if (attr->timestamp == (OS_Time)-2) {
#if defined (_WIN32)
      BOOL res;
      WIN32_FILE_ATTRIBUTE_DATA fad;
      __time64_t ret = -1;
      TCHAR wname[GNAT_MAX_PATH_LEN];
      S2WSC (wname, name, GNAT_MAX_PATH_LEN);

      if ((res = GetFileAttributesEx (wname, GetFileExInfoStandard, &fad)))
	f2t (&fad.ftLastWriteTime, &ret);
      attr->timestamp = (OS_Time) ret;
#else
      __gnat_stat_to_attr (-1, name, attr);
#endif
  }
  return attr->timestamp;
}

OS_Time
__gnat_file_time_name (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_file_time_name_attr (name, &attr);
}

/* Return a GNAT time stamp given a file descriptor.  */

OS_Time
__gnat_file_time_fd_attr (int fd, struct file_attributes* attr)
{
   if (attr->timestamp == (OS_Time)-2) {
#if defined (_WIN32)
     HANDLE h = (HANDLE) _get_osfhandle (fd);
     time_t ret = win32_filetime (h);
     attr->timestamp = (OS_Time) ret;

#else
     __gnat_stat_to_attr (fd, NULL, attr);
#endif
   }

   return attr->timestamp;
}

OS_Time
__gnat_file_time_fd (int fd)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_file_time_fd_attr (fd, &attr);
}

/* Set the file time stamp.  */

void
__gnat_set_file_time_name (char *name, time_t time_stamp)
{
#if defined (__vxworks)

/* Code to implement __gnat_set_file_time_name for these systems.  */

#elif defined (_WIN32)
  union
  {
    FILETIME ft_time;
    unsigned long long ull_time;
  } t_write;
  TCHAR wname[GNAT_MAX_PATH_LEN];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN);

  HANDLE h  = CreateFile
    (wname, GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
     OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
     NULL);
  if (h == INVALID_HANDLE_VALUE)
    return;
  /* Add number of seconds between <Jan 1st 1601> and <Jan 1st 1970> */
  t_write.ull_time = ((unsigned long long)time_stamp + w32_epoch_offset);
  /*  Convert to 100 nanosecond units  */
  t_write.ull_time *= 10000000ULL;

  SetFileTime(h, NULL, NULL, &t_write.ft_time);
  CloseHandle (h);
  return;

#else
  struct utimbuf utimbuf;
  time_t t;

  /* Set modification time to requested time.  */
  utimbuf.modtime = time_stamp;

  /* Set access time to now in local time.  */
  t = time ((time_t) 0);
  utimbuf.actime = mktime (localtime (&t));

  utime (name, &utimbuf);
#endif
}

/* Get the list of installed standard libraries from the
   HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries
   key.  */

char *
__gnat_get_libraries_from_registry (void)
{
  char *result = (char *) xmalloc (1);

  result[0] = '\0';

#if defined (_WIN32) && ! defined (__vxworks) && ! defined (IS_CROSS)

  HKEY reg_key;
  DWORD name_size, value_size;
  char name[256];
  char value[256];
  DWORD type;
  DWORD index;
  LONG res;

  /* First open the key.  */
  res = RegOpenKeyExA (HKEY_LOCAL_MACHINE, "SOFTWARE", 0, KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "Ada Core Technologies", 0,
                         KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "GNAT", 0, KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "Standard Libraries", 0, KEY_READ, &reg_key);

  /* If the key exists, read out all the values in it and concatenate them
     into a path.  */
  for (index = 0; res == ERROR_SUCCESS; index++)
    {
      value_size = name_size = 256;
      res = RegEnumValueA (reg_key, index, name, &name_size, 0,
                           &type, (LPBYTE)value, &value_size);

      if (res == ERROR_SUCCESS && type == REG_SZ)
        {
          char *old_result = result;

          result = (char *) xmalloc (strlen (old_result) + value_size + 2);
          strcpy (result, old_result);
          strcat (result, value);
          strcat (result, ";");
          free (old_result);
        }
    }

  /* Remove the trailing ";".  */
  if (result[0] != 0)
    result[strlen (result) - 1] = 0;

#endif
  return result;
}

/* Query information for the given file NAME and return it in STATBUF.
 * Returns 0 for success, or errno value for failure.
 */
int
__gnat_stat (char *name, GNAT_STRUCT_STAT *statbuf)
{
#ifdef __MINGW32__
  WIN32_FILE_ATTRIBUTE_DATA fad;
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];
  int name_len;
  BOOL res;
  DWORD error;

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);
  name_len = _tcslen (wname);

  if (name_len > GNAT_MAX_PATH_LEN)
    return EINVAL;

  ZeroMemory (statbuf, sizeof(GNAT_STRUCT_STAT));

  res = GetFileAttributesEx (wname, GetFileExInfoStandard, &fad);

  if (res == FALSE) {
    error = GetLastError();

    /* Check file existence using GetFileAttributes() which does not fail on
       special Windows files like con:, aux:, nul: etc...  */

    if (GetFileAttributes(wname) != INVALID_FILE_ATTRIBUTES) {
      /* Just pretend that it is a regular and readable file  */
      statbuf->st_mode = S_IFREG | S_IREAD | S_IWRITE;
      return 0;
    }

    switch (error) {
      case ERROR_ACCESS_DENIED:
      case ERROR_SHARING_VIOLATION:
      case ERROR_LOCK_VIOLATION:
      case ERROR_SHARING_BUFFER_EXCEEDED:
	return EACCES;
      case ERROR_BUFFER_OVERFLOW:
	return ENAMETOOLONG;
      case ERROR_NOT_ENOUGH_MEMORY:
	return ENOMEM;
      default:
	return ENOENT;
    }
  }

  f2t (&fad.ftCreationTime, &statbuf->st_ctime);
  f2t (&fad.ftLastWriteTime, &statbuf->st_mtime);
  f2t (&fad.ftLastAccessTime, &statbuf->st_atime);

  statbuf->st_size =
    (__int64)fad.nFileSizeLow | (__int64)fad.nFileSizeHigh << 32;

  /* We do not have the S_IEXEC attribute, but this is not used on GNAT.  */
  statbuf->st_mode = S_IREAD;

  if (fad.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    statbuf->st_mode |= S_IFDIR;
  else
    statbuf->st_mode |= S_IFREG;

  if (!(fad.dwFileAttributes & FILE_ATTRIBUTE_READONLY))
    statbuf->st_mode |= S_IWRITE;

  return 0;

#else
  return GNAT_STAT (name, statbuf) == 0 ? 0 : errno;
#endif
}

/*************************************************************************
 ** Check whether a file exists
 *************************************************************************/

int
__gnat_file_exists_attr (char* name, struct file_attributes* attr)
{
   if (attr->exists == ATTR_UNSET)
     __gnat_stat_to_attr (-1, name, attr);

   return attr->exists;
}

int
__gnat_file_exists (char *name)
{
   struct file_attributes attr;
   __gnat_reset_attributes (&attr);
   return __gnat_file_exists_attr (name, &attr);
}

/**********************************************************************
 ** Whether name is an absolute path
 **********************************************************************/

int
__gnat_is_absolute_path (char *name, int length)
{
#ifdef __vxworks
  /* On VxWorks systems, an absolute path can be represented (depending on
     the host platform) as either /dir/file, or device:/dir/file, or
     device:drive_letter:/dir/file. */

  int index;

  if (name[0] == '/')
    return 1;

  for (index = 0; index < length; index++)
    {
      if (name[index] == ':' &&
          ((name[index + 1] == '/') ||
           (isalpha (name[index + 1]) && index + 2 <= length &&
            name[index + 2] == '/')))
        return 1;

      else if (name[index] == '/')
        return 0;
    }
  return 0;
#else
  return (length != 0) &&
     (*name == '/' || *name == DIR_SEPARATOR
#if defined (WINNT)
      || (length > 1 && ISALPHA (name[0]) && name[1] == ':')
#endif
	  );
#endif
}

int
__gnat_is_regular_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->regular == ATTR_UNSET)
     __gnat_stat_to_attr (-1, name, attr);

   return attr->regular;
}

int
__gnat_is_regular_file (char *name)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_regular_file_attr (name, &attr);
}

int
__gnat_is_regular_file_fd (int fd)
{
  int ret;
  GNAT_STRUCT_STAT statbuf;

  ret = GNAT_FSTAT (fd, &statbuf);
  return (!ret && S_ISREG (statbuf.st_mode));
}

int
__gnat_is_directory_attr (char* name, struct file_attributes* attr)
{
   if (attr->directory == ATTR_UNSET)
     __gnat_stat_to_attr (-1, name, attr);

   return attr->directory;
}

int
__gnat_is_directory (char *name)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_directory_attr (name, &attr);
}

#if defined (_WIN32)

/* Returns the same constant as GetDriveType but takes a pathname as
   argument. */

static UINT
GetDriveTypeFromPath (TCHAR *wfullpath)
{
  TCHAR wdrv[MAX_PATH];
  TCHAR wpath[MAX_PATH];
  TCHAR wfilename[MAX_PATH];
  TCHAR wext[MAX_PATH];

  _tsplitpath (wfullpath, wdrv, wpath, wfilename, wext);

  if (_tcslen (wdrv) != 0)
    {
      /* we have a drive specified. */
      _tcscat (wdrv, _T("\\"));
      return GetDriveType (wdrv);
    }
  else
    {
      /* No drive specified. */

      /* Is this a relative path, if so get current drive type. */
      if (wpath[0] != _T('\\') ||
	  (_tcslen (wpath) > 2 && wpath[0] == _T('\\')
	   && wpath[1] != _T('\\')))
	return GetDriveType (NULL);

      UINT result = GetDriveType (wpath);

      /* Cannot guess the drive type, is this \\.\ ? */

      if (result == DRIVE_NO_ROOT_DIR &&
	 _tcslen (wpath) >= 4 && wpath[0] == _T('\\') && wpath[1] == _T('\\')
	  && wpath[2] == _T('.') && wpath[3] == _T('\\'))
	{
	  if (_tcslen (wpath) == 4)
	    _tcscat (wpath, wfilename);

	  LPTSTR p = &wpath[4];
	  LPTSTR b = _tcschr (p, _T('\\'));

	  if (b != NULL)
	    {
	      /* logical drive \\.\c\dir\file */
	      *b++ = _T(':');
	      *b++ = _T('\\');
	      *b = _T('\0');
	    }
	  else
	    _tcscat (p, _T(":\\"));

	  return GetDriveType (p);
	}

      return result;
    }
}

/*  This MingW section contains code to work with ACL.  */
static int
__gnat_check_OWNER_ACL (TCHAR *wname,
			DWORD CheckAccessDesired,
			GENERIC_MAPPING CheckGenericMapping)
{
  DWORD dwAccessDesired, dwAccessAllowed;
  PRIVILEGE_SET PrivilegeSet;
  DWORD dwPrivSetSize = sizeof (PRIVILEGE_SET);
  BOOL fAccessGranted = FALSE;
  HANDLE hToken = NULL;
  DWORD nLength = 0;
  PSECURITY_DESCRIPTOR pSD = NULL;

  GetFileSecurity
    (wname, OWNER_SECURITY_INFORMATION |
     GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
     NULL, 0, &nLength);

  if ((pSD = (SECURITY_DESCRIPTOR *) HeapAlloc
       (GetProcessHeap (), HEAP_ZERO_MEMORY, nLength)) == NULL)
    return 0;

  /* Obtain the security descriptor.  */

  if (!GetFileSecurity
      (wname, OWNER_SECURITY_INFORMATION |
       GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
       pSD, nLength, &nLength))
    goto error;

  if (!ImpersonateSelf (SecurityImpersonation))
    goto error;

  if (!OpenThreadToken
      (GetCurrentThread(), TOKEN_DUPLICATE | TOKEN_QUERY, FALSE, &hToken))
    goto error;

  /*  Undoes the effect of ImpersonateSelf. */

  RevertToSelf ();

  /*  We want to test for write permissions. */

  dwAccessDesired = CheckAccessDesired;

  MapGenericMask (&dwAccessDesired, &CheckGenericMapping);

  if (!AccessCheck
      (pSD ,                 /* security descriptor to check */
       hToken,               /* impersonation token */
       dwAccessDesired,      /* requested access rights */
       &CheckGenericMapping, /* pointer to GENERIC_MAPPING */
       &PrivilegeSet,        /* receives privileges used in check */
       &dwPrivSetSize,       /* size of PrivilegeSet buffer */
       &dwAccessAllowed,     /* receives mask of allowed access rights */
       &fAccessGranted))
    goto error;

  CloseHandle (hToken);
  HeapFree (GetProcessHeap (), 0, pSD);
  return fAccessGranted;

 error:
  if (hToken)
    CloseHandle (hToken);
  HeapFree (GetProcessHeap (), 0, pSD);
  return 0;
}

static void
__gnat_set_OWNER_ACL (TCHAR *wname,
		      ACCESS_MODE AccessMode,
		      DWORD AccessPermissions)
{
  PACL pOldDACL = NULL;
  PACL pNewDACL = NULL;
  PSECURITY_DESCRIPTOR pSD = NULL;
  EXPLICIT_ACCESS ea;
  TCHAR username [100];
  DWORD unsize = 100;

  /*  Get current user, he will act as the owner */

  if (!GetUserName (username, &unsize))
    return;

  if (GetNamedSecurityInfo
      (wname,
       SE_FILE_OBJECT,
       DACL_SECURITY_INFORMATION,
       NULL, NULL, &pOldDACL, NULL, &pSD) != ERROR_SUCCESS)
    return;

  BuildExplicitAccessWithName
    (&ea, username, AccessPermissions, (ACCESS_MODE) AccessMode, NO_INHERITANCE);

  if (AccessMode == SET_ACCESS)
    {
      /*  SET_ACCESS, we want to set an explicte set of permissions, do not
	  merge with current DACL.  */
      if (SetEntriesInAcl (1, &ea, NULL, &pNewDACL) != ERROR_SUCCESS)
	return;
    }
  else
    if (SetEntriesInAcl (1, &ea, pOldDACL, &pNewDACL) != ERROR_SUCCESS)
      return;

  if (SetNamedSecurityInfo
      (wname, SE_FILE_OBJECT,
       DACL_SECURITY_INFORMATION, NULL, NULL, pNewDACL, NULL) != ERROR_SUCCESS)
    return;

  LocalFree (pSD);
  LocalFree (pNewDACL);
}

/* Check if it is possible to use ACL for wname, the file must not be on a
   network drive. */

static int
__gnat_can_use_acl (TCHAR *wname)
{
  return __gnat_use_acl && GetDriveTypeFromPath (wname) != DRIVE_REMOTE;
}

#endif /* defined (_WIN32) */

int
__gnat_is_readable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->readable == ATTR_UNSET)
     {
#if defined (_WIN32)
       TCHAR wname [GNAT_MAX_PATH_LEN + 2];
       GENERIC_MAPPING GenericMapping;

       S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

       if (__gnat_can_use_acl (wname))
	 {
	   ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
	   GenericMapping.GenericRead = GENERIC_READ;
	   attr->readable =
	     __gnat_check_OWNER_ACL (wname, FILE_READ_DATA, GenericMapping);
	 }
       else
	 attr->readable = GetFileAttributes (wname) != INVALID_FILE_ATTRIBUTES;
#else
       __gnat_stat_to_attr (-1, name, attr);
#endif
     }

   return attr->readable;
}

int
__gnat_is_readable_file (char *name)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_readable_file_attr (name, &attr);
}

int
__gnat_is_writable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->writable == ATTR_UNSET)
     {
#if defined (_WIN32)
       TCHAR wname [GNAT_MAX_PATH_LEN + 2];
       GENERIC_MAPPING GenericMapping;

       S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

       if (__gnat_can_use_acl (wname))
	 {
	   ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
	   GenericMapping.GenericWrite = GENERIC_WRITE;

	   attr->writable = __gnat_check_OWNER_ACL
   	     (wname, FILE_WRITE_DATA | FILE_APPEND_DATA, GenericMapping)
   	     && !(GetFileAttributes (wname) & FILE_ATTRIBUTE_READONLY);
	 }
       else
	 attr->writable =
	   !(GetFileAttributes (wname) & FILE_ATTRIBUTE_READONLY);

#else
       __gnat_stat_to_attr (-1, name, attr);
#endif
     }

   return attr->writable;
}

int
__gnat_is_writable_file (char *name)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_writable_file_attr (name, &attr);
}

int
__gnat_is_executable_file_attr (char* name, struct file_attributes* attr)
{
   if (attr->executable == ATTR_UNSET)
     {
#if defined (_WIN32)
       TCHAR wname [GNAT_MAX_PATH_LEN + 2];
       GENERIC_MAPPING GenericMapping;

       S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

       if (__gnat_can_use_acl (wname))
	 {
	   ZeroMemory (&GenericMapping, sizeof (GENERIC_MAPPING));
	   GenericMapping.GenericExecute = GENERIC_EXECUTE;

	   attr->executable =
	     __gnat_check_OWNER_ACL (wname, FILE_EXECUTE, GenericMapping);
	 }
       else
	 {
	   TCHAR *l, *last = _tcsstr(wname, _T(".exe"));

	   /* look for last .exe */
	   if (last)
	     while ((l = _tcsstr(last+1, _T(".exe"))))
	       last = l;

	   attr->executable =
	     GetFileAttributes (wname) != INVALID_FILE_ATTRIBUTES
	     && (last - wname) == (int) (_tcslen (wname) - 4);
	 }
#else
       __gnat_stat_to_attr (-1, name, attr);
#endif
     }

   return attr->regular && attr->executable;
}

int
__gnat_is_executable_file (char *name)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_executable_file_attr (name, &attr);
}

void
__gnat_set_writable (char *name)
{
#if defined (_WIN32)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

  if (__gnat_can_use_acl (wname))
    __gnat_set_OWNER_ACL (wname, GRANT_ACCESS, FILE_GENERIC_WRITE);

  SetFileAttributes
    (wname, GetFileAttributes (wname) & ~FILE_ATTRIBUTE_READONLY);
#elif ! (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  GNAT_STRUCT_STAT statbuf;

  if (GNAT_STAT (name, &statbuf) == 0)
    {
      statbuf.st_mode = statbuf.st_mode | S_IWUSR;
      chmod (name, statbuf.st_mode);
    }
#endif
}

/* must match definition in s-os_lib.ads */
#define S_OWNER  1
#define S_GROUP  2
#define S_OTHERS 4

void
__gnat_set_executable (char *name, int mode ATTRIBUTE_UNUSED)
{
#if defined (_WIN32)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

  if (__gnat_can_use_acl (wname))
    __gnat_set_OWNER_ACL (wname, GRANT_ACCESS, FILE_GENERIC_EXECUTE);

#elif ! (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  GNAT_STRUCT_STAT statbuf;

  if (GNAT_STAT (name, &statbuf) == 0)
    {
      if (mode & S_OWNER)
        statbuf.st_mode = statbuf.st_mode | S_IXUSR;
      if (mode & S_GROUP)
        statbuf.st_mode = statbuf.st_mode | S_IXGRP;
      if (mode & S_OTHERS)
        statbuf.st_mode = statbuf.st_mode | S_IXOTH;
      chmod (name, statbuf.st_mode);
    }
#endif
}

void
__gnat_set_non_writable (char *name)
{
#if defined (_WIN32)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

  if (__gnat_can_use_acl (wname))
    __gnat_set_OWNER_ACL
      (wname, DENY_ACCESS,
       FILE_WRITE_DATA | FILE_APPEND_DATA |
       FILE_WRITE_EA | FILE_WRITE_ATTRIBUTES);

  SetFileAttributes
    (wname, GetFileAttributes (wname) | FILE_ATTRIBUTE_READONLY);
#elif ! (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  GNAT_STRUCT_STAT statbuf;

  if (GNAT_STAT (name, &statbuf) == 0)
    {
      statbuf.st_mode = statbuf.st_mode & 07577;
      chmod (name, statbuf.st_mode);
    }
#endif
}

void
__gnat_set_readable (char *name)
{
#if defined (_WIN32)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

  if (__gnat_can_use_acl (wname))
    __gnat_set_OWNER_ACL (wname, GRANT_ACCESS, FILE_GENERIC_READ);

#elif ! (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  GNAT_STRUCT_STAT statbuf;

  if (GNAT_STAT (name, &statbuf) == 0)
    {
      chmod (name, statbuf.st_mode | S_IREAD);
    }
#endif
}

void
__gnat_set_non_readable (char *name)
{
#if defined (_WIN32)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  S2WSC (wname, name, GNAT_MAX_PATH_LEN + 2);

  if (__gnat_can_use_acl (wname))
    __gnat_set_OWNER_ACL (wname, DENY_ACCESS, FILE_GENERIC_READ);

#elif ! (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  GNAT_STRUCT_STAT statbuf;

  if (GNAT_STAT (name, &statbuf) == 0)
    {
      chmod (name, statbuf.st_mode & (~S_IREAD));
    }
#endif
}

int
__gnat_is_symbolic_link_attr (char* name ATTRIBUTE_UNUSED,
                              struct file_attributes* attr)
{
   if (attr->symbolic_link == ATTR_UNSET)
     {
#if defined (__vxworks)
       attr->symbolic_link = 0;

#elif defined (_AIX) || defined (__APPLE__) || defined (__unix__)
       int ret;
       GNAT_STRUCT_STAT statbuf;
       ret = GNAT_LSTAT (name, &statbuf);
       attr->symbolic_link = (!ret && S_ISLNK (statbuf.st_mode));
#else
       attr->symbolic_link = 0;
#endif
     }
   return attr->symbolic_link;
}

int
__gnat_is_symbolic_link (char *name ATTRIBUTE_UNUSED)
{
   struct file_attributes attr;

   __gnat_reset_attributes (&attr);
   return __gnat_is_symbolic_link_attr (name, &attr);
}

#if defined (__sun__)
/* Using fork on Solaris will duplicate all the threads. fork1, which
   duplicates only the active thread, must be used instead, or spawning
   subprocess from a program with tasking will lead into numerous problems.  */
#define fork fork1
#endif

int
__gnat_portable_spawn (char *args[] ATTRIBUTE_UNUSED)
{
  int status ATTRIBUTE_UNUSED = 0;
  int finished ATTRIBUTE_UNUSED;
  int pid ATTRIBUTE_UNUSED;

#if defined (__vxworks) || defined(__PikeOS__)
  return -1;

#elif defined (_WIN32)
  /* args[0] must be quotes as it could contain a full pathname with spaces */
  char *args_0 = args[0];
  args[0] = (char *)xmalloc (strlen (args_0) + 3);
  strcpy (args[0], "\"");
  strcat (args[0], args_0);
  strcat (args[0], "\"");

  status = spawnvp (P_WAIT, args_0, (char ** const)args);

  /* restore previous value */
  free (args[0]);
  args[0] = (char *)args_0;

  if (status < 0)
    return -1;
  else
    return status;

#else

  pid = fork ();
  if (pid < 0)
    return -1;

  if (pid == 0)
    {
      /* The child. */
      if (execv (args[0], MAYBE_TO_PTR32 (args)) != 0)
	_exit (1);
    }

  /* The parent.  */
  finished = waitpid (pid, &status, 0);

  if (finished != pid || WIFEXITED (status) == 0)
    return -1;

  return WEXITSTATUS (status);
#endif

  return 0;
}

/* Create a copy of the given file descriptor.
   Return -1 if an error occurred.  */

int
__gnat_dup (int oldfd)
{
#if defined (__vxworks) && !defined (__RTP__)
  /* Not supported on VxWorks 5.x, but supported on VxWorks 6.0 when using
     RTPs. */
  return -1;
#else
  return dup (oldfd);
#endif
}

/* Make newfd be the copy of oldfd, closing newfd first if necessary.
   Return -1 if an error occurred.  */

int
__gnat_dup2 (int oldfd ATTRIBUTE_UNUSED, int newfd ATTRIBUTE_UNUSED)
{
#if defined (__vxworks) && !defined (__RTP__)
  /* Not supported on VxWorks 5.x, but supported on VxWorks 6.0 when using
     RTPs.  */
  return -1;
#elif defined (__PikeOS__)
  /* Not supported.  */
  return -1;
#elif defined (_WIN32)
  /* Special case when oldfd and newfd are identical and are the standard
     input, output or error as this makes Windows XP hangs. Note that we
     do that only for standard file descriptors that are known to be valid. */
  if (oldfd == newfd && newfd >= 0 && newfd <= 2)
    return newfd;
  else
    return dup2 (oldfd, newfd);
#else
  return dup2 (oldfd, newfd);
#endif
}

int
__gnat_number_of_cpus (void)
{
  int cores = 1;

#if defined (__linux__) || defined (__sun__) || defined (_AIX) \
  || defined (__APPLE__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
  || defined (__DragonFly__) || defined (__NetBSD__)
  cores = (int) sysconf (_SC_NPROCESSORS_ONLN);

#elif defined (__hpux__)
  struct pst_dynamic psd;
  if (pstat_getdynamic (&psd, sizeof (psd), 1, 0) != -1)
    cores = (int) psd.psd_proc_cnt;

#elif defined (_WIN32)
  SYSTEM_INFO sysinfo;
  GetSystemInfo (&sysinfo);
  cores = (int) sysinfo.dwNumberOfProcessors;

#elif defined (_WRS_CONFIG_SMP)
  unsigned int vxCpuConfiguredGet (void);

  cores = vxCpuConfiguredGet ();

#endif

  return cores;
}

/* WIN32 code to implement a wait call that wait for any child process.  */

#if defined (_WIN32)

/* Synchronization code, to be thread safe.  */

#ifdef CERT

/* For the Cert run times on native Windows we use dummy functions
   for locking and unlocking tasks since we do not support multiple
   threads on this configuration (Cert run time on native Windows). */

static void EnterCS (void) {}
static void LeaveCS (void) {}
static void SignalListChanged (void) {}

#else

CRITICAL_SECTION ProcListCS;
HANDLE ProcListEvt = NULL;

static void EnterCS (void)
{
  EnterCriticalSection(&ProcListCS);
}

static void LeaveCS (void)
{
  LeaveCriticalSection(&ProcListCS);
}

static void SignalListChanged (void)
{
  SetEvent (ProcListEvt);
}

#endif

static HANDLE *HANDLES_LIST = NULL;
static int *PID_LIST = NULL, plist_length = 0, plist_max_length = 0;

static void
add_handle (HANDLE h, int pid)
{
  /* -------------------- critical section -------------------- */
  EnterCS();

  if (plist_length == plist_max_length)
    {
      plist_max_length += 100;
      HANDLES_LIST =
        (HANDLE *) xrealloc (HANDLES_LIST, sizeof (HANDLE) * plist_max_length);
      PID_LIST =
        (int *) xrealloc (PID_LIST, sizeof (int) * plist_max_length);
    }

  HANDLES_LIST[plist_length] = h;
  PID_LIST[plist_length] = pid;
  ++plist_length;

  SignalListChanged();
  LeaveCS();
  /* -------------------- critical section -------------------- */
}

int
__gnat_win32_remove_handle (HANDLE h, int pid)
{
  int j;
  int found = 0;

  /* -------------------- critical section -------------------- */
  EnterCS();

  for (j = 0; j < plist_length; j++)
    {
      if ((HANDLES_LIST[j] == h) || (PID_LIST[j] == pid))
        {
          CloseHandle (h);
          --plist_length;
          HANDLES_LIST[j] = HANDLES_LIST[plist_length];
          PID_LIST[j] = PID_LIST[plist_length];
          found = 1;
          break;
        }
    }

  LeaveCS();
  /* -------------------- critical section -------------------- */

  if (found)
    SignalListChanged();

  return found;
}

static void
win32_no_block_spawn (char *command, char *args[], HANDLE *h, int *pid)
{
  BOOL result;
  STARTUPINFO SI;
  PROCESS_INFORMATION PI;
  SECURITY_ATTRIBUTES SA;
  int csize = 1;
  char *full_command;
  int k;

  /* compute the total command line length */
  k = 0;
  while (args[k])
    {
      csize += strlen (args[k]) + 1;
      k++;
    }

  full_command = (char *) xmalloc (csize);

  /* Startup info. */
  SI.cb          = sizeof (STARTUPINFO);
  SI.lpReserved  = NULL;
  SI.lpReserved2 = NULL;
  SI.lpDesktop   = NULL;
  SI.cbReserved2 = 0;
  SI.lpTitle     = NULL;
  SI.dwFlags     = 0;
  SI.wShowWindow = SW_HIDE;

  /* Security attributes. */
  SA.nLength = sizeof (SECURITY_ATTRIBUTES);
  SA.bInheritHandle = TRUE;
  SA.lpSecurityDescriptor = NULL;

  /* Prepare the command string. */
  strcpy (full_command, command);
  strcat (full_command, " ");

  k = 1;
  while (args[k])
    {
      strcat (full_command, args[k]);
      strcat (full_command, " ");
      k++;
    }

  {
    int wsize = csize * 2;
    TCHAR *wcommand = (TCHAR *) xmalloc (wsize);

    S2WSC (wcommand, full_command, wsize);

    free (full_command);

    result = CreateProcess
      (NULL, wcommand, &SA, NULL, TRUE,
       GetPriorityClass (GetCurrentProcess()), NULL, NULL, &SI, &PI);

    free (wcommand);
  }

  if (result == TRUE)
    {
      CloseHandle (PI.hThread);
      *h = PI.hProcess;
      *pid = PI.dwProcessId;
    }
  else
    {
      *h = NULL;
      *pid = 0;
    }
}

static int
win32_wait (int *status)
{
  DWORD exitcode, pid;
  HANDLE *hl;
  HANDLE h;
  int *pidl;
  DWORD res;
  int hl_len;
  int found;

 START_WAIT:

  if (plist_length == 0)
    {
      errno = ECHILD;
      return -1;
    }

  /* -------------------- critical section -------------------- */
  EnterCS();

  hl_len = plist_length;

#ifdef CERT
  hl = (HANDLE *) xmalloc (sizeof (HANDLE) * hl_len);
  memmove (hl, HANDLES_LIST, sizeof (HANDLE) * hl_len);
  pidl = (int *) xmalloc (sizeof (int) * hl_len);
  memmove (pidl, PID_LIST, sizeof (int) * hl_len);
#else
  /* Note that index 0 contains the event handle that is signaled when the
     process list has changed */
  hl = (HANDLE *) xmalloc (sizeof (HANDLE) * hl_len + 1);
  hl[0] = ProcListEvt;
  memmove (&hl[1], HANDLES_LIST, sizeof (HANDLE) * hl_len);
  pidl = (int *) xmalloc (sizeof (int) * hl_len + 1);
  memmove (&pidl[1], PID_LIST, sizeof (int) * hl_len);
  hl_len++;
#endif

  LeaveCS();
  /* -------------------- critical section -------------------- */

  res = WaitForMultipleObjects (hl_len, hl, FALSE, INFINITE);

  /* if the ProcListEvt has been signaled then the list of processes has been
     updated to add or remove a handle, just loop over */

  if (res - WAIT_OBJECT_0 == 0)
    {
      free (hl);
      free (pidl);
      goto START_WAIT;
    }

  h = hl[res - WAIT_OBJECT_0];
  GetExitCodeProcess (h, &exitcode);
  pid = pidl [res - WAIT_OBJECT_0];

  found = __gnat_win32_remove_handle (h, -1);

  free (hl);
  free (pidl);

  /* if not found another process waiting has already handled this process */

  if (!found)
    {
      goto START_WAIT;
    }

  *status = (int) exitcode;
  return (int) pid;
}

#endif

int
__gnat_portable_no_block_spawn (char *args[] ATTRIBUTE_UNUSED)
{

#if defined (__vxworks) || defined (__PikeOS__)
  /* Not supported.  */
  return -1;

#elif defined (_WIN32)

  HANDLE h = NULL;
  int pid;

  win32_no_block_spawn (args[0], args, &h, &pid);
  if (h != NULL)
    {
      add_handle (h, pid);
      return pid;
    }
  else
    return -1;

#else

  int pid = fork ();

  if (pid == 0)
    {
      /* The child.  */
      if (execv (args[0], MAYBE_TO_PTR32 (args)) != 0)
	_exit (1);
    }

  return pid;

  #endif
}

int
__gnat_portable_wait (int *process_status)
{
  int status = 0;
  int pid = 0;

#if defined (__vxworks) || defined (__PikeOS__)
  /* Not sure what to do here, so do nothing but return zero.  */

#elif defined (_WIN32)

  pid = win32_wait (&status);

#else

  pid = waitpid (-1, &status, 0);
  status = status & 0xffff;
#endif

  *process_status = status;
  return pid;
}

void
__gnat_os_exit (int status)
{
  exit (status);
}

int
__gnat_current_process_id (void)
{
#if defined (__vxworks) || defined (__PikeOS__)
  return -1;

#elif defined (_WIN32)

  return (int)GetCurrentProcessId();

#else

  return (int)getpid();
#endif
}

/* Locate file on path, that matches a predicate */

char *
__gnat_locate_file_with_predicate (char *file_name, char *path_val,
				   int (*predicate)(char *))
{
  char *ptr;
  char *file_path = (char *) alloca (strlen (file_name) + 1);
  int absolute;

  /* Return immediately if file_name is empty */

  if (*file_name == '\0')
    return 0;

  /* Remove quotes around file_name if present */

  ptr = file_name;
  if (*ptr == '"')
    ptr++;

  strcpy (file_path, ptr);

  ptr = file_path + strlen (file_path) - 1;

  if (*ptr == '"')
    *ptr = '\0';

  /* Handle absolute pathnames.  */

  absolute = __gnat_is_absolute_path (file_path, strlen (file_name));

  if (absolute)
    {
     if (predicate (file_path))
       return xstrdup (file_path);

      return 0;
    }

  /* If file_name include directory separator(s), try it first as
     a path name relative to the current directory */
  for (ptr = file_name; *ptr && *ptr != '/' && *ptr != DIR_SEPARATOR; ptr++)
    ;

  if (*ptr != 0)
    {
      if (predicate (file_name))
        return xstrdup (file_name);
    }

  if (path_val == 0)
    return 0;

  {
    /* The result has to be smaller than path_val + file_name.  */
    char *file_path =
      (char *) alloca (strlen (path_val) + strlen (file_name) + 2);

    for (;;)
      {
      /* Skip the starting quote */

      if (*path_val == '"')
	path_val++;

      for (ptr = file_path; *path_val && *path_val != PATH_SEPARATOR; )
	*ptr++ = *path_val++;

      /* If directory is empty, it is the current directory*/

      if (ptr == file_path)
        {
         *ptr = '.';
        }
      else
        ptr--;

      /* Skip the ending quote */

      if (*ptr == '"')
	ptr--;

      if (*ptr != '/' && *ptr != DIR_SEPARATOR)
        *++ptr = DIR_SEPARATOR;

      strcpy (++ptr, file_name);

      if (predicate (file_path))
        return xstrdup (file_path);

      if (*path_val == 0)
        return 0;

      /* Skip path separator */

      path_val++;
      }
  }

  return 0;
}

/* Locate an executable file, give a Path value.  */

char *
__gnat_locate_executable_file (char *file_name, char *path_val)
{
   return __gnat_locate_file_with_predicate
      (file_name, path_val, &__gnat_is_executable_file);
}

/* Locate a regular file, give a Path value.  */

char *
__gnat_locate_regular_file (char *file_name, char *path_val)
{
   return __gnat_locate_file_with_predicate
      (file_name, path_val, &__gnat_is_regular_file);
}

/* Locate an executable given a Path argument. This routine is only used by
   gnatbl and should not be used otherwise.  Use locate_exec_on_path
   instead.  */

char *
__gnat_locate_exec (char *exec_name, char *path_val)
{
  char *ptr;
  if (!strstr (exec_name, HOST_EXECUTABLE_SUFFIX))
    {
      char *full_exec_name =
        (char *) alloca
	  (strlen (exec_name) + strlen (HOST_EXECUTABLE_SUFFIX) + 1);

      strcpy (full_exec_name, exec_name);
      strcat (full_exec_name, HOST_EXECUTABLE_SUFFIX);
      ptr = __gnat_locate_executable_file (full_exec_name, path_val);

      if (ptr == 0)
         return __gnat_locate_executable_file (exec_name, path_val);
      return ptr;
    }
  else
    return __gnat_locate_executable_file (exec_name, path_val);
}

/* Locate an executable using the Systems default PATH.  */

char *
__gnat_locate_exec_on_path (char *exec_name)
{
  char *apath_val;

#if defined (_WIN32)
  TCHAR *wpath_val = _tgetenv (_T("PATH"));
  TCHAR *wapath_val;
  /* In Win32 systems we expand the PATH as for XP environment
     variables are not automatically expanded. We also prepend the
     ".;" to the path to match normal NT path search semantics */

  #define EXPAND_BUFFER_SIZE 32767

  wapath_val = (TCHAR *) alloca (EXPAND_BUFFER_SIZE);

  wapath_val [0] = '.';
  wapath_val [1] = ';';

  DWORD res = ExpandEnvironmentStrings
    (wpath_val, &wapath_val[2], EXPAND_BUFFER_SIZE - 2);

  if (!res) wapath_val [0] = _T('\0');

  apath_val = (char *) alloca (EXPAND_BUFFER_SIZE);

  WS2SC (apath_val, wapath_val, EXPAND_BUFFER_SIZE);

#else
  const char *path_val = getenv ("PATH");

  /* If PATH is not defined, proceed with __gnat_locate_exec anyway, so we can
     find files that contain directory names.  */

  if (path_val == NULL) path_val = "";
  apath_val = (char *) alloca (strlen (path_val) + 1);
  strcpy (apath_val, path_val);
#endif

  return __gnat_locate_exec (exec_name, apath_val);
}

/* Dummy functions for Osint import for non-VMS systems.
   ??? To be removed.  */

int
__gnat_to_canonical_file_list_init (char *dirspec ATTRIBUTE_UNUSED,
				    int onlydirs ATTRIBUTE_UNUSED)
{
  return 0;
}

char *
__gnat_to_canonical_file_list_next (void)
{
  static char empty[] = "";
  return empty;
}

void
__gnat_to_canonical_file_list_free (void)
{
}

char *
__gnat_to_canonical_dir_spec (char *dirspec, int prefixflag ATTRIBUTE_UNUSED)
{
  return dirspec;
}

char *
__gnat_to_canonical_file_spec (char *filespec)
{
  return filespec;
}

char *
__gnat_to_canonical_path_spec (char *pathspec)
{
  return pathspec;
}

char *
__gnat_to_host_dir_spec (char *dirspec, int prefixflag ATTRIBUTE_UNUSED)
{
  return dirspec;
}

char *
__gnat_to_host_file_spec (char *filespec)
{
  return filespec;
}

void
__gnat_adjust_os_resource_limits (void)
{
}

#if defined (__mips_vxworks)
int
_flush_cache (void)
{
   CACHE_USER_FLUSH (0, ENTIRE_CACHE);
}
#endif

#if defined (_WIN32)
int __gnat_argument_needs_quote = 1;
#else
int __gnat_argument_needs_quote = 0;
#endif

/* This option is used to enable/disable object files handling from the
   binder file by the GNAT Project module. For example, this is disabled on
   Windows (prior to GCC 3.4) as it is already done by the mdll module.
   Stating with GCC 3.4 the shared libraries are not based on mdll
   anymore as it uses the GCC's -shared option  */
#if defined (_WIN32) \
    && ((__GNUC__ < 3) || ((__GNUC__ == 3) && (__GNUC_MINOR__ < 4)))
int __gnat_prj_add_obj_files = 0;
#else
int __gnat_prj_add_obj_files = 1;
#endif

/* char used as prefix/suffix for environment variables */
#if defined (_WIN32)
char __gnat_environment_char = '%';
#else
char __gnat_environment_char = '$';
#endif

/* This functions copy the file attributes from a source file to a
   destination file.

   mode = 0  : In this mode copy only the file time stamps (last access and
               last modification time stamps).

   mode = 1  : In this mode, time stamps and read/write/execute attributes are
               copied.

   mode = 2  : In this mode, only read/write/execute attributes are copied

   Returns 0 if operation was successful and -1 in case of error. */

int
__gnat_copy_attribs (char *from ATTRIBUTE_UNUSED, char *to ATTRIBUTE_UNUSED,
                     int mode ATTRIBUTE_UNUSED)
{
#if (defined (__vxworks) && _WRS_VXWORKS_MAJOR < 6)
  return -1;

#elif defined (_WIN32)
  TCHAR wfrom [GNAT_MAX_PATH_LEN + 2];
  TCHAR wto [GNAT_MAX_PATH_LEN + 2];
  BOOL res;
  FILETIME fct, flat, flwt;
  HANDLE hfrom, hto;

  S2WSC (wfrom, from, GNAT_MAX_PATH_LEN + 2);
  S2WSC (wto, to, GNAT_MAX_PATH_LEN + 2);

  /*  Do we need to copy the timestamp ? */

  if (mode != 2) {
     /* retrieve from times */

     hfrom = CreateFile
       (wfrom, GENERIC_READ, 0, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);

     if (hfrom == INVALID_HANDLE_VALUE)
       return -1;

     res = GetFileTime (hfrom, &fct, &flat, &flwt);

     CloseHandle (hfrom);

     if (res == 0)
       return -1;

     /* retrieve from times */

     hto = CreateFile
       (wto, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);

     if (hto == INVALID_HANDLE_VALUE)
       return -1;

     res = SetFileTime (hto, NULL, &flat, &flwt);

     CloseHandle (hto);

     if (res == 0)
       return -1;
  }

  /* Do we need to copy the permissions ? */
  /* Set file attributes in full mode. */

  if (mode != 0)
    {
      DWORD attribs = GetFileAttributes (wfrom);

      if (attribs == INVALID_FILE_ATTRIBUTES)
	return -1;

      res = SetFileAttributes (wto, attribs);
      if (res == 0)
	return -1;
    }

  return 0;

#else
  GNAT_STRUCT_STAT fbuf;
  struct utimbuf tbuf;

  if (GNAT_STAT (from, &fbuf) == -1) {
     return -1;
  }

  /* Do we need to copy timestamp ? */
  if (mode != 2) {
     tbuf.actime = fbuf.st_atime;
     tbuf.modtime = fbuf.st_mtime;

     if (utime (to, &tbuf) == -1) {
        return -1;
     }
  }

  /* Do we need to copy file permissions ? */
  if (mode != 0 && (chmod (to, fbuf.st_mode) == -1)) {
	  return -1;
  }

  return 0;
#endif
}

int
__gnat_lseek (int fd, long offset, int whence)
{
  return (int) lseek (fd, offset, whence);
}

/* This function returns the major version number of GCC being used.  */
int
get_gcc_version (void)
{
#ifdef IN_RTS
  return __GNUC__;
#else
  return (int) (version_string[0] - '0');
#endif
}

/*
 * Set Close_On_Exec as indicated.
 * Note: this is used for both GNAT.OS_Lib and GNAT.Sockets.
 */

int
__gnat_set_close_on_exec (int fd ATTRIBUTE_UNUSED,
                          int close_on_exec_p ATTRIBUTE_UNUSED)
{
#if defined (F_GETFD) && defined (FD_CLOEXEC) && ! defined (__vxworks)
  int flags = fcntl (fd, F_GETFD, 0);
  if (flags < 0)
    return flags;
  if (close_on_exec_p)
    flags |= FD_CLOEXEC;
  else
    flags &= ~FD_CLOEXEC;
  return fcntl (fd, F_SETFD, flags);
#elif defined(_WIN32)
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  if (h == (HANDLE) -1)
    return -1;
  if (close_on_exec_p)
    return ! SetHandleInformation (h, HANDLE_FLAG_INHERIT, 0);
  return ! SetHandleInformation (h, HANDLE_FLAG_INHERIT,
    HANDLE_FLAG_INHERIT);
#else
  /* TODO: Unimplemented. */
  return -1;
#endif
}

/* Indicates if platforms supports automatic initialization through the
   constructor mechanism */
int
__gnat_binder_supports_auto_init (void)
{
  return 1;
}

/* Indicates that Stand-Alone Libraries are automatically initialized through
   the constructor mechanism */
int
__gnat_sals_init_using_constructors (void)
{
#if defined (__vxworks) || defined (__Lynx__)
   return 0;
#else
   return 1;
#endif
}

#if defined (__linux__) || defined (__ANDROID__)
/* There is no function in the glibc to retrieve the LWP of the current
   thread. We need to do a system call in order to retrieve this
   information. */
#include <sys/syscall.h>
void *
__gnat_lwp_self (void)
{
   return (void *) syscall (__NR_gettid);
}
#endif

#if defined (__linux__)
#include <sched.h>

/* glibc versions earlier than 2.7 do not define the routines to handle
   dynamically allocated CPU sets. For these targets, we use the static
   versions. */

#ifdef CPU_ALLOC

/* Dynamic cpu sets */

cpu_set_t *
__gnat_cpu_alloc (size_t count)
{
  return CPU_ALLOC (count);
}

size_t
__gnat_cpu_alloc_size (size_t count)
{
  return CPU_ALLOC_SIZE (count);
}

void
__gnat_cpu_free (cpu_set_t *set)
{
  CPU_FREE (set);
}

void
__gnat_cpu_zero (size_t count, cpu_set_t *set)
{
  CPU_ZERO_S (count, set);
}

void
__gnat_cpu_set (int cpu, size_t count, cpu_set_t *set)
{
  /* Ada handles CPU numbers starting from 1, while C identifies the first
     CPU by a 0, so we need to adjust. */
  CPU_SET_S (cpu - 1, count, set);
}

#else /* !CPU_ALLOC */

/* Static cpu sets */

cpu_set_t *
__gnat_cpu_alloc (size_t count ATTRIBUTE_UNUSED)
{
  return (cpu_set_t *) xmalloc (sizeof (cpu_set_t));
}

size_t
__gnat_cpu_alloc_size (size_t count ATTRIBUTE_UNUSED)
{
  return sizeof (cpu_set_t);
}

void
__gnat_cpu_free (cpu_set_t *set)
{
  free (set);
}

void
__gnat_cpu_zero (size_t count ATTRIBUTE_UNUSED, cpu_set_t *set)
{
  CPU_ZERO (set);
}

void
__gnat_cpu_set (int cpu, size_t count ATTRIBUTE_UNUSED, cpu_set_t *set)
{
  /* Ada handles CPU numbers starting from 1, while C identifies the first
     CPU by a 0, so we need to adjust. */
  CPU_SET (cpu - 1, set);
}
#endif /* !CPU_ALLOC */
#endif /* __linux__ */

/* Return the load address of the executable, or 0 if not known.  In the
   specific case of error, (void *)-1 can be returned. Beware: this unit may
   be in a shared library.  As low-level units are needed, we allow #include
   here.  */

#if defined (__APPLE__)
#include <mach-o/dyld.h>
#endif

const void *
__gnat_get_executable_load_address (void)
{
#if defined (__APPLE__)
  return _dyld_get_image_header (0);

#elif 0 && defined (__linux__)
  /* Currently disabled as it needs at least -ldl.  */
  struct link_map *map = _r_debug.r_map;

  return (const void *)map->l_addr;

#else
  return NULL;
#endif
}

void
__gnat_kill (int pid, int sig, int close ATTRIBUTE_UNUSED)
{
#if defined(_WIN32)
  HANDLE h = OpenProcess (PROCESS_ALL_ACCESS, FALSE, pid);
  if (h == NULL)
    return;
  if (sig == 9)
    {
      TerminateProcess (h, 1);
    }
  else if (sig == SIGINT)
    GenerateConsoleCtrlEvent (CTRL_C_EVENT, pid);
  else if (sig == SIGBREAK)
    GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid);
  /* ??? The last two alternatives don't really work. SIGBREAK requires setting
     up process groups at start time which we don't do; treating SIGINT is just
     not possible apparently. So we really only support signal 9. Fortunately
     that's all we use in GNAT.Expect */

  CloseHandle (h);
#elif defined (__vxworks)
  /* Not implemented */
#else
  kill (pid, sig);
#endif
}

void __gnat_killprocesstree (int pid, int sig_num)
{
#if defined(_WIN32)
  HANDLE hWnd;
  PROCESSENTRY32 pe;

  memset(&pe, 0, sizeof(PROCESSENTRY32));
  pe.dwSize = sizeof(PROCESSENTRY32);

  HANDLE hSnap = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

  /*  cannot take snapshot, just kill the parent process */

  if (hSnap == INVALID_HANDLE_VALUE)
    {
      __gnat_kill (pid, sig_num, 1);
      return;
    }

  if (Process32First(hSnap, &pe))
    {
      BOOL bContinue = TRUE;

      /* kill child processes first */

      while (bContinue)
        {
          if (pe.th32ParentProcessID == (int)pid)
            __gnat_killprocesstree (pe.th32ProcessID, sig_num);

          bContinue = Process32Next (hSnap, &pe);
        }
    }

  CloseHandle (hSnap);

  /* kill process */

  __gnat_kill (pid, sig_num, 1);

#elif defined (__vxworks)
  /* not implemented */

#elif defined (__linux__)
  DIR *dir;
  struct dirent *d;

  /*  read all processes' pid and ppid */

  dir = opendir ("/proc");

  /*  cannot open proc, just kill the parent process */

  if (!dir)
    {
      __gnat_kill (pid, sig_num, 1);
      return;
    }

  /* kill child processes first */

  while ((d = readdir (dir)) != NULL)
    {
      if ((d->d_type & DT_DIR) == DT_DIR)
        {
          char statfile[64] = { 0 };
          int _pid, _ppid;

          /* read /proc/<PID>/stat */

          strncpy (statfile, "/proc/", sizeof(statfile));
          strncat (statfile, d->d_name, sizeof(statfile));
          strncat (statfile, "/stat", sizeof(statfile));

          FILE *fd = fopen (statfile, "r");

          if (fd)
            {
              const int match = fscanf (fd, "%d %*s %*s %d", &_pid, &_ppid);
              fclose (fd);

              if (match == 2 && _ppid == pid)
                __gnat_killprocesstree (_pid, sig_num);
            }
        }
    }

  closedir (dir);

  /* kill process */

  __gnat_kill (pid, sig_num, 1);
#else
  __gnat_kill (pid, sig_num, 1);
#endif
  /* Note on Solaris it is possible to read /proc/<PID>/status.
     The 5th and 6th words are the pid and the 7th and 8th the ppid.
     See: /usr/include/sys/procfs.h (struct pstatus).
  */
}

#ifdef __cplusplus
}
#endif
