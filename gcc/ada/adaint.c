/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               A D A I N T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
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

/* This file contains those routines named by Import pragmas in
   packages in the GNAT hierarchy (especially GNAT.OS_Lib) and in
   package Osint.  Many of the subprograms in OS_Lib import standard
   library calls directly. This file contains all other routines.  */

#ifdef __vxworks

/* No need to redefine exit here.  */
#undef exit

/* We want to use the POSIX variants of include files.  */
#define POSIX
#include "vxWorks.h"

#if defined (__mips_vxworks)
#include "cacheLib.h"
#endif /* __mips_vxworks */

#endif /* VxWorks */

#ifdef VMS
#define _POSIX_EXIT 1
#define HOST_EXECUTABLE_SUFFIX ".exe"
#define HOST_OBJECT_SUFFIX ".obj"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"

#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#ifdef VMS
#include <unixio.h>
#endif

/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#define xrealloc(V,S) realloc (V,S)
#else
#include "config.h"
#include "system.h"
#endif

#ifdef __MINGW32__
#include "mingw32.h"
#include <sys/utime.h>
#include <ctype.h>
#else
#ifndef VMS
#include <utime.h>
#endif
#endif

#ifdef __MINGW32__
#if OLD_MINGW
#include <sys/wait.h>
#endif
#elif defined (__vxworks) && defined (__RTP__)
#include <wait.h>
#else
#include <sys/wait.h>
#endif

#if defined (__EMX__) || defined (MSDOS) || defined (_WIN32)
#elif defined (VMS)

/* Header files and definitions for __gnat_set_file_time_name.  */

#include <vms/rms.h>
#include <vms/atrdef.h>
#include <vms/fibdef.h>
#include <vms/stsdef.h>
#include <vms/iodef.h>
#include <errno.h>
#include <vms/descrip.h>
#include <string.h>
#include <unixlib.h>

/* Use native 64-bit arithmetic.  */
#define unix_time_to_vms(X,Y) \
  { unsigned long long reftime, tmptime = (X); \
    $DESCRIPTOR (unixtime,"1-JAN-1970 0:00:00.00"); \
    SYS$BINTIM (&unixtime, &reftime); \
    Y = tmptime * 10000000 + reftime; }

/* descrip.h doesn't have everything ... */
struct dsc$descriptor_fib
{
  unsigned long fib$l_len;
  struct fibdef *fib$l_addr;
};

/* I/O Status Block.  */
struct IOSB
{
  unsigned short status, count;
  unsigned long devdep;
};

static char *tryfile;

/* Variable length string.  */
struct vstring
{
  short length;
  char string[NAM$C_MAXRSS+1];
};

#else
#include <utime.h>
#endif

#if defined (__EMX__) || defined (MSDOS) || defined (_WIN32)
#include <process.h>
#endif

#if defined (_WIN32)
#include <dir.h>
#include <windows.h>
#undef DIR_SEPARATOR
#define DIR_SEPARATOR '\\'
#endif

#include "adaint.h"

/* Define symbols O_BINARY and O_TEXT as harmless zeroes if they are not
   defined in the current system. On DOS-like systems these flags control
   whether the file is opened/created in text-translation mode (CR/LF in
   external file mapped to LF in internal file), but in Unix-like systems,
   no text translation is required, so these flags have no effect.  */

#if defined (__EMX__)
#include <os2.h>
#endif

#if defined (MSDOS)
#include <dos.h>
#endif

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
#if defined (__EMX__)
#define GNAT_LIBRARY_TEMPLATE "*.a"
#elif defined (VMS)
#define GNAT_LIBRARY_TEMPLATE "*.olb"
#else
#define GNAT_LIBRARY_TEMPLATE "lib*.a"
#endif
#endif

const char *__gnat_library_template = GNAT_LIBRARY_TEMPLATE;

/* This variable is used in hostparm.ads to say whether the host is a VMS
   system.  */
#ifdef VMS
const int __gnat_vmsp = 1;
#else
const int __gnat_vmsp = 0;
#endif

#ifdef __EMX__
#define GNAT_MAX_PATH_LEN MAX_PATH

#elif defined (VMS)
#define GNAT_MAX_PATH_LEN 256 /* PATH_MAX */

#elif defined (__vxworks) || defined (__OPENNT)
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

/* The __gnat_max_path_len variable is used to export the maximum
   length of a path name to Ada code. max_path_len is also provided
   for compatibility with older GNAT versions, please do not use
   it. */

int __gnat_max_path_len = GNAT_MAX_PATH_LEN;
int max_path_len = GNAT_MAX_PATH_LEN;

/* The following macro HAVE_READDIR_R should be defined if the
   system provides the routine readdir_r.  */
#undef HAVE_READDIR_R

#if defined(VMS) && defined (__LONG_POINTERS)

/* Return a 32 bit pointer to an array of 32 bit pointers
   given a 64 bit pointer to an array of 64 bit pointers */

typedef __char_ptr32 *__char_ptr_char_ptr32 __attribute__ ((mode (SI)));

static __char_ptr_char_ptr32
to_ptr32 (char **ptr64)
{
  int argc;
  __char_ptr_char_ptr32 short_argv;

  for (argc=0; ptr64[argc]; argc++);

  /* Reallocate argv with 32 bit pointers. */
  short_argv = (__char_ptr_char_ptr32) decc$malloc
    (sizeof (__char_ptr32) * (argc + 1));

  for (argc=0; ptr64[argc]; argc++)
    short_argv[argc] = (__char_ptr32) decc$strdup (ptr64[argc]);

  short_argv[argc] = (__char_ptr32) 0;
  return short_argv;

}
#define MAYBE_TO_PTR32(argv) to_ptr32 (argv)
#else
#define MAYBE_TO_PTR32(argv) argv
#endif

void
__gnat_to_gm_time
  (OS_Time *p_time,
   int *p_year,
   int *p_month,
   int *p_day,
   int *p_hours,
   int *p_mins,
   int *p_secs)
{
  struct tm *res;
  time_t time = (time_t) *p_time;

#ifdef _WIN32
  /* On Windows systems, the time is sometimes rounded up to the nearest
     even second, so if the number of seconds is odd, increment it.  */
  if (time & 1)
    time++;
#endif

#ifdef VMS
  res = localtime (&time);
#else
  res = gmtime (&time);
#endif

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

/* Place the contents of the symbolic link named PATH in the buffer BUF,
   which has size BUFSIZ.  If PATH is a symbolic link, then return the number
   of characters of its content in BUF.  Otherwise, return -1.  For Windows,
   OS/2 and vxworks, always return -1.  */

int
__gnat_readlink (char *path ATTRIBUTE_UNUSED,
		 char *buf ATTRIBUTE_UNUSED,
		 size_t bufsiz ATTRIBUTE_UNUSED)
{
#if defined (MSDOS) || defined (_WIN32) || defined (__EMX__)
  return -1;
#elif defined (__INTERIX) || defined (VMS)
  return -1;
#elif defined (__vxworks)
  return -1;
#else
  return readlink (path, buf, bufsiz);
#endif
}

/* Creates a symbolic link named NEWPATH which contains the string OLDPATH.  If
   NEWPATH exists it will NOT be overwritten.  For Windows, OS/2, VxWorks,
   Interix and VMS, always return -1. */

int
__gnat_symlink (char *oldpath ATTRIBUTE_UNUSED,
		char *newpath ATTRIBUTE_UNUSED)
{
#if defined (MSDOS) || defined (_WIN32) || defined (__EMX__)
  return -1;
#elif defined (__INTERIX) || defined (VMS)
  return -1;
#elif defined (__vxworks)
  return -1;
#else
  return symlink (oldpath, newpath);
#endif
}

/* Try to lock a file, return 1 if success.  */

#if defined (__vxworks) || defined (MSDOS) || defined (_WIN32)

/* Version that does not use link. */

int
__gnat_try_lock (char *dir, char *file)
{
  char full_path[256];
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  fd = open (full_path, O_CREAT | O_EXCL, 0600);
  if (fd < 0)
    return 0;

  close (fd);
  return 1;
}

#elif defined (__EMX__) || defined (VMS)

/* More cases that do not use link; identical code, to solve too long
   line problem ??? */

int
__gnat_try_lock (char *dir, char *file)
{
  char full_path[256];
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  fd = open (full_path, O_CREAT | O_EXCL, 0600);
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
  struct stat stat_result;
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
#if defined (MSDOS)
  return 8;
#elif defined (VMS)
  if (getenv ("GNAT$EXTENDED_FILE_SPECIFICATIONS"))
    return -1;
  else
    return 39;
#else
  return -1;
#endif
}

/* Return nonzero if file names are case sensitive.  */

int
__gnat_get_file_names_case_sensitive (void)
{
#if defined (__EMX__) || defined (MSDOS) || defined (VMS) || defined (WINNT)
  return 0;
#else
  return 1;
#endif
}

char
__gnat_get_default_identifier_character_set (void)
{
#if defined (__EMX__) || defined (MSDOS)
  return 'p';
#else
  return '1';
#endif
}

/* Return the current working directory.  */

void
__gnat_get_current_dir (char *dir, int *length)
{
#ifdef VMS
   /* Force Unix style, which is what GNAT uses internally.  */
   getcwd (dir, *length, 0);
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
#ifndef MSDOS
  *value = HOST_EXECUTABLE_SUFFIX;
#else
  /* On DOS, the extensionless COFF file is what gdb likes.  */
  *value = "";
#endif

  if (*value == 0)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

int
__gnat_open_read (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined (VMS)
  /* Optional arguments mbc,deq,fop increase read performance.  */
  fd = open (path, O_RDONLY | o_fmode, 0444,
             "mbc=16", "deq=64", "fop=tef");
#elif defined (__vxworks)
  fd = open (path, O_RDONLY | o_fmode, 0444);
#else
  fd = open (path, O_RDONLY | o_fmode);
#endif

  return fd < 0 ? -1 : fd;
}

#if defined (__EMX__) || defined (__MINGW32__)
#define PERM (S_IREAD | S_IWRITE)
#elif defined (VMS)
/* Excerpt from DECC C RTL Reference Manual:
   To create files with OpenVMS RMS default protections using the UNIX
   system-call functions umask, mkdir, creat, and open, call mkdir, creat,
   and open with a file-protection mode argument of 0777 in a program
   that never specifically calls umask. These default protections include
   correctly establishing protections based on ACLs, previous versions of
   files, and so on. */
#define PERM 0777
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

#if defined (VMS)
  fd = open (path, O_RDWR | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_RDWR | o_fmode, PERM);
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

#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_create_output_file (char *path)
{
  int fd;
#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM,
             "rfm=stmlf", "ctx=rec", "rat=none", "rop=nlk",
             "shr=del,get,put,upd");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM);
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

#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM);
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

#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/* Open a new temp file.  Return error (-1) if the file already exists.
   Special options for VMS allow the file to be shared between parent and child
   processes, however they really slow down output.  Used in gnatchop.  */

int
__gnat_open_new_temp (char *path, int fmode)
{
  int fd;
  int o_fmode = O_BINARY;

  strcpy (path, "GNAT-XXXXXX");

#if (defined (__FreeBSD__) || defined (linux)) && !defined (__vxworks)
  return mkstemp (path);
#elif defined (__Lynx__)
  mktemp (path);
#else
  if (mktemp (path) == NULL)
    return -1;
#endif

  if (fmode)
    o_fmode = O_TEXT;

#if defined (VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM,
             "rfm=stmlf", "ctx=rec", "rat=none", "shr=del,get,put,upd",
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/* Return the number of bytes in the specified file.  */

long
__gnat_file_length (int fd)
{
  int ret;
  struct stat statbuf;

  ret = fstat (fd, &statbuf);
  if (ret || !S_ISREG (statbuf.st_mode))
    return 0;

  return (statbuf.st_size);
}

/* Return the number of bytes in the specified named file.  */

long
__gnat_named_file_length (char *name)
{
  int ret;
  struct stat statbuf;

  ret = __gnat_stat (name, &statbuf);
  if (ret || !S_ISREG (statbuf.st_mode))
    return 0;

  return (statbuf.st_size);
}

/* Create a temporary filename and put it in string pointed to by
   TMP_FILENAME.  */

void
__gnat_tmp_name (char *tmp_filename)
{
#ifdef __MINGW32__
  {
    char *pname;

    /* tempnam tries to create a temporary file in directory pointed to by
       TMP environment variable, in c:\temp if TMP is not set, and in
       directory specified by P_tmpdir in stdio.h if c:\temp does not
       exist. The filename will be created with the prefix "gnat-".  */

    pname = (char *) tempnam ("c:\\temp", "gnat-");

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

#elif defined (linux) || defined (__FreeBSD__)
#define MAX_SAFE_PATH 1000
  char *tmpdir = getenv ("TMPDIR");

  /* If tmpdir is longer than MAX_SAFE_PATH, revert to default value to avoid
     a buffer overflow.  */
  if (tmpdir == NULL || strlen (tmpdir) > MAX_SAFE_PATH)
    strcpy (tmp_filename, "/tmp/gnat-XXXXXX");
  else
    sprintf (tmp_filename, "%s/gnat-XXXXXX", tmpdir);

  close (mkstemp(tmp_filename));
#else
  tmpnam (tmp_filename);
#endif
}

/* Read the next entry in a directory.  The returned string points somewhere
   in the buffer.  */

char *
__gnat_readdir (DIR *dirp, char *buffer)
{
  /* If possible, try to use the thread-safe version.  */
#ifdef HAVE_READDIR_R
  if (readdir_r (dirp, buffer) != NULL)
    return ((struct dirent*) buffer)->d_name;
  else
    return NULL;

#else
  struct dirent *dirent = (struct dirent *) readdir (dirp);

  if (dirent != NULL)
    {
      strcpy (buffer, dirent->d_name);
      return buffer;
    }
  else
    return NULL;

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

#ifdef _WIN32
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
    return (time_t) (t_write.ull_time / 10000000ULL
		     - w32_epoch_offset);
  return (time_t) 0;
}
#endif

/* Return a GNAT time stamp given a file name.  */

OS_Time
__gnat_file_time_name (char *name)
{

#if defined (__EMX__) || defined (MSDOS)
  int fd = open (name, O_RDONLY | O_BINARY);
  time_t ret = __gnat_file_time_fd (fd);
  close (fd);
  return (OS_Time)ret;

#elif defined (_WIN32)
  time_t ret = 0;
  HANDLE h = CreateFile (name, GENERIC_READ, FILE_SHARE_READ, 0,
			 OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

  if (h != INVALID_HANDLE_VALUE)
    {
      ret = win32_filetime (h);
      CloseHandle (h);
    }
  return (OS_Time) ret;
#else
  struct stat statbuf;
  if (__gnat_stat (name, &statbuf) != 0) {
     return (OS_Time)-1;
  } else {
#ifdef VMS
     /* VMS has file versioning.  */
     return (OS_Time)statbuf.st_ctime;
#else
     return (OS_Time)statbuf.st_mtime;
#endif
  }
#endif
}

/* Return a GNAT time stamp given a file descriptor.  */

OS_Time
__gnat_file_time_fd (int fd)
{
  /* The following workaround code is due to the fact that under EMX and
     DJGPP fstat attempts to convert time values to GMT rather than keep the
     actual OS timestamp of the file. By using the OS2/DOS functions directly
     the GNAT timestamp are independent of this behavior, which is desired to
     facilitate the distribution of GNAT compiled libraries.  */

#if defined (__EMX__) || defined (MSDOS)
#ifdef __EMX__

  FILESTATUS fs;
  int ret = DosQueryFileInfo (fd, 1, (unsigned char *) &fs,
                                sizeof (FILESTATUS));

  unsigned file_year  = fs.fdateLastWrite.year;
  unsigned file_month = fs.fdateLastWrite.month;
  unsigned file_day   = fs.fdateLastWrite.day;
  unsigned file_hour  = fs.ftimeLastWrite.hours;
  unsigned file_min   = fs.ftimeLastWrite.minutes;
  unsigned file_tsec  = fs.ftimeLastWrite.twosecs;

#else
  struct ftime fs;
  int ret = getftime (fd, &fs);

  unsigned file_year  = fs.ft_year;
  unsigned file_month = fs.ft_month;
  unsigned file_day   = fs.ft_day;
  unsigned file_hour  = fs.ft_hour;
  unsigned file_min   = fs.ft_min;
  unsigned file_tsec  = fs.ft_tsec;
#endif

  /* Calculate the seconds since epoch from the time components. First count
     the whole days passed.  The value for years returned by the DOS and OS2
     functions count years from 1980, so to compensate for the UNIX epoch which
     begins in 1970 start with 10 years worth of days and add days for each
     four year period since then.  */

  time_t tot_secs;
  int cum_days[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  int days_passed = 3652 + (file_year / 4) * 1461;
  int years_since_leap = file_year % 4;

  if (years_since_leap == 1)
    days_passed += 366;
  else if (years_since_leap == 2)
    days_passed += 731;
  else if (years_since_leap == 3)
    days_passed += 1096;

  if (file_year > 20)
    days_passed -= 1;

  days_passed += cum_days[file_month - 1];
  if (years_since_leap == 0 && file_year != 20 && file_month > 2)
    days_passed++;

  days_passed += file_day - 1;

  /* OK - have whole days.  Multiply -- then add in other parts.  */

  tot_secs  = days_passed * 86400;
  tot_secs += file_hour * 3600;
  tot_secs += file_min * 60;
  tot_secs += file_tsec * 2;
  return (OS_Time) tot_secs;

#elif defined (_WIN32)
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  time_t ret = win32_filetime (h);
  return (OS_Time) ret;

#else
  struct stat statbuf;

  if (fstat (fd, &statbuf) != 0) {
     return (OS_Time) -1;
  } else {
#ifdef VMS
     /* VMS has file versioning.  */
     return (OS_Time) statbuf.st_ctime;
#else
     return (OS_Time) statbuf.st_mtime;
#endif
  }
#endif
}

/* Set the file time stamp.  */

void
__gnat_set_file_time_name (char *name, time_t time_stamp)
{
#if defined (__EMX__) || defined (MSDOS) || defined (__vxworks)

/* Code to implement __gnat_set_file_time_name for these systems.  */

#elif defined (_WIN32)
  union
  {
    FILETIME ft_time;
    unsigned long long ull_time;
  } t_write;

  HANDLE h  = CreateFile (name, GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
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

#elif defined (VMS)
  struct FAB fab;
  struct NAM nam;

  struct
    {
      unsigned long long backup, create, expire, revise;
      unsigned long uic;
      union
	{
	  unsigned short value;
	  struct
	    {
	      unsigned system : 4;
	      unsigned owner  : 4;
	      unsigned group  : 4;
	      unsigned world  : 4;
	    } bits;
	} prot;
    } Fat = { 0, 0, 0, 0, 0, { 0 }};

  ATRDEF atrlst[]
    = {
      { ATR$S_CREDATE,  ATR$C_CREDATE,  &Fat.create },
      { ATR$S_REVDATE,  ATR$C_REVDATE,  &Fat.revise },
      { ATR$S_EXPDATE,  ATR$C_EXPDATE,  &Fat.expire },
      { ATR$S_BAKDATE,  ATR$C_BAKDATE,  &Fat.backup },
      { ATR$S_FPRO,     ATR$C_FPRO,     &Fat.prot },
      { ATR$S_UIC,      ATR$C_UIC,      &Fat.uic },
      { 0, 0, 0}
    };

  FIBDEF fib;
  struct dsc$descriptor_fib fibdsc = {sizeof (fib), (void *) &fib};

  struct IOSB iosb;

  unsigned long long newtime;
  unsigned long long revtime;
  long status;
  short chan;

  struct vstring file;
  struct dsc$descriptor_s filedsc
    = {NAM$C_MAXRSS, DSC$K_DTYPE_T, DSC$K_CLASS_S, (void *) file.string};
  struct vstring device;
  struct dsc$descriptor_s devicedsc
    = {NAM$C_MAXRSS, DSC$K_DTYPE_T, DSC$K_CLASS_S, (void *) device.string};
  struct vstring timev;
  struct dsc$descriptor_s timedsc
    = {NAM$C_MAXRSS, DSC$K_DTYPE_T, DSC$K_CLASS_S, (void *) timev.string};
  struct vstring result;
  struct dsc$descriptor_s resultdsc
    = {NAM$C_MAXRSS, DSC$K_DTYPE_VT, DSC$K_CLASS_VS, (void *) result.string};

  tryfile = (char *) __gnat_to_host_dir_spec (name, 0);

  /* Allocate and initialize a FAB and NAM structures.  */
  fab = cc$rms_fab;
  nam = cc$rms_nam;

  nam.nam$l_esa = file.string;
  nam.nam$b_ess = NAM$C_MAXRSS;
  nam.nam$l_rsa = result.string;
  nam.nam$b_rss = NAM$C_MAXRSS;
  fab.fab$l_fna = tryfile;
  fab.fab$b_fns = strlen (tryfile);
  fab.fab$l_nam = &nam;

  /* Validate filespec syntax and device existence.  */
  status = SYS$PARSE (&fab, 0, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);

  file.string[nam.nam$b_esl] = 0;

  /* Find matching filespec.  */
  status = SYS$SEARCH (&fab, 0, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);

  file.string[nam.nam$b_esl] = 0;
  result.string[result.length=nam.nam$b_rsl] = 0;

  /* Get the device name and assign an IO channel.  */
  strncpy (device.string, nam.nam$l_dev, nam.nam$b_dev);
  devicedsc.dsc$w_length  = nam.nam$b_dev;
  chan = 0;
  status = SYS$ASSIGN (&devicedsc, &chan, 0, 0, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);

  /* Initialize the FIB and fill in the directory id field.  */
  memset (&fib, 0, sizeof (fib));
  fib.fib$w_did[0]  = nam.nam$w_did[0];
  fib.fib$w_did[1]  = nam.nam$w_did[1];
  fib.fib$w_did[2]  = nam.nam$w_did[2];
  fib.fib$l_acctl = 0;
  fib.fib$l_wcc = 0;
  strcpy (file.string, (strrchr (result.string, ']') + 1));
  filedsc.dsc$w_length = strlen (file.string);
  result.string[result.length = 0] = 0;

  /* Open and close the file to fill in the attributes.  */
  status
    = SYS$QIOW (0, chan, IO$_ACCESS|IO$M_ACCESS, &iosb, 0, 0,
		&fibdsc, &filedsc, &result.length, &resultdsc, &atrlst, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);
  if ((iosb.status & 1) != 1)
    LIB$SIGNAL (iosb.status);

  result.string[result.length] = 0;
  status = SYS$QIOW (0, chan, IO$_DEACCESS, &iosb, 0, 0, &fibdsc, 0, 0, 0,
		     &atrlst, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);
  if ((iosb.status & 1) != 1)
    LIB$SIGNAL (iosb.status);

  {
    time_t t;

    /* Set creation time to requested time.  */
    unix_time_to_vms (time_stamp, newtime);

    t = time ((time_t) 0);

    /* Set revision time to now in local time.  */
    unix_time_to_vms (t, revtime);
  }

  /* Reopen the file, modify the times and then close.  */
  fib.fib$l_acctl = FIB$M_WRITE;
  status
    = SYS$QIOW (0, chan, IO$_ACCESS|IO$M_ACCESS, &iosb, 0, 0,
		&fibdsc, &filedsc, &result.length, &resultdsc, &atrlst, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);
  if ((iosb.status & 1) != 1)
    LIB$SIGNAL (iosb.status);

  Fat.create = newtime;
  Fat.revise = revtime;

  status = SYS$QIOW (0, chan, IO$_DEACCESS, &iosb, 0, 0,
                     &fibdsc, 0, 0, 0, &atrlst, 0);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);
  if ((iosb.status & 1) != 1)
    LIB$SIGNAL (iosb.status);

  /* Deassign the channel and exit.  */
  status = SYS$DASSGN (chan);
  if ((status & 1) != 1)
    LIB$SIGNAL (status);
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

void
__gnat_get_env_value_ptr (char *name, int *len, char **value)
{
  *value = getenv (name);
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* VMS specific declarations for set_env_value.  */

#ifdef VMS

static char *to_host_path_spec (char *);

struct descriptor_s
{
  unsigned short len, mbz;
  __char_ptr32 adr;
};

typedef struct _ile3
{
  unsigned short len, code;
  __char_ptr32 adr;
  unsigned short *retlen_adr;
} ile_s;

#endif

void
__gnat_set_env_value (char *name, char *value)
{
#ifdef MSDOS

#elif defined (VMS)
  struct descriptor_s name_desc;
  /* Put in JOB table for now, so that the project stuff at least works.  */
  struct descriptor_s table_desc = {7, 0, "LNM$JOB"};
  char *host_pathspec = value;
  char *copy_pathspec;
  int num_dirs_in_pathspec = 1;
  char *ptr;
  long status;

  name_desc.len = strlen (name);
  name_desc.mbz = 0;
  name_desc.adr = name;

  if (*host_pathspec == 0)
    /* deassign */
    {
      status = LIB$DELETE_LOGICAL (&name_desc, &table_desc);
      /* no need to check status; if the logical name is not
         defined, that's fine. */
      return;
    }

  ptr = host_pathspec;
  while (*ptr++)
    if (*ptr == ',')
      num_dirs_in_pathspec++;

  {
    int i, status;
    ile_s *ile_array = alloca (sizeof (ile_s) * (num_dirs_in_pathspec + 1));
    char *copy_pathspec = alloca (strlen (host_pathspec) + 1);
    char *curr, *next;

    strcpy (copy_pathspec, host_pathspec);
    curr = copy_pathspec;
    for (i = 0; i < num_dirs_in_pathspec; i++)
      {
	next = strchr (curr, ',');
	if (next == 0)
	  next = strchr (curr, 0);

	*next = 0;
	ile_array[i].len = strlen (curr);

	/* Code 2 from lnmdef.h means it's a string.  */
	ile_array[i].code = 2;
	ile_array[i].adr = curr;

	/* retlen_adr is ignored.  */
	ile_array[i].retlen_adr = 0;
	curr = next + 1;
      }

    /* Terminating item must be zero.  */
    ile_array[i].len = 0;
    ile_array[i].code = 0;
    ile_array[i].adr = 0;
    ile_array[i].retlen_adr = 0;

    status = LIB$SET_LOGICAL (&name_desc, 0, &table_desc, 0, ile_array);
    if ((status & 1) != 1)
      LIB$SIGNAL (status);
  }

#elif defined (__vxworks) && defined (__RTP__)
  setenv (name, value, 1);

#else
  int size = strlen (name) + strlen (value) + 2;
  char *expression;

  expression = (char *) xmalloc (size * sizeof (char));

  sprintf (expression, "%s=%s", name, value);
  putenv (expression);
#endif
}

#ifdef _WIN32
#include <windows.h>
#endif

/* Get the list of installed standard libraries from the
   HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries
   key.  */

char *
__gnat_get_libraries_from_registry (void)
{
  char *result = (char *) "";

#if defined (_WIN32) && ! defined (__vxworks) && ! defined (CROSS_COMPILE)

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
      res = RegEnumValue (reg_key, index, name, &name_size, 0,
                          &type, (LPBYTE)value, &value_size);

      if (res == ERROR_SUCCESS && type == REG_SZ)
        {
          char *old_result = result;

          result = (char *) xmalloc (strlen (old_result) + value_size + 2);
          strcpy (result, old_result);
          strcat (result, value);
          strcat (result, ";");
        }
    }

  /* Remove the trailing ";".  */
  if (result[0] != 0)
    result[strlen (result) - 1] = 0;

#endif
  return result;
}

int
__gnat_stat (char *name, struct stat *statbuf)
{
#ifdef _WIN32
  /* Under Windows the directory name for the stat function must not be
     terminated by a directory separator except if just after a drive name.  */
  int name_len  = strlen (name);
  char last_char = name[name_len - 1];
  char win32_name[GNAT_MAX_PATH_LEN + 2];

  if (name_len > GNAT_MAX_PATH_LEN)
    return -1;

  strcpy (win32_name, name);

  while (name_len > 1 && (last_char == '\\' || last_char == '/'))
    {
      win32_name[name_len - 1] = '\0';
      name_len--;
      last_char = win32_name[name_len - 1];
    }

  if (name_len == 2 && win32_name[1] == ':')
    strcat (win32_name, "\\");

  return stat (win32_name, statbuf);

#else
  return stat (name, statbuf);
#endif
}

int
__gnat_file_exists (char *name)
{
  struct stat statbuf;

  return !__gnat_stat (name, &statbuf);
}

int
__gnat_is_absolute_path (char *name, int length)
{
  return (length != 0) &&
     (*name == '/' || *name == DIR_SEPARATOR
#if defined (__EMX__) || defined (MSDOS) || defined (WINNT)
      || (length > 1 && isalpha (name[0]) && name[1] == ':')
#endif
	  );
}

int
__gnat_is_regular_file (char *name)
{
  int ret;
  struct stat statbuf;

  ret = __gnat_stat (name, &statbuf);
  return (!ret && S_ISREG (statbuf.st_mode));
}

int
__gnat_is_directory (char *name)
{
  int ret;
  struct stat statbuf;

  ret = __gnat_stat (name, &statbuf);
  return (!ret && S_ISDIR (statbuf.st_mode));
}

int
__gnat_is_readable_file (char *name)
{
  int ret;
  int mode;
  struct stat statbuf;

  ret = __gnat_stat (name, &statbuf);
  mode = statbuf.st_mode & S_IRUSR;
  return (!ret && mode);
}

int
__gnat_is_writable_file (char *name)
{
  int ret;
  int mode;
  struct stat statbuf;

  ret = __gnat_stat (name, &statbuf);
  mode = statbuf.st_mode & S_IWUSR;
  return (!ret && mode);
}

void
__gnat_set_writable (char *name)
{
#ifndef __vxworks
  struct stat statbuf;

  if (stat (name, &statbuf) == 0)
  {
    statbuf.st_mode = statbuf.st_mode | S_IWUSR;
    chmod (name, statbuf.st_mode);
  }
#endif
}

void
__gnat_set_executable (char *name)
{
#ifndef __vxworks
  struct stat statbuf;

  if (stat (name, &statbuf) == 0)
  {
    statbuf.st_mode = statbuf.st_mode | S_IXUSR;
    chmod (name, statbuf.st_mode);
  }
#endif
}

void
__gnat_set_readonly (char *name)
{
#ifndef __vxworks
  struct stat statbuf;

  if (stat (name, &statbuf) == 0)
  {
    statbuf.st_mode = statbuf.st_mode & 07577;
    chmod (name, statbuf.st_mode);
  }
#endif
}

int
__gnat_is_symbolic_link (char *name ATTRIBUTE_UNUSED)
{
#if defined (__vxworks)
  return 0;

#elif defined (_AIX) || defined (__APPLE__) || defined (__unix__)
  int ret;
  struct stat statbuf;

  ret = lstat (name, &statbuf);
  return (!ret && S_ISLNK (statbuf.st_mode));

#else
  return 0;
#endif
}

#if defined (sun) && defined (__SVR4)
/* Using fork on Solaris will duplicate all the threads. fork1, which
   duplicates only the active thread, must be used instead, or spawning
   subprocess from a program with tasking will lead into numerous problems.  */
#define fork fork1
#endif

int
__gnat_portable_spawn (char *args[])
{
  int status = 0;
  int finished ATTRIBUTE_UNUSED;
  int pid ATTRIBUTE_UNUSED;

#if defined (MSDOS) || defined (_WIN32)
  /* args[0] must be quotes as it could contain a full pathname with spaces */
  char *args_0 = args[0];
  args[0] = (char *)xmalloc (strlen (args_0) + 3);
  strcpy (args[0], "\"");
  strcat (args[0], args_0);
  strcat (args[0], "\"");

  status = spawnvp (P_WAIT, args_0, (const char* const*)args);

  /* restore previous value */
  free (args[0]);
  args[0] = (char *)args_0;

  if (status < 0)
    return -1;
  else
    return status;

#elif defined (__vxworks)
  return -1;
#else

#ifdef __EMX__
  pid = spawnvp (P_NOWAIT, args[0], args);
  if (pid == -1)
    return -1;

#else
  pid = fork ();
  if (pid < 0)
    return -1;

  if (pid == 0)
    {
      /* The child. */
      if (execv (args[0], MAYBE_TO_PTR32 (args)) != 0)
#if defined (VMS)
	return -1; /* execv is in parent context on VMS.  */
#else
	_exit (1);
#endif
    }
#endif

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
__gnat_dup2 (int oldfd, int newfd)
{
#if defined (__vxworks) && !defined (__RTP__)
  /* Not supported on VxWorks 5.x, but supported on VxWorks 6.0 when using
     RTPs.  */
  return -1;
#else
  return dup2 (oldfd, newfd);
#endif
}

/* WIN32 code to implement a wait call that wait for any child process.  */

#ifdef _WIN32

/* Synchronization code, to be thread safe.  */

static CRITICAL_SECTION plist_cs;

void
__gnat_plist_init (void)
{
  InitializeCriticalSection (&plist_cs);
}

static void
plist_enter (void)
{
  EnterCriticalSection (&plist_cs);
}

static void
plist_leave (void)
{
  LeaveCriticalSection (&plist_cs);
}

typedef struct _process_list
{
  HANDLE h;
  struct _process_list *next;
} Process_List;

static Process_List *PLIST = NULL;

static int plist_length = 0;

static void
add_handle (HANDLE h)
{
  Process_List *pl;

  pl = (Process_List *) xmalloc (sizeof (Process_List));

  plist_enter();

  /* -------------------- critical section -------------------- */
  pl->h = h;
  pl->next = PLIST;
  PLIST = pl;
  ++plist_length;
  /* -------------------- critical section -------------------- */

  plist_leave();
}

static void
remove_handle (HANDLE h)
{
  Process_List *pl;
  Process_List *prev = NULL;

  plist_enter();

  /* -------------------- critical section -------------------- */
  pl = PLIST;
  while (pl)
    {
      if (pl->h == h)
        {
          if (pl == PLIST)
	    PLIST = pl->next;
          else
	    prev->next = pl->next;
          free (pl);
          break;
        }
      else
        {
          prev = pl;
          pl = pl->next;
        }
    }

  --plist_length;
  /* -------------------- critical section -------------------- */

  plist_leave();
}

static int
win32_no_block_spawn (char *command, char *args[])
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

  result = CreateProcess
	     (NULL, (char *) full_command, &SA, NULL, TRUE,
              GetPriorityClass (GetCurrentProcess()), NULL, NULL, &SI, &PI);

  free (full_command);

  if (result == TRUE)
    {
      add_handle (PI.hProcess);
      CloseHandle (PI.hThread);
      return (int) PI.hProcess;
    }
  else
    return -1;
}

static int
win32_wait (int *status)
{
  DWORD exitcode;
  HANDLE *hl;
  HANDLE h;
  DWORD res;
  int k;
  Process_List *pl;

  if (plist_length == 0)
    {
      errno = ECHILD;
      return -1;
    }

  hl = (HANDLE *) xmalloc (sizeof (HANDLE) * plist_length);

  k = 0;
  plist_enter();

  /* -------------------- critical section -------------------- */
  pl = PLIST;
  while (pl)
    {
      hl[k++] = pl->h;
      pl = pl->next;
    }
  /* -------------------- critical section -------------------- */

  plist_leave();

  res = WaitForMultipleObjects (plist_length, hl, FALSE, INFINITE);
  h = hl[res - WAIT_OBJECT_0];
  free (hl);

  remove_handle (h);

  GetExitCodeProcess (h, &exitcode);
  CloseHandle (h);

  *status = (int) exitcode;
  return (int) h;
}

#endif

int
__gnat_portable_no_block_spawn (char *args[])
{
  int pid = 0;

#if defined (__EMX__) || defined (MSDOS)

  /* ??? For PC machines I (Franco) don't know the system calls to implement
     this routine. So I'll fake it as follows. This routine will behave
     exactly like the blocking portable_spawn and will systematically return
     a pid of 0 unless the spawned task did not complete successfully, in
     which case we return a pid of -1.  To synchronize with this the
     portable_wait below systematically returns a pid of 0 and reports that
     the subprocess terminated successfully. */

  if (spawnvp (P_WAIT, args[0], args) != 0)
    return -1;

#elif defined (_WIN32)

  pid = win32_no_block_spawn (args[0], args);
  return pid;

#elif defined (__vxworks)
  return -1;

#else
  pid = fork ();

  if (pid == 0)
    {
      /* The child.  */
      if (execv (args[0], MAYBE_TO_PTR32 (args)) != 0)
#if defined (VMS)
	return -1; /* execv is in parent context on VMS. */
#else
	_exit (1);
#endif
    }

#endif

  return pid;
}

int
__gnat_portable_wait (int *process_status)
{
  int status = 0;
  int pid = 0;

#if defined (_WIN32)

  pid = win32_wait (&status);

#elif defined (__EMX__) || defined (MSDOS)
  /* ??? See corresponding comment in portable_no_block_spawn.  */

#elif defined (__vxworks)
  /* Not sure what to do here, so do same as __EMX__ case, i.e., nothing but
     return zero.  */
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

/* Locate a regular file, give a Path value.  */

char *
__gnat_locate_regular_file (char *file_name, char *path_val)
{
  char *ptr;
  char *file_path = alloca (strlen (file_name) + 1);
  int absolute;

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
     if (__gnat_is_regular_file (file_path))
       return xstrdup (file_path);

      return 0;
    }

  /* If file_name include directory separator(s), try it first as
     a path name relative to the current directory */
  for (ptr = file_name; *ptr && *ptr != '/' && *ptr != DIR_SEPARATOR; ptr++)
    ;

  if (*ptr != 0)
    {
      if (__gnat_is_regular_file (file_name))
        return xstrdup (file_name);
    }

  if (path_val == 0)
    return 0;

  {
    /* The result has to be smaller than path_val + file_name.  */
    char *file_path = alloca (strlen (path_val) + strlen (file_name) + 2);

    for (;;)
      {
        for (; *path_val == PATH_SEPARATOR; path_val++)
          ;

      if (*path_val == 0)
        return 0;

      /* Skip the starting quote */

      if (*path_val == '"')
	path_val++;

      for (ptr = file_path; *path_val && *path_val != PATH_SEPARATOR; )
	*ptr++ = *path_val++;

      ptr--;

      /* Skip the ending quote */

      if (*ptr == '"')
	ptr--;

      if (*ptr != '/' && *ptr != DIR_SEPARATOR)
        *++ptr = DIR_SEPARATOR;

      strcpy (++ptr, file_name);

      if (__gnat_is_regular_file (file_path))
        return xstrdup (file_path);
      }
  }

  return 0;
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
      char *full_exec_name
        = alloca (strlen (exec_name) + strlen (HOST_EXECUTABLE_SUFFIX) + 1);

      strcpy (full_exec_name, exec_name);
      strcat (full_exec_name, HOST_EXECUTABLE_SUFFIX);
      ptr = __gnat_locate_regular_file (full_exec_name, path_val);

      if (ptr == 0)
         return __gnat_locate_regular_file (exec_name, path_val);
      return ptr;
    }
  else
    return __gnat_locate_regular_file (exec_name, path_val);
}

/* Locate an executable using the Systems default PATH.  */

char *
__gnat_locate_exec_on_path (char *exec_name)
{
  char *apath_val;
#ifdef VMS
  char *path_val = "/VAXC$PATH";
#else
  char *path_val = getenv ("PATH");
#endif
#ifdef _WIN32
  /* In Win32 systems we expand the PATH as for XP environment
     variables are not automatically expanded. We also prepend the
     ".;" to the path to match normal NT path search semantics */

  #define EXPAND_BUFFER_SIZE 32767

  apath_val = alloca (EXPAND_BUFFER_SIZE);

  apath_val [0] = '.';
  apath_val [1] = ';';

  DWORD res = ExpandEnvironmentStrings
    (path_val, apath_val + 2, EXPAND_BUFFER_SIZE - 2);

  if (!res) apath_val [0] = '\0';
#else
  apath_val = alloca (strlen (path_val) + 1);
  strcpy (apath_val, path_val);
#endif

  return __gnat_locate_exec (exec_name, apath_val);
}

#ifdef VMS

/* These functions are used to translate to and from VMS and Unix syntax
   file, directory and path specifications.  */

#define MAXPATH  256
#define MAXNAMES 256
#define NEW_CANONICAL_FILELIST_INCREMENT 64

static char new_canonical_dirspec [MAXPATH];
static char new_canonical_filespec [MAXPATH];
static char new_canonical_pathspec [MAXNAMES*MAXPATH];
static unsigned new_canonical_filelist_index;
static unsigned new_canonical_filelist_in_use;
static unsigned new_canonical_filelist_allocated;
static char **new_canonical_filelist;
static char new_host_pathspec [MAXNAMES*MAXPATH];
static char new_host_dirspec [MAXPATH];
static char new_host_filespec [MAXPATH];

/* Routine is called repeatedly by decc$from_vms via
   __gnat_to_canonical_file_list_init until it returns 0 or the expansion
   runs out. */

static int
wildcard_translate_unix (char *name)
{
  char *ver;
  char buff [MAXPATH];

  strncpy (buff, name, MAXPATH);
  buff [MAXPATH - 1] = (char) 0;
  ver = strrchr (buff, '.');

  /* Chop off the version.  */
  if (ver)
    *ver = 0;

  /* Dynamically extend the allocation by the increment.  */
  if (new_canonical_filelist_in_use == new_canonical_filelist_allocated)
    {
      new_canonical_filelist_allocated += NEW_CANONICAL_FILELIST_INCREMENT;
      new_canonical_filelist = (char **) xrealloc
	(new_canonical_filelist,
	 new_canonical_filelist_allocated * sizeof (char *));
    }

  new_canonical_filelist[new_canonical_filelist_in_use++] = xstrdup (buff);

  return 1;
}

/* Translate a wildcard VMS file spec into a list of Unix file specs. First do
   full translation and copy the results into a list (_init), then return them
   one at a time (_next). If onlydirs set, only expand directory files.  */

int
__gnat_to_canonical_file_list_init (char *filespec, int onlydirs)
{
  int len;
  char buff [MAXPATH];

  len = strlen (filespec);
  strncpy (buff, filespec, MAXPATH);

  /* Only look for directories */
  if (onlydirs && !strstr (&buff [len-5], "*.dir"))
    strncat (buff, "*.dir", MAXPATH);

  buff [MAXPATH - 1] = (char) 0;

  decc$from_vms (buff, wildcard_translate_unix, 1);

  /* Remove the .dir extension.  */
  if (onlydirs)
    {
      int i;
      char *ext;

      for (i = 0; i < new_canonical_filelist_in_use; i++)
	{
	  ext = strstr (new_canonical_filelist[i], ".dir");
	  if (ext)
	    *ext = 0;
	}
    }

  return new_canonical_filelist_in_use;
}

/* Return the next filespec in the list.  */

char *
__gnat_to_canonical_file_list_next ()
{
  return new_canonical_filelist[new_canonical_filelist_index++];
}

/* Free storage used in the wildcard expansion.  */

void
__gnat_to_canonical_file_list_free ()
{
  int i;

   for (i = 0; i < new_canonical_filelist_in_use; i++)
     free (new_canonical_filelist[i]);

  free (new_canonical_filelist);

  new_canonical_filelist_in_use = 0;
  new_canonical_filelist_allocated = 0;
  new_canonical_filelist_index = 0;
  new_canonical_filelist = 0;
}

/* Translate a VMS syntax directory specification in to Unix syntax.  If
   PREFIXFLAG is set, append an underscore "/". If no indicators of VMS syntax
   found, return input string. Also translate a dirname that contains no
   slashes, in case it's a logical name.  */

char *
__gnat_to_canonical_dir_spec (char *dirspec, int prefixflag)
{
  int len;

  strcpy (new_canonical_dirspec, "");
  if (strlen (dirspec))
    {
      char *dirspec1;

      if (strchr (dirspec, ']') || strchr (dirspec, ':'))
	{
	  strncpy (new_canonical_dirspec,
		   (char *) decc$translate_vms (dirspec),
		   MAXPATH);
	}
      else if (!strchr (dirspec, '/') && (dirspec1 = getenv (dirspec)) != 0)
	{
	  strncpy (new_canonical_dirspec,
		  (char *) decc$translate_vms (dirspec1),
		  MAXPATH);
	}
      else
	{
	  strncpy (new_canonical_dirspec, dirspec, MAXPATH);
	}
    }

  len = strlen (new_canonical_dirspec);
  if (prefixflag && new_canonical_dirspec [len-1] != '/')
    strncat (new_canonical_dirspec, "/", MAXPATH);

  new_canonical_dirspec [MAXPATH - 1] = (char) 0;

  return new_canonical_dirspec;

}

/* Translate a VMS syntax file specification into Unix syntax.
   If no indicators of VMS syntax found, check if it's an uppercase
   alphanumeric_ name and if so try it out as an environment
   variable (logical name). If all else fails return the
   input string.  */

char *
__gnat_to_canonical_file_spec (char *filespec)
{
  char *filespec1;

  strncpy (new_canonical_filespec, "", MAXPATH);

  if (strchr (filespec, ']') || strchr (filespec, ':'))
    {
      char *tspec = (char *) decc$translate_vms (filespec);

      if (tspec != (char *) -1)
	strncpy (new_canonical_filespec, tspec, MAXPATH);
    }
  else if ((strlen (filespec) == strspn (filespec,
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"))
	&& (filespec1 = getenv (filespec)))
    {
      char *tspec = (char *) decc$translate_vms (filespec1);

      if (tspec != (char *) -1)
	strncpy (new_canonical_filespec, tspec, MAXPATH);
    }
  else
    {
      strncpy (new_canonical_filespec, filespec, MAXPATH);
    }

  new_canonical_filespec [MAXPATH - 1] = (char) 0;

  return new_canonical_filespec;
}

/* Translate a VMS syntax path specification into Unix syntax.
   If no indicators of VMS syntax found, return input string.  */

char *
__gnat_to_canonical_path_spec (char *pathspec)
{
  char *curr, *next, buff [MAXPATH];

  if (pathspec == 0)
    return pathspec;

  /* If there are /'s, assume it's a Unix path spec and return.  */
  if (strchr (pathspec, '/'))
    return pathspec;

  new_canonical_pathspec[0] = 0;
  curr = pathspec;

  for (;;)
    {
      next = strchr (curr, ',');
      if (next == 0)
        next = strchr (curr, 0);

      strncpy (buff, curr, next - curr);
      buff[next - curr] = 0;

      /* Check for wildcards and expand if present.  */
      if (strchr (buff, '*') || strchr (buff, '%') || strstr (buff, "..."))
        {
          int i, dirs;

          dirs = __gnat_to_canonical_file_list_init (buff, 1);
          for (i = 0; i < dirs; i++)
            {
              char *next_dir;

              next_dir = __gnat_to_canonical_file_list_next ();
              strncat (new_canonical_pathspec, next_dir, MAXPATH);

              /* Don't append the separator after the last expansion.  */
              if (i+1 < dirs)
                strncat (new_canonical_pathspec, ":", MAXPATH);
            }

	  __gnat_to_canonical_file_list_free ();
        }
      else
	strncat (new_canonical_pathspec,
		__gnat_to_canonical_dir_spec (buff, 0), MAXPATH);

      if (*next == 0)
        break;

      strncat (new_canonical_pathspec, ":", MAXPATH);
      curr = next + 1;
    }

  new_canonical_pathspec [MAXPATH - 1] = (char) 0;

  return new_canonical_pathspec;
}

static char filename_buff [MAXPATH];

static int
translate_unix (char *name, int type)
{
  strncpy (filename_buff, name, MAXPATH);
  filename_buff [MAXPATH - 1] = (char) 0;
  return 0;
}

/* Translate a Unix syntax path spec into a VMS style (comma separated list of
   directories.  */

static char *
to_host_path_spec (char *pathspec)
{
  char *curr, *next, buff [MAXPATH];

  if (pathspec == 0)
    return pathspec;

  /* Can't very well test for colons, since that's the Unix separator!  */
  if (strchr (pathspec, ']') || strchr (pathspec, ','))
    return pathspec;

  new_host_pathspec[0] = 0;
  curr = pathspec;

  for (;;)
    {
      next = strchr (curr, ':');
      if (next == 0)
        next = strchr (curr, 0);

      strncpy (buff, curr, next - curr);
      buff[next - curr] = 0;

      strncat (new_host_pathspec, __gnat_to_host_dir_spec (buff, 0), MAXPATH);
      if (*next == 0)
        break;
      strncat (new_host_pathspec, ",", MAXPATH);
      curr = next + 1;
    }

  new_host_pathspec [MAXPATH - 1] = (char) 0;

  return new_host_pathspec;
}

/* Translate a Unix syntax directory specification into VMS syntax.  The
   PREFIXFLAG has no effect, but is kept for symmetry with
   to_canonical_dir_spec.  If indicators of VMS syntax found, return input
   string. */

char *
__gnat_to_host_dir_spec (char *dirspec, int prefixflag ATTRIBUTE_UNUSED)
{
  int len = strlen (dirspec);

  strncpy (new_host_dirspec, dirspec, MAXPATH);
  new_host_dirspec [MAXPATH - 1] = (char) 0;

  if (strchr (new_host_dirspec, ']') || strchr (new_host_dirspec, ':'))
    return new_host_dirspec;

  while (len > 1 && new_host_dirspec[len - 1] == '/')
    {
      new_host_dirspec[len - 1] = 0;
      len--;
    }

  decc$to_vms (new_host_dirspec, translate_unix, 1, 2);
  strncpy (new_host_dirspec, filename_buff, MAXPATH);
  new_host_dirspec [MAXPATH - 1] = (char) 0;

  return new_host_dirspec;
}

/* Translate a Unix syntax file specification into VMS syntax.
   If indicators of VMS syntax found, return input string.  */

char *
__gnat_to_host_file_spec (char *filespec)
{
  strncpy (new_host_filespec, "", MAXPATH);
  if (strchr (filespec, ']') || strchr (filespec, ':'))
    {
      strncpy (new_host_filespec, filespec, MAXPATH);
    }
  else
    {
      decc$to_vms (filespec, translate_unix, 1, 1);
      strncpy (new_host_filespec, filename_buff, MAXPATH);
    }

  new_host_filespec [MAXPATH - 1] = (char) 0;

  return new_host_filespec;
}

void
__gnat_adjust_os_resource_limits ()
{
  SYS$ADJWSL (131072, 0);
}

#else /* VMS */

/* Dummy functions for Osint import for non-VMS systems.  */

int
__gnat_to_canonical_file_list_init
  (char *dirspec ATTRIBUTE_UNUSED, int onlydirs ATTRIBUTE_UNUSED)
{
  return 0;
}

char *
__gnat_to_canonical_file_list_next (void)
{
  return (char *) "";
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

#endif

/* For EMX, we cannot include dummy in libgcc, since it is too difficult
   to coordinate this with the EMX distribution. Consequently, we put the
   definition of dummy which is used for exception handling, here.  */

#if defined (__EMX__)
void __dummy () {}
#endif

#if defined (__mips_vxworks)
int
_flush_cache()
{
   CACHE_USER_FLUSH (0, ENTIRE_CACHE);
}
#endif

#if defined (CROSS_COMPILE)  \
  || (! (defined (sparc) && defined (sun) && defined (__SVR4)) \
      && ! (defined (linux) && (defined (i386) || defined (__x86_64__))) \
      && ! defined (__FreeBSD__) \
      && ! defined (__hpux__) \
      && ! defined (__APPLE__) \
      && ! defined (_AIX) \
      && ! (defined (__alpha__)  && defined (__osf__)) \
      && ! defined (__MINGW32__) \
      && ! (defined (__mips) && defined (__sgi)))

/* Dummy function to satisfy g-trasym.o.  Currently Solaris sparc, HP/UX,
   GNU/Linux x86{_64}, Tru64 & Windows provide a non-dummy version of this
   procedure in libaddr2line.a.  */

void
convert_addresses (void *addrs ATTRIBUTE_UNUSED,
		   int n_addr ATTRIBUTE_UNUSED,
		   void *buf ATTRIBUTE_UNUSED,
		   int *len ATTRIBUTE_UNUSED)
{
  *len = 0;
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

   Returns 0 if operation was successful and -1 in case of error. */

int
__gnat_copy_attribs (char *from, char *to, int mode)
{
#if defined (VMS) || defined (__vxworks)
  return -1;
#else
  struct stat fbuf;
  struct utimbuf tbuf;

  if (stat (from, &fbuf) == -1)
    {
      return -1;
    }

  tbuf.actime = fbuf.st_atime;
  tbuf.modtime = fbuf.st_mtime;

  if (utime (to, &tbuf) == -1)
    {
      return -1;
    }

  if (mode == 1)
    {
      if (chmod (to, fbuf.st_mode) == -1)
	{
	  return -1;
	}
    }

  return 0;
#endif
}

/* This function is installed in libgcc.a.  */
extern void __gnat_install_locks (void (*) (void), void (*) (void));

/* This function offers a hook for libgnarl to set the
   locking subprograms for libgcc_eh.
   This is only needed on OpenVMS, since other platforms use standard
   --enable-threads=posix option, or similar.  */

void
__gnatlib_install_locks (void (*lock) (void) ATTRIBUTE_UNUSED,
                         void (*unlock) (void) ATTRIBUTE_UNUSED)
{
#if defined (IN_RTS) && defined (VMS)
  __gnat_install_locks (lock, unlock);
  /* There is a bootstrap path issue if adaint is build with this
     symbol unresolved for the stage1 compiler. Since the compiler
     does not use tasking, we simply make __gnatlib_install_locks
     a no-op in this case. */
#endif
}

int
__gnat_lseek (int fd, long offset, int whence)
{
  return (int) lseek (fd, offset, whence);
}

/* This function returns the version of GCC being used.  Here it's GCC 3.  */
int
get_gcc_version (void)
{
  return 3;
}

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
  return fcntl (fd, F_SETFD, flags | FD_CLOEXEC);
#else
  return -1;
  /* For the Windows case, we should use SetHandleInformation to remove
     the HANDLE_INHERIT property from fd. This is not implemented yet,
     but for our purposes (support of GNAT.Expect) this does not matter,
     as by default handles are *not* inherited. */
#endif
}

/* Indicates if platforms supports automatic initialization through the
   constructor mechanism */
int
__gnat_binder_supports_auto_init ()
{
#ifdef VMS
   return 0;
#else
   return 1;
#endif
}

/* Indicates that Stand-Alone Libraries are automatically initialized through
   the constructor mechanism */
int
__gnat_sals_init_using_constructors ()
{
#if defined (__vxworks) || defined (__Lynx__) || defined (VMS)
   return 0;
#else
   return 1;
#endif
}
