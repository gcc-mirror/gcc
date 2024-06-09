/* Glibc.c provides access to some libc functions.

Copyright (C) 2016-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"

#if defined(__cplusplus)
#define EXTERN extern "C"
#else
#define EXTERN
#endif

#undef BUILD_MC_LIBC_TRACE

#if defined(BUILD_MC_LIBC_TRACE)
static bool initialzed_trace = false;
static bool trace_on = false;

static
void
check_init (void)
{
  if (! initialzed_trace)
    {
      initialzed_trace = true;
      trace_on = ((getenv ("MC_LIBC_TRACE") != NULL));
    }
}
#endif

static
void
tracedb (const char *format, ...)
{
#if defined(BUILD_MC_LIBC_TRACE)
  check_init ();
  if (trace_on)
    {
      va_list arg;
      va_start (arg, format);
      {
	vfprintf (stdout, format, arg);
	fflush (stdout);
      }
      va_end (arg);
    }
#endif
}

static
void
tracedb_open (const char *p, int flags, mode_t mode)
{
#if defined(BUILD_MC_LIBC_TRACE)
  bool item_written = false;
  tracedb ("libc_open (%s, flags = 0x%x = ", p, flags);

  int bits = (flags & O_ACCMODE);
  tracedb ("bits = 0x%x", bits);
  if (bits == O_RDONLY)
    {
      tracedb ("O_RDONLY");
      item_written = true;
    }
  if ((flags & O_WRONLY) != 0)
    {
      if (item_written)
	tracedb (" | ");
      tracedb ("O_WRONLY");
      item_written = true;
    }
  if ((flags & O_RDWR) != 0)
    {
      if (item_written)
	tracedb (" | ");
      tracedb ("O_RDWR");
      item_written = true;
    }
  tracedb (", 0x%x)\n", mode);
#endif
}

static
void
tracedb_result (int result)
{
#if defined(BUILD_MC_LIBC_TRACE)
  tracedb (" result = %d", result);
  if (result == -1)
    tracedb (", errno = %s", strerror (errno));
  tracedb ("\n");
#endif
}

EXTERN
int
libc_read (int fd, void *a, int nbytes)
{
  tracedb ("libc_read (%d, %p, %d)\n", fd, a, nbytes);
  int result = read (fd, a, nbytes);
  tracedb_result (result);
  return result;
}

EXTERN
int
libc_write (int fd, void *a, int nbytes)
{
  tracedb ("libc_write (%d, %p, %d)\n", fd, a, nbytes);
  int result = write (fd, a, nbytes);
  tracedb_result (result);
  return result;
}

EXTERN
int
libc_close (int fd)
{
  tracedb ("libc_close (%d)\n", fd);
  int result = close (fd);
  tracedb_result (result);
  return result;
}

EXTERN
int
libc_exit (int code)
{
  exit (code);
}

EXTERN
void
libc_perror (char *s)
{
  perror (s);
}

EXTERN
int
libc_abort ()
{
  abort ();
}

EXTERN
int
libc_strlen (char *s)
{
  return strlen (s);
}

EXTERN
time_t
libc_time (time_t *buf)
{
  return time (buf);
}

EXTERN
void *
libc_localtime (time_t *epochtime)
{
  return localtime (epochtime);
}

EXTERN
int
libc_printf (char *_format, unsigned int _format_high, ...)
{
  va_list arg;
  int done;
  char format[_format_high + 1];
  unsigned int i = 0;
  unsigned int j = 0;
  char *c;

  do
    {
      c = index (&_format[i], '\\');
      if (c == NULL)
        strcpy (&format[j], &_format[i]);
      else
        {
          memcpy (&format[j], &_format[i], (c - _format) - i);
          i = c - _format;
          j += c - _format;
          if (_format[i + 1] == 'n')
            format[j] = '\n';
          else
            format[j] = _format[i + 1];
          j++;
          i += 2;
        }
    }
  while (c != NULL);

  va_start (arg, _format_high);
  done = vfprintf (stdout, format, arg);
  va_end (arg);
  return done;
}

EXTERN
int
libc_snprintf (char *dest, size_t length, char *_format, unsigned int _format_high, ...)
{
  va_list arg;
  int done;
  char format[_format_high + 1];
  unsigned int i = 0;
  unsigned int j = 0;
  char *c;

  do
    {
      c = index (&_format[i], '\\');
      if (c == NULL)
        strcpy (&format[j], &_format[i]);
      else
        {
          memcpy (&format[j], &_format[i], (c - _format) - i);
          i = c - _format;
          j += c - _format;
          if (_format[i + 1] == 'n')
            format[j] = '\n';
          else
            format[j] = _format[i + 1];
          j++;
          i += 2;
        }
    }
  while (c != NULL);

  va_start (arg, _format_high);
  done = vsnprintf (dest, length, format, arg);
  va_end (arg);
  return done;
}

EXTERN
void *
libc_malloc (unsigned int size)
{
  return malloc (size);
}

EXTERN
void
libc_free (void *p)
{
  free (p);
}

EXTERN
char *
libc_strcpy (char *dest, char *src)
{
  return strcpy (dest, src);
}

EXTERN
char *
libc_strncpy (char *dest, char *src, int n)
{
  return strncpy (dest, src, n);
}

EXTERN
int
libc_unlink (char *p)
{
  return unlink (p);
}

EXTERN
int
libc_system (char *command)
{
  return system (command);
}

EXTERN
void *
libc_memcpy (void *dest, void *src, int n)
{
  return memcpy (dest, src, n);
}

EXTERN
char *
libc_getenv (char *name)
{
  return getenv (name);
}

EXTERN
int
libc_putenv (char *name)
{
  return putenv (name);
}

EXTERN
int
libc_creat (char *p, mode_t mode)
{
  tracedb ("libc_creat (%s, 0x%x)\n", p, mode);
  int result = creat (p, mode);
  tracedb_result (result);
  return result;
}

EXTERN
int
libc_open (char *p, int flags, mode_t mode)
{
  tracedb_open (p, flags, mode);
  int result = open (p, flags, mode);
  tracedb_result (result);
  return result;
}

EXTERN
off_t
libc_lseek (int fd, off_t offset, int whence)
{
  tracedb ("libc_lseek (%s, %p, %d)\n", fd, offset, whence);
  int result = lseek (fd, offset, whence);
  tracedb_result (result);
  return result;
}

EXTERN
void *
libc_realloc (void *ptr, size_t size)
{
  return realloc (ptr, size);
}

EXTERN
void *
libc_memset (void *s, int c, size_t n)
{
  return memset (s, c, n);
}

EXTERN
void *
libc_memmove (void *dest, void *src, size_t n)
{
  return memmove (dest, src, n);
}

EXTERN
int
libc_getpid (void)
{
  return getpid ();
}

EXTERN
unsigned int
libc_sleep (unsigned int s)
{
  return sleep (s);
}

EXTERN
int
libc_atexit (void (*function) (void))
{
  return atexit (function);
}
