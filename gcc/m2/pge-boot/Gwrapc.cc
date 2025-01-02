/* Gwrapc.c wrap libc functions for mc.

Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
#include "ansidecl.h"

#include "gm2-libs-host.h"

#ifdef __cplusplus
extern "C" {
#endif

/* strtime returns the address of a string which describes the
   local time.  */

char *
wrapc_strtime (void)
{
#if defined(HAVE_CTIME)
  time_t clock = time ((time_t *)0);
  char *string = ctime (&clock);

  string[24] = (char)0;

  return string;
#else
  return "";
#endif
}

int
wrapc_filesize (int f, unsigned int *low, unsigned int *high)
{
  struct stat s;
  int res = fstat (f, (struct stat *)&s);

  if (res == 0)
    {
      *low = (unsigned int)s.st_size;
      *high = (unsigned int)(s.st_size >> (sizeof (unsigned int) * 8));
    }
  return res;
}

/* filemtime returns the mtime of a file, f.  */

int
wrapc_filemtime (int f)
{
  struct stat s;

  if (fstat (f, (struct stat *)&s) == 0)
    return s.st_mtime;
  else
    return -1;
}

/* getrand returns a random number between 0..n-1 */

int
wrapc_getrand (int n)
{
  return rand () % n;
}

#if defined(HAVE_PWD_H)
#include <pwd.h>

char *
wrapc_getusername (void)
{
  return getpwuid (getuid ())->pw_gecos;
}

/* getnameuidgid fills in the, uid, and, gid, which represents
   user, name.  */

void
wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  struct passwd *p = getpwnam (name);

  if (p == NULL)
    {
      *uid = -1;
      *gid = -1;
    }
  else
    {
      *uid = p->pw_uid;
      *gid = p->pw_gid;
    }
}
#else
char *
wrapc_getusername (void)
{
  return "unknown";
}

void
wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  *uid = -1;
  *gid = -1;
}
#endif

int
wrapc_signbit (double r)
{
#if defined(HAVE_SIGNBIT)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbit (r);
#else
  return 0;
#endif
}

int
wrapc_signbitl (long double r)
{
#if defined(HAVE_SIGNBITL)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbitl (r);
#else
  return 0;
#endif
}

int
wrapc_signbitf (float r)
{
#if defined(HAVE_SIGNBITF)

  /* signbit is a macro which tests its argument against sizeof(float),
     sizeof(double).  */
  return signbitf (r);
#else
  return 0;
#endif
}

/* SeekSet return the system libc SEEK_SET value.  */

int
wrapc_SeekSet (void)
{
  return SEEK_SET;
}

/* SeekEnd return the system libc SEEK_END value.  */

int
wrapc_SeekEnd (void)
{
  return SEEK_END;
}

/* ReadOnly return the system value of O_RDONLY.  */

int
wrapc_ReadOnly (void)
{
  return O_RDONLY;
}

/* WriteOnly return the system value of O_WRONLY.  */

int
wrapc_WriteOnly (void)
{
  return O_WRONLY;
}

/* init constructor for the module.  */

void
_M2_wrapc_init ()
{
}

/* finish deconstructor for the module.  */

void
_M2_wrapc_finish ()
{
}

#ifdef __cplusplus
}
#endif
