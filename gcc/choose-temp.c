/* Utility to pick a temporary filename prefix.
   Copyright (C) 1996 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file exports one function: choose_temp_base.  */

/* This file lives in at least two places: libiberty and gcc.
   Don't change one without the other.  */

#ifndef NO_SYS_FILE_H
#include <sys/types.h>
#include <sys/file.h>   /* May get R_OK, etc. on some systems.  */
#endif

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#include <stdio.h>	/* May get P_tmpdir.  */

#ifdef IN_GCC
#include "config.h"
#include "gansidecl.h"
extern char *xmalloc ();
#else
#include "ansidecl.h"
#include "libiberty.h"
#if defined (__MSDOS__) || defined (_WIN32)
#define DIR_SEPARATOR '\\'
#endif
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* On MSDOS, write temp files in current dir
   because there's no place else we can expect to use.  */
/* ??? Although the current directory is tried as a last resort,
   this is left in so that on MSDOS it is prefered to /tmp on the
   off chance that someone requires this, since that was the previous
   behaviour.  */
#ifdef __MSDOS__
#ifndef P_tmpdir
#define P_tmpdir "."
#endif
#endif

/* Name of temporary file.
   mktemp requires 6 trailing X's.  */
#define TEMP_FILE "ccXXXXXX"

/* Subroutine of choose_temp_base.
   If BASE is non-NULL, returh it.
   Otherwise it checks if DIR is a usable directory.
   If success, DIR is returned.
   Otherwise NULL is returned.  */

static char *
try (dir, base)
     char *dir, *base;
{
  if (base != 0)
    return base;
  if (dir != 0
      && access (dir, R_OK | W_OK | X_OK) == 0)
    return dir;
  return 0;
}

/* Return a prefix for temporary file names or NULL if unable to find one.
   The current directory is chosen if all else fails so the program is
   exited if a temporary directory can't be found (mktemp fails).
   The buffer for the result is obtained with xmalloc.  */

char *
choose_temp_base ()
{
  char *base = 0;
  char *temp_filename;
  int len;
  static char tmp[] = { DIR_SEPARATOR, 't', 'm', 'p', 0 };
  static char usrtmp[] = { DIR_SEPARATOR, 'u', 's', 'r', DIR_SEPARATOR, 't', 'm', 'p', 0 };

#ifndef MPW
  base = try (getenv ("TMPDIR"), base);
  base = try (getenv ("TMP"), base);
  base = try (getenv ("TEMP"), base);

#ifdef P_tmpdir
  base = try (P_tmpdir, base);
#endif

  /* Try /usr/tmp, then /tmp.  */
  base = try (usrtmp, base);
  base = try (tmp, base);
 
  /* If all else fails, use the current directory!  */
  if (base == 0)
#ifdef VMS
    base = "[";
#else
    base = ".";
#endif

#else /* MPW */
  base = ":";
#endif

  len = strlen (base);
  temp_filename = xmalloc (len + 1 /*DIR_SEPARATOR*/
			   + strlen (TEMP_FILE) + 1);
  strcpy (temp_filename, base);

#ifndef MPW
  if (len != 0
      && temp_filename[len-1] != '/'
      && temp_filename[len-1] != DIR_SEPARATOR)
    temp_filename[len++] = DIR_SEPARATOR;
#else /* MPW */
  if (temp_filename[len-1] != ':')
    temp_filename[len++] = ':';
#endif /* MPW */
  strcpy (temp_filename + len, TEMP_FILE);

  mktemp (temp_filename);
  if (strlen (temp_filename) == 0)
    abort ();
  return temp_filename;
}
