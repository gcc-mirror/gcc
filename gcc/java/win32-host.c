/* Platform-Specific Win32 Functions
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Mohan Embar <gnustuff@thisiscool.com>, March 2003. */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "jcf.h"

#ifdef WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

/* Simulate an open() failure with ENOENT */
static int
file_not_found (void);

static int
file_not_found (void)
{
  errno = ENOENT;
  return -1;
}

int
jcf_open_exact_case (const char *filename, int oflag)
{
  int filename_len = strlen (filename);
  int found_file_len;
  HANDLE found_file_handle;
  WIN32_FIND_DATA fd;
  
  /* See if we can find this file. */
  found_file_handle = FindFirstFile (filename, &fd);
  if (found_file_handle == INVALID_HANDLE_VALUE)
    return file_not_found ();
  FindClose (found_file_handle);

  found_file_len = strlen (fd.cFileName);
  
  /* This should never happen. */
  if (found_file_len > filename_len)
    return file_not_found ();
  
  /* Here, we're only actually comparing the filename and not
     checking the case of any containing directory components.
     Although we're not fully obeying our contract, checking
     all directory components would be tedious and time-consuming
     and it's a pretty safe assumption that mixed-case package
     names are a fringe case.... */
  if (strcmp (filename + filename_len - found_file_len, fd.cFileName))
    {
      /* Reject this because it is not a perfect-case match. */
      /* printf("************\nRejected:\n%s\n%s\n************\n\n", filename, fd.cFileName); */
      return file_not_found ();
    }
  else
    {
      return open (filename, oflag);
    }
}

#endif /* WIN32 */
