/* Configuration for GNU C-compiler for hosting on Windows NT.
   using a unix style C library.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#define EXECUTABLE_SUFFIX ".exe"
#define NO_SYS_SIGLIST 1
#define HAVE_BCOPY 1
#define HAVE_BZERO 1
#define HAVE_BCMP 1
#define HAVE_RINDEX 1
#define HAVE_INDEX 1

/* Even though we support "/", allow "\" since everybody tests both.  */
#define DIR_SEPARATOR '\\'

/* If we allow both '/' and '\' as dir separators, then
   allow both unix and win32 PATH syntax */
#undef GET_ENVIRONMENT
#define GET_ENVIRONMENT(ENV_VALUE,ENV_NAME)                   \
{                                                             \
  char *epath;                                                \
  char *win32epath;                                           \
                                                              \
  epath = win32epath = getenv (ENV_NAME);                     \
  /* if we have a posix path list, convert to win32 path list */ \
  if (epath != NULL && *epath != 0 && cygwin32_posix_path_list_p (epath)) \
    {                                                         \
      win32epath = (char *) xmalloc                           \
      (cygwin32_posix_to_win32_path_list_buf_size (epath));   \
      cygwin32_posix_to_win32_path_list (epath, win32epath);  \
    }                                                         \
   ENV_VALUE = win32epath;                                    \
}

#define PATH_SEPARATOR ';'

/* This is needed so that protoize will compile.  */
#define POSIX
