/* Configuration for GNU C-compiler for Intel 80386 running DJGPP.
   Copyright (C) 1988, 1996, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

#define HOST_EXECUTABLE_SUFFIX ".exe"

/* Even though we support "/", allow "\" since everybody tests both.  */
#define DIR_SEPARATOR '/'
#define DIR_SEPARATOR_2 '\\'

/* Allow test for DOS drive names.  */
#define HAVE_DOS_BASED_FILE_SYSTEM

/* System dependent initialization for collect2
   to tell system() to act like Unix.  */
#define COLLECT2_HOST_INITIALIZATION \
  do { __system_flags |= (__system_allow_multiple_cmds			\
		          | __system_emulate_chdir); } while (0)

/* Define a version appropriate for DOS.  */
#undef XREF_FILE_NAME
#define XREF_FILE_NAME(xref_file, file) \
  do { \
    const char xref_ext[] = ".gxref"; \
    strcpy (xref_file, file); \
    s = basename (xref_file); \
    t = strchr (s, '.'); \
    if (t) \
      strcpy (t, xref_ext); \
    else \
      strcat (xref_file, xref_ext); \
  } while (0)

/* Change /dev/env/DJDIR/prefix/dir/ to canonical form so gcc_exec_prefix
   is set properly in 'gcc.c'. It also helps to cut down the number of times
   the value of the DJGPP environment variable 'DJDIR' is evaluated.  */
#undef GCC_DRIVER_HOST_INITIALIZATION
#define GCC_DRIVER_HOST_INITIALIZATION \
  do { \
    /* If the environment variable DJDIR is not defined, then DJGPP is not \
       installed correctly and GCC will quickly become confused with the \
       default prefix settings. Report the problem now so the user doesn't \
       receive deceptive "file not found" error messages later.  */ \
    char *djdir = getenv ("DJDIR"); \
    if (djdir == NULL) \
      { \
        /* DJDIR is automatically defined by the DJGPP environment config \
           file pointed to by the environment variable DJGPP. Examine DJGPP \
           to try and figure out what's wrong.  */ \
        char *djgpp = getenv ("DJGPP"); \
        if (djgpp == NULL) \
          fatal ("environment variable DJGPP not defined"); \
        else if (access (djgpp, R_OK) == 0) \
          fatal ("environment variable DJGPP points to missing file '%s'", \
                 djgpp); \
        else \
          fatal ("environment variable DJGPP points to corrupt file '%s'", \
                  djgpp); \
      } \
    standard_exec_prefix = update_path (standard_exec_prefix, NULL); \
    standard_bindir_prefix = update_path (standard_bindir_prefix, NULL); \
    standard_startfile_prefix = update_path (standard_startfile_prefix, NULL); \
  } while (0)

/* Canonicalize paths containing '/dev/env/'; used in prefix.c.
   _fixpath is a djgpp-specific function to canonicalize a path.
   "/dev/env/DJDIR" evaluates to "c:/djgpp" if DJDIR is "c:/djgpp" for
   example.  It removes any trailing '/', so add it back.  */
/* We cannot free PATH below as it can point to string constant  */
#define UPDATE_PATH_HOST_CANONICALIZE(PATH) \
  if (memcmp ((PATH), "/dev/env/", sizeof("/dev/env/") - 1) == 0) \
    {						\
      static char fixed_path[FILENAME_MAX + 1];	\
						\
      _fixpath ((PATH), fixed_path);		\
      strcat (fixed_path, "/");			\
      (PATH) = xstrdup (fixed_path);		\
    } 
