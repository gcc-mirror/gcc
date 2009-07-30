/* VMS 64bit crt0 returning VMS style condition codes .
   Copyright (C) 2001, 2009 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (rupp@gnat.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if !defined(__DECC)
You Lose! This file can only be compiled with DEC C.
#else

/* This file can only be compiled with DEC C, due to the call to
   lib$establish and the pragmas pointer_size.  */

#pragma __pointer_size short

#include <stdlib.h>
#include <string.h>
#include <ssdef.h>

extern void decc$main ();

extern int main ();

static int
handler (sigargs, mechargs)
     void *sigargs;
     void *mechargs;
{
  return SS$_RESIGNAL;
}

int
__main (arg1, arg2, arg3, image_file_desc, arg5, arg6)
     void *arg1, *arg2, *arg3;
     void *image_file_desc;
     void *arg5, *arg6;
{
  int argc;
  char **argv;
  char **envp;

#pragma __pointer_size long

  int i;
  char **long_argv;
  char **long_envp;

#pragma __pointer_size short

  lib$establish (handler);
  decc$main (arg1, arg2, arg3, image_file_desc,
	     arg5, arg6, &argc, &argv, &envp);

#pragma __pointer_size long

  /* Reallocate argv with 64 bit pointers.  */
  long_argv = (char **) _malloc32 (sizeof (char *) * (argc + 1));

  for (i = 0; i < argc; i++)
    long_argv[i] = (char *) _strdup32 (argv[i]);

  long_argv[argc] = (char *) 0;

  for (i = 0; envp[i]; i++);
  long_envp = (char **) _malloc32 (sizeof (char *) * (i + 1));

  for (i = 0; envp[i]; i++)
    long_envp[i] = (char *) _strdup32 (envp[i]);

  long_envp[i] = (char *) 0;

#pragma __pointer_size short

  return main (argc, long_argv, long_envp);
}
#endif
