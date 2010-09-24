/* VMS crt0 returning Unix style condition codes.
   Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.
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

#include <stdlib.h>

/* Lots of cheat to handle 32bits/64bits pointer conversions.
   We use 'long long' for 64 bits pointers and 'int' for 32 bits pointers.  */

extern void decc$main (void *arg1, void *arg2, void *arg3,
                       void *image_file_desc, void *arg5, void *arg6,
                       int *, int *, int *);
extern int main (int, char **, char **);
extern int _malloc32 (int);

#ifdef __ia64__
#define MAIN_ASM_NAME asm ("ELF$TFRADR")
#else
#define MAIN_ASM_NAME
#endif

int __main (void *arg1, void *arg2, void *arg3,
            void *image_file_desc, void *arg5, void *arg6) MAIN_ASM_NAME;

/* From errnodef.h, but we need to emulate the globalval.  */
extern int C$_EXIT1;

/* From stsdef.h  */
#define STS$V_MSG_NO 0x03
#define STS$M_INHIB_MSG 0x10000000

/* From ssdef.h  */
#define SS$_NORMAL 1

int
__main (void *arg1, void *arg2, void *arg3,
        void *image_file_desc, void *arg5, void *arg6)
{
  int argc;
  int argv;
  int envp;
  int status;
  int i;
  long long *long_argv;
  long long *long_envp;

  /* The argv and envp arrays are 32 bits pointers to 32 bits pointers.  */
  decc$main (arg1, arg2, arg3, image_file_desc,
	     arg5, arg6, &argc, &argv, &envp);

  if (sizeof (void *) == 8)
    {
      /* Reallocate argv and envp with 64 bit pointers.  */
      long_argv = (long long *)
        (long long) _malloc32 (sizeof (long long) * (argc + 1));

      for (i = 0; i < argc; i++)
        long_argv[i] = ((int *) (long long) argv)[i];

      long_argv[argc] = 0;

      for (i = 0; ((int *) (long long) envp)[i]; i++)
        ;
      long_envp = (long long *)
        (long long) _malloc32 (sizeof (long long) * (i + 1));

      for (i = 0; ((int *) (long long) envp)[i]; i++)
        long_envp[i] = ((int *) (long long) envp)[i];

      long_envp[i] = 0;
    }
  else
    {
      long_argv = (long long *) argv;
      long_envp = (long long *) envp;
    }
  status = main (argc, (char **)long_argv, (char **)long_envp);

#ifdef CRT0_POSIX_EXIT
  /* Map into a range of 0 - 255.  */
  status = status & 255;

  if (status > 0)
    {
      int save_status = status;

      status = (long) &C$_EXIT1 + ((status - 1) << STS$V_MSG_NO);

      /* An exit failure status requires a "severe" error.  All status values
	 are defined in errno with a successful (1) severity but can be
	 changed to an error (2) severity by adding 1.  In addition for
	 compatibility with UNIX exit() routines we inhibit a run-time error
	 message from being generated on exit(1).  */

      if (save_status == 1)
	{
	  status++;
	  status |= STS$M_INHIB_MSG;
	}
    }
  else
    status = SS$_NORMAL;
#endif /* CRT0_POSIX_EXIT */

  return status;
}
