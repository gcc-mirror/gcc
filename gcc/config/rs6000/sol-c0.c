/* Solaris PowerPC startfile.  */
/* Copyright (C) 1996 Free Software Foundation, Inc.

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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

extern char **_environ;

extern int atexit (void (*__func) (void));
extern void __init (void);
extern void __fini (void);
extern void __do_global_ctors (void);

typedef void (*func_ptr) (void);
int (*__atexit)(func_ptr) = atexit;

/* Start function.  */

void
_start(int argc, char *argv[], char *envp[], void *auxp, void (*termfunc)())
{
  int ret;

  _environ = envp;

  /* Register loader termination function */
  if (termfunc)
    atexit (termfunc);

  /* Call any global constructors and destructors.  */
  __do_global_ctors ();

  /* Call the main program now */
  ret = main (argc, argv, envp, auxp);

  /* Return to the os */
  exit (ret);
}

/* Provide a dummy __eabi in case main got compiled without -mcall-solaris.  */
void
__eabi ()
{
}
