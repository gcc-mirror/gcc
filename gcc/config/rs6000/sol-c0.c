/* Solaris PowerPC startfile.  */
/* Copyright (C) 1996, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern char **_environ;

extern int atexit (void (*__func) (void));
extern void __init (void) __attribute__ ((__longcall__));
extern void __fini (void) __attribute__ ((__longcall__));
extern void _start(int argc, char *argv[], char *envp[], void *auxp, 
		   void (*termfunc)(void));
extern void exit(int);
extern int main (int argc, char *argv[], char *envp[], void *auxp);

typedef void (*func_ptr) (void);
int (*__atexit)(func_ptr) = atexit;

/* Exception handling */
struct ex_shared1 {
  void	*prev;
  void	*next;
  char	*text_start;
  char	*range_start;
  char	*text_end;
  char	*range_end;
};

struct ex_shared {
  void (*ex_register) (struct ex_shared1 *);
  void (*ex_deregister) (struct ex_shared1 *);
  struct ex_shared1 shared_info;
};

extern char _ex_text0[], _ex_text1[];
extern char _ex_range0[], _ex_range1[];
extern void _ex_register (struct ex_shared1 *);
extern void _ex_deregister (struct ex_shared1 *);
extern char _SDA_BASE_[];
extern char _SDA2_BASE_[];

struct ex_shared shared __attribute__((section(".ex_shared"))) = {
  _ex_register,
  _ex_deregister,
  {
    (void *)0,
    (void *)0,
    _ex_text0,
    _ex_range0,
    _ex_text1,
    _ex_range1
  }
};

static void
deregister (void)
{
  (* shared.ex_deregister) (&shared.shared_info);
}

/* Start function.  */
void
_start(int argc, char *argv[], char *envp[], void *auxp, 
       void (*termfunc)(void))
{
  int ret;
  int dummy = 0;

#if 0
  /* Disable this for now, it causes an impossible reload.  */
  /* Load up r13/r2 before we do anything else.  */
  __asm__ volatile ("mr %%r13,%0;mr %%r2,%1" : "=r" (dummy) : "r" (&_SDA_BASE_[0]), "r" (&_SDA2_BASE_[0]), "r" (dummy));
#endif

  _environ = envp + dummy;

  /* Register loader termination function (the || dummy is to make sure the above asm
     is not optimized away).  */
  if (termfunc)
    atexit (termfunc);

  /* Register exception handler if needed */
  if (shared.ex_register)
    (* shared.ex_register) (&shared.shared_info);

  if (shared.ex_deregister)
    atexit (deregister);

  /* Call any global constructors and destructors.  */
  __init ();

  atexit (__fini);

  /* Call the main program now */
  ret = main (argc, argv, envp, auxp);

  /* Return to the os */
  exit (ret);
}
