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
extern void __init (void) __attribute__ ((__longcall__));
extern void __fini (void) __attribute__ ((__longcall__));

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
_start(int argc, char *argv[], char *envp[], void *auxp, void (*termfunc)())
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

/* Provide a dummy __eabi in case main got compiled without -mcall-solaris.  */
void
__eabi ()
{
}
