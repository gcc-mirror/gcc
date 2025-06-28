/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-omit-frame-pointer -mtune-ctrl=prologue_using_move,epilogue_using_move,use_leave" } */

#ifndef DONT_SAVE_REGS1
# define DONT_SAVE_REGS1 __attribute__((no_callee_saved_registers))
#endif
#ifndef DONT_SAVE_REGS2
# define DONT_SAVE_REGS2 __attribute__((no_callee_saved_registers))
#endif

/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-linux*" } {^\t?\.}  } } */

/*
**do_test:
**.LFB[0-9]+:
**...
**	leave
**...
**	ret
**...
*/

#include <stdlib.h>

DONT_SAVE_REGS1
__attribute__ ((weak, __optimize__ ("-fomit-frame-pointer")))
void
continuation (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  /* Clobber frame pointer register.  */
  asm ("xor %%ebp, %%ebp" ::: "ebp");

  if (arg1 != 17)
    abort ();
  if (arg2 != 8)
    abort ();
  if (arg3 != 20)
    abort ();
  if (arg4 != -3)
    abort ();
  if (arg5 != -4)
    abort ();
  if (arg6 != 26)
    abort ();
}

DONT_SAVE_REGS2
__attribute__ ((weak, __optimize__ ("-fomit-frame-pointer")))
void
entry (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  /* Clobber frame pointer register.  */
  asm ("xor %%ebp, %%ebp" ::: "ebp");

  if (arg1 != 17)
    abort ();
  if (arg2 != 8)
    abort ();
  if (arg3 != 20)
    abort ();
  if (arg4 != -3)
    abort ();
  if (arg5 != -4)
    abort ();
  if (arg6 != 26)
    abort ();
  continuation (arg1, arg2, arg3, arg4, arg5, arg6);
}

__attribute__ ((weak))
void
do_test (void)
{
  entry (17, 8, 20, -3, -4, 26);
}

int
main (void)
{
  do_test ();
  return 0;
}
