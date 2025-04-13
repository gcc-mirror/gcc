/* { dg-do run { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -mtune=generic -msse2 -mno-apxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */

#include <stdarg.h>
#include <stdlib.h>

__attribute__ ((preserve_none, weak))
void
continuation (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6,
	      ...)
{
  int a;
  va_list va_arglist;
  va_start (va_arglist, arg6);
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
  a = va_arg (va_arglist, int);
  if (a != 0xdeadbeef)
    abort ();
  va_end (va_arglist);
}

__attribute__ ((no_callee_saved_registers, weak))
void
entry (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
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
  continuation (arg1, arg2, arg3, arg4, arg5, arg6, 0xdeadbeef);
}

int
main (void)
{
  entry (17, 8, 20, -3, -4, 26);
  return 0;
}
