/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr" } */
/* { dg-final { scan-assembler-times "clui" "1" } } */
/* { dg-final { scan-assembler-times "stui" "2" } } */
/* { dg-final { scan-assembler-times "senduipi" "1" } } */
/* { dg-final { scan-assembler-times "setc" "1" } } */
/* { dg-final { scan-assembler-times "testui" "1" } } */

#include <x86gprintrin.h>

extern volatile unsigned char c;
extern volatile unsigned long long l;

void
foo (void)
{
  _clui ();
  _stui ();
  _senduipi (l);
  c = _testui ();
}
