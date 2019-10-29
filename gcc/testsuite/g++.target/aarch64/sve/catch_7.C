/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O" } */

#include <arm_sve.h>

void __attribute__ ((noipa))
f1 (void)
{
  throw 1;
}

void __attribute__ ((noipa))
f2 (svbool_t)
{
  register svint8_t z8 asm ("z8") = svindex_s8 (11, 1);
  asm volatile ("" :: "w" (z8));
  f1 ();
}

void __attribute__ ((noipa))
f3 (int n)
{
  register double d8 asm ("v8") = 42.0;
  for (int i = 0; i < n; ++i)
    {
      asm volatile ("" : "=w" (d8) : "w" (d8));
      try { f2 (svptrue_b8 ()); } catch (int) { break; }
    }
  if (d8 != 42.0)
    __builtin_abort ();
}

int
main (void)
{
  f3 (100);
  return 0;
}
