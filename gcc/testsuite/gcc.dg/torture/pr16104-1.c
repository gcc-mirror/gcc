/* PR rtl-optimization/16104 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */

#include "../i386-cpuid.h"

extern void abort (void);

typedef int V2SI __attribute__ ((vector_size (8)));
typedef unsigned int V2USI __attribute__ ((vector_size (8)));
typedef short V2HI __attribute__ ((vector_size (4)));
typedef unsigned int V2UHI __attribute__ ((vector_size (4)));

int
test1 (void)
{
  return (long long) (V2SI) 0LL;
}

int
test2 (V2SI x)
{
  return (long long) x;
}

V2SI
test3 (void)
{
  return (V2SI) (long long) (int) (V2HI) 0;
}

V2SI
test4 (V2HI x)
{
  return (V2SI) (long long) (int) x;
}

V2SI
test5 (V2USI x)
{
  return (V2SI) x;
}

int
__attribute__ ((noinline))
do_test (void)
{
  if (sizeof (short) != 2 || sizeof (int) != 4 || sizeof (long long) != 8)
    return 0;

  if (test1 () != 0)
    abort ();

  V2SI x = { 2, 2 };
  if (test2 (x) != 2)
    abort ();

  union { V2SI x; int y[2]; V2USI z; long long l; } u;
  u.x = test3 ();
  if (u.y[0] != 0 || u.y[1] != 0)
    abort ();

  V2HI y = { 4, 4 };
  union { V2SI x; long long y; } v;
  v.x = test4 (y);
  if (v.y != 0x40004)
    abort ();

  V2USI z = { 6, 6 };
  u.x = test5 (z);
  if (u.y[0] != 6 || u.y[1] != 6)
    abort ();
  return 0;
}

int
main (void)
{
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_CMOV | bit_SSE2))
      != (bit_MMX | bit_SSE | bit_CMOV | bit_SSE2))
    return 0;

  return do_test ();
}
