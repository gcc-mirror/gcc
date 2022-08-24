/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx512f" } */

#include <immintrin.h>

char z[128];

int i;

__attribute__((noipa))
int
do_test (void)
{
  register int xmm31 __asm ("xmm31") = i;
  asm volatile ("" : "+v" (xmm31));
  __builtin_memset (&z, 0, sizeof (z));
  register int xmm2 __asm ("xmm2") = xmm31;
  asm volatile ("" : "+v" (xmm2));
  return xmm2;
}

__attribute__((target("arch=x86-64")))
int
main (void)
{
 if (__builtin_cpu_supports ("avx512f"))
   {
     i = 4;
     if (do_test () != 4)
       __builtin_abort ();
   }
  return 0;
}
