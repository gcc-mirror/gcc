/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mtune=skylake -mavx2" } */

#include <immintrin.h>

char z[64];

int i;

__attribute__((noipa))
int
do_test (void)
{
  register int xmm15 __asm ("xmm15") = i;
  asm volatile ("" : "+v" (xmm15));
  __builtin_memset (&z, 0, sizeof (z));
  register int xmm2 __asm ("xmm2") = xmm15;
  asm volatile ("" : "+v" (xmm2));
  return xmm2;
}

__attribute__((target("arch=x86-64")))
int
main (void)
{
 if (__builtin_cpu_supports ("avx2"))
   {
     i = 4;
     if (do_test () != 4)
       __builtin_abort ();
   }
  return 0;
}
