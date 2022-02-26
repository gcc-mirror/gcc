/* { dg-do run { target ia32 } } */
/* { dg-options "-O2 -march=i686 -mtune=skylake -msse2" } */

#include <immintrin.h>

char z[16];

int i;

__attribute__((noipa))
int
do_test (void)
{
  register int xmm7 __asm ("xmm7") = i;
  asm volatile ("" : "+v" (xmm7));
  __builtin_memset (&z, 0, sizeof (z));
  register int xmm2 __asm ("xmm2") = xmm7;
  asm volatile ("" : "+v" (xmm2));
  return xmm2;
}

__attribute__((target("arch=i486")))
int
main (void)
{
 if (__builtin_cpu_supports ("sse2"))
   {
     i = 4;
     if (do_test () != 4)
       __builtin_abort ();
   }
  return 0;
}
