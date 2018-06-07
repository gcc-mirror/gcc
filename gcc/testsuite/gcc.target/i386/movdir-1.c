/* { dg-do compile } */
/* { dg-options "-mmovdir64b -mmovdiri -O2" } */
/* { dg-final { scan-assembler-times "movdiri" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movdiri" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movdir64b\[ \\t\]" } } */

#include <x86intrin.h>

unsigned int w;
void *x;
unsigned long long q, *z;

int
main ()
{

   unsigned int array[] = {1, 2, 3, 4, 5};
   unsigned int *ap = &w;

   _directstoreu_u32(x, w);

#ifdef __x86_64__
   _directstoreu_u64(z, q);
#endif

   _movdir64b(ap, array);

return 0;
}

