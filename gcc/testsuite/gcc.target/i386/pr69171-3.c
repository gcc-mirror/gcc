/* { dg-do compile { target fpic } } */
/* { dg-options "-fpic -O2 -msse2 -mno-avx -mfpmath=sse" } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

extern int glob1;

double
foo (void)
{
  return (double) (int) (uintptr_t) &glob1;
}
