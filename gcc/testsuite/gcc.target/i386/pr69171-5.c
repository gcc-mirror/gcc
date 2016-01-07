/* { dg-do compile { target fpic } } */
/* { dg-options "-fpic -O2 -msse2 -mno-avx -mfpmath=sse" } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

extern int glob1;

float
foo (void)
{
  return (float) (long long) (uintptr_t) &glob1;
}
