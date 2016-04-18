/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

__m128d
foo (double d)
{
  return __extension__ (__m128d){ d, 0.0 };
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-not "movsd\[ \\t\]+\[^\n\]*%xmm" } } */
/* { dg-final { scan-assembler-not "\\(%\[er\]sp\\)" { target { ! ia32 } }} } */
