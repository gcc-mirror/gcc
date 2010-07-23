/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-w -msse2 -Os" } */

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

extern void foo (__m128 x, __m128 y ,__m128 z ,__m128 a, int size);

void
bar (void)
{
  __m128 x = { 1.0 };
  foo (x, x, x, x, 5);
}

/* { dg-final { scan-assembler-not "movups" { xfail { ! *-*-darwin* } } } } */
