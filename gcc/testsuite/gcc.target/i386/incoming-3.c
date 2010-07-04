/* PR middle-end/37009 */
/* { dg-do compile { target { { ! *-*-darwin* } && ilp32 } } } */
/* { dg-options "-w -msse2 -mpreferred-stack-boundary=2" } */
/* { dg-require-effective-target sse2 } */

#include <emmintrin.h>

extern void bar (int *);

int
foo(__m128 y, int size, ...)
{
  int __attribute((aligned(16))) xxx;

  xxx = 2;
  bar (&xxx);
  return size;
}

/* { dg-final { scan-assembler-not "and\[l\]\[ \t\]" } } */
