/* Test whether using target specific options, we can generate SSE2 code on
   32-bit, which does not generate SSE2 by default, but still generate 387 code
   for a function that doesn't use attribute((option)).  */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O3 -ftree-vectorize -march=i386" } */
/* { dg-final { scan-assembler "addps\[ \t\]" } } */
/* { dg-final { scan-assembler "fsubs\[ \t\]" } } */

#ifndef SIZE
#define SIZE 1024
#endif

static float a[SIZE] __attribute__((__aligned__(16)));
static float b[SIZE] __attribute__((__aligned__(16)));
static float c[SIZE] __attribute__((__aligned__(16)));

void sse_addnums (void) __attribute__ ((__target__ ("sse2")));

void
sse_addnums (void)
{
  int i = 0;
  for (; i < SIZE; ++i)
    a[i] = b[i] + c[i];
}

void
i387_subnums (void)
{
  int i = 0;
  for (; i < SIZE; ++i)
    a[i] = b[i] - c[i];
}
