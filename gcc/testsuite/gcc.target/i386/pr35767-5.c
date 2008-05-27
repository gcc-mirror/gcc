/* Test that we generate aligned load when memory is aligned.  */
/* { dg-do compile } */
/* { dg-options "-O -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler-not "movups" } } */
/* { dg-final { scan-assembler "movaps" } } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

extern void foo(v4sf, v4sf, v4sf, v4sf, v4sf, v4sf, v4sf, v4sf, v4sf);

int test(void)
{
  v4sf x = { 0.0, 1.0, 2.0, 3.0 };

  foo (x, x, x, x, x, x, x, x, x);
  return 0;
}
