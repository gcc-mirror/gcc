/* Test that we generate aligned load when memory is aligned.  */
/* { dg-do compile } */
/* { dg-options "-O -mavx -mtune=generic" } */
/* { dg-final { scan-assembler-not "movups" } } */
/* { dg-final { scan-assembler "movaps" } } */

typedef float v8sf __attribute__ ((__vector_size__ (32)));

extern void foo (v8sf, v8sf, v8sf, v8sf, v8sf, v8sf, v8sf, v8sf, v8sf);

int
test (void)
{
  v8sf x = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };

  foo (x, x, x, x, x, x, x, x, x);
  return 0;
}
