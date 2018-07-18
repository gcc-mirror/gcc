/* PR rtl-optimization/83723 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */
/* { dg-additional-options "-mfpmath=sse -msse2" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-fpie" { target pie } } */

int foo (void);
float bar (float);
int *v;

void
baz (void)
{
  float a = bar (0.0);
  bar (a);
  if (v)
    bar (1.0);
  if (a < 1.0)
    a = foo () / a;
}
