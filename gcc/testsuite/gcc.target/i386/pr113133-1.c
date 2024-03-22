/* PR target/113133 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mtune=barcelona" } */

void
foo1 (double *d, float f)
{
  register float x __asm ("xmm16") = f;
  asm volatile ("" : "+v" (x));

  *d = x;
}

void
foo2 (float *f, double d)
{
  register double x __asm ("xmm16") = d;
  asm volatile ("" : "+v" (x));

  *f = x;
}
