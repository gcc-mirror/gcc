/* PR target/47312 */
/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-require-effective-target xop } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O -flto -mno-sse3 -mxop" } */

extern double fma (double, double, double);
extern float fmaf (float, float, float);
extern long double fmal (long double, long double, long double);

volatile float f;
volatile double d;
volatile long double ld;

int
main ()
{
  f = fmaf (f, f, f);
  d = fma (d, d, d);
  ld = fmal (ld, ld, ld);
  __asm__ volatile ("" : : "r" (&f), "r" (&d), "r" (&ld) : "memory");
  return 0;
}
