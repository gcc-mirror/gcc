/* PR tree-optimization/82004 */
/* { dg-do run } */
/* { dg-options "-Ofast" } */

extern double log10 (double);
extern double pow (double, double);

__attribute__((noipa)) void
bar (double x)
{
  if (x < 0.001)
    __builtin_abort ();
  asm volatile ("" : : : "memory");
}

int
main ()
{
  double d = 0.001;
  double e = 10.0;
  double f = (log10 (e) - log10 (d)) / 400.0;
  double g = log10 (d) - f;
  volatile int q = 0;
  int i;
  if (__builtin_expect (q == 0, 0))
    for (i = 0; i < 400; ++i)
      {
	g = g + f;
	bar (pow (10.0, g));
      }
  return 0;  
}
