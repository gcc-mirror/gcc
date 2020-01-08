/* PR target/90867 */
/* { dg-do run { target lp64 } } */
/* { dg-options "-O2 -msse2" } */

unsigned long long freq = 3600000000UL;   /* 3.6 GHz = 3600.0 MHz */

__attribute__((noipa)) void
bar (double x)
{
  static double d = 3600000000.0;
  if (x != d)
    __builtin_abort ();
  d /= 1000.0;
}

__attribute__ ((target ("arch=x86-64"))) int
foo ()
{
  bar ((double) freq);
  bar (1e-3 * freq);
  bar (1e-6 * freq);
  bar (1e-9 * freq);
  return 0;
}

int
main ()
{
  return foo ();
}
