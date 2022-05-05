/* PR target/104674 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */

__attribute__((noipa)) double
bar (double x, double y)
{
  return x + y;
}

__attribute__((noipa)) double
foo (long long x)
{
  long long a = x / 10000000;
  int b = x % 10000000;
  double s = (double) a;
  double n = (double) b / 1e7;
  double t = s + n;
  if (t == s + 1.0)
    t = bar (t, s);
  return t;
}

int
main ()
{
  long long n = 888888;
  n = n * 10000000;
  if (foo (n) != 888888.0)
    __builtin_abort ();
}
