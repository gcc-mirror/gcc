/* { dg-additional-options "-ffast-math" } */

double _Complex __attribute__((noipa))
foo (double _Complex acc, const double _Complex *x, const double _Complex* y, int N)
{
  for (int c = 0; c < N; ++c)
    acc -= x[c] * y[c];
  return acc;
}

int
main()
{
  static const double _Complex y[] = { 1, 2, };
  static const double _Complex x[] = { 1, 3, };
  double _Complex ref = foo (0, x, y, 2);
  if (__builtin_creal (ref) != -7.)
    __builtin_abort ();
  return 0;
}
