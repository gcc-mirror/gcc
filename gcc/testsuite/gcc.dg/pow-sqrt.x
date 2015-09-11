
extern void abort (void);


__attribute__((noinline)) double
real_pow (double x, double pow_exp)
{
  return __builtin_pow (x, pow_exp);
}

#define EPS (0.000000000000000000001)

#define SYNTH_POW(X, Y) __builtin_pow (X, Y)
volatile double arg;

int
main (void)
{
  double i_arg = 0.1;

  for (arg = i_arg; arg < 100.0; arg += 1.0)
    {
      double synth_res = SYNTH_POW (arg, EXPN);
      double real_res = real_pow (arg, EXPN);

      if (__builtin_abs (SYNTH_POW (arg, EXPN) - real_pow (arg, EXPN)) > EPS)
	abort ();
    }
  return 0;
}
