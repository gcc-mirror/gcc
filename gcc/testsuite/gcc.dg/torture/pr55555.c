/* { dg-do run } */

double s[4] = { 1.0, 2.0, 3.0, 4.0 }, pol_x[2] = { 5.0, 6.0 };

__attribute__((noinline)) int
foo (void)
{
  double coef_x[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int lxp = 0;
  if (lxp <= 1)
    do
      {
	double t = pol_x[lxp];
	long S;
	long l = lxp * 4L - 1;
	for (S = 1; S <= 4; S++)
	  coef_x[S + l] = coef_x[S + l] + s[S - 1] * t;
      }
    while (lxp++ != 1);
  asm volatile ("" : : "r" (coef_x) : "memory");
  for (lxp = 0; lxp < 8; lxp++)
    if (coef_x[lxp] != ((lxp & 3) + 1) * (5.0 + (lxp >= 4)))
      __builtin_abort ();
  return 1;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  if (!foo ())
    __builtin_abort ();
  return 0;
}
