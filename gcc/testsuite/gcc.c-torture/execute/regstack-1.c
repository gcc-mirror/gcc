void abort (void);
void exit (int);

long double C = 5;
long double U = 1;
long double Y2 = 11;
long double Y1 = 17;
long double X, Y, Z, T, R, S;
int
main (void)
{
  X = (C + U) * Y2;
  Y = C - U - U;
  Z = C + U + U;
  T = (C - U) * Y1;
  X = X - (Z + U);
  R = Y * Y1;
  S = Z * Y2;
  T = T - Y;
  Y = (U - Y) + R;
  Z = S - (Z + U + U);
  R = (Y2 + U) * Y1;
  Y1 = Y2 * Y1;
  R = R - Y2;
  Y1 = Y1 - 0.5L;
  if (Z != 68. || Y != 49. || X != 58. || Y1 != 186.5 || R != 193. || S != 77.
      || T != 65. || Y2 != 11.)
    abort ();
  exit (0);
}
