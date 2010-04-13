/* { dg-do compile } */
/* { dg-options "-O0" } */
extern int (isinfl)(long double);

int
bugfun(long double x, long double y)
{
  int result;

  if (isinfl(x))
    result = isinfl(y);
  else
    {
      int kx, ky;
      kx = ky = 1;
      result = (kx == ky);
    }
  return (result);
}
