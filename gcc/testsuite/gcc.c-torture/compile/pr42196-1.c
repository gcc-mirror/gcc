union U
{
  double d;
  __complex__ int c;
};

double gd;
extern double bar (union U);

double foo (int b, double d, int c1, int c2)
{
  union U u;
  double r;

  if (b)
    {
      u.d = d;
      r = u.d;
    }
  else
    {
      __real__ u.c = c1;
      __imag__ u.c = c2;
      r = bar (u);
    }

  return r;
}
