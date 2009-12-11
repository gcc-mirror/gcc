union U
{
  __complex__ int ci;
  __complex__ float cf;
};

float gd;
extern float bar (union U);

float foo (int b, double f1, double f2, int c1, int c2)
{
  union U u;
  double r;

  if (b)
    {
      __real__ u.cf = f1;
      __imag__ u.cf = f2;
    }
  else
    {
      __real__ u.ci = c1;
      __imag__ u.ci = c2;
    }

  r = bar (u);
  return r;
}
