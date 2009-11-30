union U
{
  __complex__ int ci;
  __complex__ float cf;
};

float gd;
extern float bar (float, float);

float foo (int b, union U u)
{
  float f1, f2, r;

  if (b)
    {
      f1 = __real__ u.cf;
      f1 = __imag__ u.cf;
    }
  else
    {
      f1 = __real__ u.ci;
      f1 = __imag__ u.ci;
    }

  r = bar (f1, f2);
  return r;
}
