extern "C" void printf (char *, ...);
template <class T> T max (const T&x, const T&y)
{
  return (x>y)?x:y;
}

class complex
{
  double re, im;
 public:
  complex (double r, double i=0) { re = r; im = i; }
  friend int operator > (const complex& x, const complex &y);
  void print () { printf ("re = %g; im = %g;\n", re, im); }
};
int operator >(const complex& x, const complex &y)
{
  double c1 = x.re * x.re + x.im * x.im;
  double c2 = y.re * y.re + y.im * y.im;
  return c1 > c2;
}

int main ()
{
  complex c1 (1, 0);
  complex c2 (2, 0);
  complex c3 (2, 3);
  complex c4 (2, 1);

  complex m1 = max (c1, c2);
  complex m2 = max (c3, c4);
  m1.print ();
  m2.print ();
  return 0;
}
