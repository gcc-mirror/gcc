extern "C" void printf (char *, ...);
template <class T> T max (const T&x, const T&y)
{
  return (x>y)?x:y;
}
int min (const float&, const float&);
int min (const int& i1, const int& i2) {
  return (i1 < i2) ? i1 : i2;
}

class complex
{
  double re, im;
 public:
  complex (double r, double i=0) { re = r; im = i; }
  friend int operator > (const complex& x, const complex &y) { return 0; }
  void print () { }
};

int main ()
{
  complex c1 (1, 0);
  complex c2 (2, 0);

  int j = max (1, 37);
  complex m1 = max (c1, c2);
  m1.print ();
  printf ("j=%d\n", j);
  return 0;
}
