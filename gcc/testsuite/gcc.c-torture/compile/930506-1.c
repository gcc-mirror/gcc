long long
f (a)
     double a;
{
  double b;
  unsigned long long v;

  b = a / 2.0;
  v = (unsigned) b;
  a -= (double) v;
  return v;
}
