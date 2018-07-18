double
f (long double x)
{
  union { long double ld; double d[2]; } u;
  u.ld = x;
  return u.d[1];
}
