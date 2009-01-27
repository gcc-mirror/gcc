/* { dg-do compile { target { fixed_point } } } */
/* { dg-options "-O2 -mgp32 -mdsp" } */
/* { dg-final { scan-assembler-times "\tdpaq_sa.l.w\t\\\$ac" 3 } } */

NOMIPS16 _Sat long long _Fract
f1 (_Sat long _Fract x, _Sat long _Fract y, _Sat long long _Fract z)
{
  return (_Sat long long _Fract) x * y + z;
}

NOMIPS16 _Sat long long _Fract
f2 (_Sat long _Fract x, _Sat long _Fract y, _Sat long long _Fract z)
{
  return z + (_Sat long long _Fract) y * x;
}

NOMIPS16 _Sat long long _Fract
f3 (_Sat long _Fract x, _Sat long _Fract y, _Sat long long _Fract z)
{
  _Sat long long _Fract t = (_Sat long long _Fract) x * y;
  int temp = 5;
  if (temp == 5)
    z = t + z; /* Need to put z at the end.  GCC does not swap operands to
		  match the ssmadd pattern, because types are saturating.  */
  return z;
}

long long _Fract
f4 (long _Fract x, long _Fract y, long long _Fract z)
{
  return (long long _Fract) x * y + z;
}

long long _Fract
f5 (long _Fract x, long _Fract y, long long _Fract z)
{
  return z + (long long _Fract) y * x;
}

long long _Fract
f6 (long _Fract x, long _Fract y, long long _Fract z)
{
  long long _Fract t = (long long _Fract) x * y;
  int temp = 5;
  if (temp == 5)
    z = t + z; /* Need to put z at the end.  GCC does not swap operands to
		  match the ssmadd pattern, because types are saturating.  */
  return z;
}
