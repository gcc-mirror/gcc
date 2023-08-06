/* { dg-do compile } */
/* { dg-options "-O2" } */
float
__cbrtf (float x)
{
  double r = 13322 * 1111;
  float ub = r;
  long long cvt1 = x;
  long long m0 = cvt1 << 19;
  long long m1 = m0 >> 63;
  if ((m0 ^ m1) < (1ULL << 31))
    {
      cvt1 = (cvt1 + (1 << 31)) & 0xffffffff00000000;
      ub = cvt1;
    }
  return ub;
}
