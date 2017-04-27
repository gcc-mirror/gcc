/* { dg-do compile { target powerpc-*-*spe } } */
/* { dg-options "-O2 -ftracer -fPIC" } */
long long int vi, ls;
int wq, oa, to, fv;
signed char zo;

long long int
ai (long long int ip, long long int jc, int gt)
{
  ip /= 3;
  jc += ip;
  if (ip != 0)
    vi = 0;
  vi += ls;

  if (wq != oa)
    {
      int tz;

      for (tz = 0; tz < 32; ++tz)
        zo -= wq & gt;

      if ((gt & 5) > oa)
        {
          zo += gt;
          fv += zo + to;
        }

      if (gt != 0)
        oa = 0;

      if (fv != 0)
        {
          vi += wq;
          ls += ip;
          jc += (vi != 0) ? ip : ls;
        }

      while (tz != 0)
        {
          zo = wq;
          tz = zo;
        }

      ++to;
      wq = ip;
    }

  return jc;
}
