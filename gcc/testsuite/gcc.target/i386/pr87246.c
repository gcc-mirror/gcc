/* PR rtl-optimization/87246 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -w -fnon-call-exceptions -fno-split-wide-types" } */

__int128 zd;
int c1;

void
s2 (__int128 *qv)
{
  if (*qv != 0)
    {
      zd = 0;
      c1 = c1 <= *qv;
    }
}

void
lt (unsigned int vb)
{
  s2 ((__int128 *) (vb + 1));
}
