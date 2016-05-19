/* PR rtl-optimization/71148 */
/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops" } */

int rh, ok, kq, fu;

void
js (int cs)
{
  rh = fu;
  if (fu != 0)
    {
      cs /= 3;
      if (cs <= 0)
        {
          int z9;
          for (z9 = 0; z9 < 2; ++z9)
            {
              z9 += cs;
              ok += z9;
              fu += ok;
            }
        }
    }
}

void
vy (int s3)
{
  int yo, g2 = 0;
 sd:
  js (g2);
  for (yo = 0; yo < 2; ++yo)
    {
      if (fu != 0)
        goto sd;
      kq += (s3 != (g2 ? s3 : 0));
      for (s3 = 0; s3 < 72; ++s3)
        g2 *= (~0 - 1);
      g2 -= yo;
    }
  for (fu = 0; fu < 18; ++fu)
    for (yo = 0; yo < 17; ++yo)
      if (g2 < 0)
        goto sd;
}
