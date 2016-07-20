/* { dg-do compile } */
/* { dg-options "-Ofast" { target *-*-* } } */

short unsigned int ve;

void
u1 (void)
{
  int oq = 0;

  while (ve != 0)
    {
      int j4, w7 = oq;

      oq = 0 / oq;
      ve %= oq;
      j4 = ve ^ 1;
      ve ^= oq;
      if (j4 != 0 ? j4 : ve)
        oq = ve;
      else
        if (w7 != 0)
          oq = ve;
    }
}
