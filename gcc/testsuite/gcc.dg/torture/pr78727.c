/* { dg-do compile } */

int
dd (int gj, unsigned int o7)
{
  long long int e8 = gj;

  e8 |= gj + 1u;
  if (e8 != 0)
    {
      short int *mn = (short int *)&e8;
      int pv;

      e8 &= e8 > gj;
      gj = o7 > e8;
      pv = ((gj != 0) ? gj : *mn) && e8;
      gj |= *mn / pv;
    }

  return gj;
}
