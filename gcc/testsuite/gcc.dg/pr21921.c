/* { dg-do compile } */
/* { dg-options "-O1 -funsafe-math-optimizations" } */

void
Q (double *s, double h)
{
  int i;
  if (h > 1)
    h = h - 1;

  for (i = 1; i < 3; i++)
    if (s[i] / h > 0)
      s[0] = h, s[i] = s[i] / h;
}
