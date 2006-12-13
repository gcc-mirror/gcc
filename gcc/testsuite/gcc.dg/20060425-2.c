/* { dg-do compile } */
/* { dg-options "-O2" } */

double
crashme (double v, double *p)
{
  if (v < 0. && *p == 1.)
    v = 0.;
  return v;
}
