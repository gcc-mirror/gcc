/* Derived from PR optimization/10764  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double log(double x);

double ndtri(double y0)
{
  double x;

  x = log(y0);
  x = log(x);

  return x;
}

