/* Related to PR optimization/10764  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double atan(double x);

double foo(double x)
{
  return atan(atan(x));
}

