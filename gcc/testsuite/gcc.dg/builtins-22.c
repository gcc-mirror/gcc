/* Related to PR optimization/10764  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double exp(double x);

double foo(double x)
{
  return exp(exp(x));
}

