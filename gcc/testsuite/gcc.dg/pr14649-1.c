/* PR c/14649 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

double atan(double);

const double pi = 4*atan(1.0);  /* { dg-warning "(not constant)|(near initialization)" } */

const double ok = 4*__builtin_atan(1.0);

double foo()
{
  double ok2 = 4*atan(1.0);
  return ok2;
}

