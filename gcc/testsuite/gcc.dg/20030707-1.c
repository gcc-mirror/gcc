/* Derived from PR target/10979.  */
/* This testcase used to ICE on x86.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

void t(double);
double atan2(double,double);

void temp(double *c)
{
  double c2 = 8;
  double s2 = 0;
  *c = atan2(s2,c2);
  t(1/s2);
}

