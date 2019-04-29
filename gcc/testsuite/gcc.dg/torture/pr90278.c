/* { dg-do compile } */
/* { dg-additional-options "-fexceptions -fnon-call-exceptions" } */

double
hc (void)
{
  double dp = 0.0;
  double ek[1];

  ek[0] = 1.0 / dp < 0.0;

  return ek[0];
}
