/* { dg-do compile } */
/* { dg-options "-Ofast" } */

double
foo (double f(void))
{
  return 2. * f ();
}
