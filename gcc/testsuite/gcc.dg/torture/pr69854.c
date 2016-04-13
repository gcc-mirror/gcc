/* { dg-do compile } */
/* { dg-additional-options "-frounding-math -ffast-math" } */

double fn1()
{
  double w, s = fn1() - 6.12323399573676603587e17;
  return 1.57079632679489655800e00 - (s + w);
}
