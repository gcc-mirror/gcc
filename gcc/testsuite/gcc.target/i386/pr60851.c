/* { dg-do compile } */
/* { dg-options "-O2 -flive-range-shrinkage -mtune=bdver4 -mdispatch-scheduler" } */

long double ld (char c)
{
  return c;
}
