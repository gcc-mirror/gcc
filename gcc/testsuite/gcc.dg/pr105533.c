/* PR middle-end/105533 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

long long
foo (long long x, long long y)
{
  return ((x < 0) & (y != 0)) * (-__LONG_LONG_MAX__ - 1);
}
