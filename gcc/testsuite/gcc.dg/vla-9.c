/* { dg-do compile  } */
/* { dg-options "-std=c99 -pedantic-errors -W -Wall" } */
/* PR c/28280 */

void f(__SIZE_TYPE__ d)
{
  typedef int t[d];
  t *g = (__typeof (g)) d;
}
