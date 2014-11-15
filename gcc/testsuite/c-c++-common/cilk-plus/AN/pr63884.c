/* PR middle-end/63884 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int
foo (int x, int y)
{
  int r;
  return __builtin_sadd_overflow (x, y, &r);
}
