/* PR c/80097 */
/* { dg-do compile } */
/* { dg-options "-std=c89 -fsanitize=float-divide-by-zero" } */

int
foo (double a)
{
  int b = (1 / a >= 1);
  return b;
}
