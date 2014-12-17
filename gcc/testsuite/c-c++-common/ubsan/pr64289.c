/* PR sanitizer/64289 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

int
foo (int a)
{
  return (int) (0 ? 0 : a ? a : 0.5);
}
