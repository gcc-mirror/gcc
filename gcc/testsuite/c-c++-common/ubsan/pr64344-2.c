/* PR sanitizer/64344 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

int
foo (void)
{
  static const int a = 0.5;
  static const int b = (int) 13.5 + 1;
  return a + b;
}
