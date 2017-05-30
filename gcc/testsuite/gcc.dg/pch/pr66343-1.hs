/* PR sanitizer/66343 */
/* { dg-options "-fsanitize=undefined" } */

void
foo (int a, int b)
{
  a / b;
}
