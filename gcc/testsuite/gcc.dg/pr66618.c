/* PR c/66618 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo (void)
{
  const int a = 0;
  static int b = a;	/* { dg-bogus "initializer element is not constant" } */
  return b;
}
