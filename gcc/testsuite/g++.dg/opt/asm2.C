/* PR inline-asm/15740 */
/* { dg-do compile } */
/* { dg-options "-O" } */

void foo(void)
{
  int a, b;
  a = 1;
  b = a + 1;
  asm ("" : : "m" (a));
}
