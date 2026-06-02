/* { dg-do compile } */

int b(int c)
{
  int a;
  __asm__ goto("" : "=r"(a) : : : d);
d:
  return c && a != 42 && a >= 42;
}
