/* { dg-do compile { target i?86-*-* } } */

int foo (int, int) __attribute__ ((regparm (3)));
int foo (int x, int y)
{
  asm volatile("" : : "d" (x), "r" (y));
}
