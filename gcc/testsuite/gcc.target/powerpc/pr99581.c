/* { dg-do compile } */
/* { dg-options "-O2" } */

char e[37540];
struct A { int c; } d;

void
bar (int n)
{
  __asm__("" : : "r" (e));
}

void
foo (void)
{
  __asm__("stw %1, %0" : "=o" (d.c) : "r" (0));
}
