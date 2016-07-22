/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct foo { unsigned a: 3, b: 8, c: 21; } bar;

void
f (void)
{
  bar.b = 42;
}

void
g (int j)
{
  bar.b = j;
}
/* { dg-final { scan-assembler "movbi\[ \t\]" } } */
/* { dg-final { scan-assembler "movb\[ \t\]" } } */
