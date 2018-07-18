/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct foo { unsigned a: 3, b: 5, c: 24; };

int
f (struct foo i)
{
  return i.b;
}
/* { dg-final { scan-assembler "movb\.cl" } } */
