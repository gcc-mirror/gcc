/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x, int y)
{
  return (x==21) ^ (y==69);
}

/* { dg-final { scan-assembler "xor.pred" } } */
