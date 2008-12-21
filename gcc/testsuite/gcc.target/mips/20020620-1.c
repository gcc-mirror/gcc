/* { dg-do compile } */
/* { dg-options "-O2 -mlong64" } */
int foo (int *x, int i)
{
  return x[i] + i;
}
/* { dg-final { scan-assembler-not "move" } } */
