/* { dg-do compile } */
/* { dg-options "-mfixed-range=1-20" } */
/* { dg-final { scan-assembler "lqd.*21" } } */

int foo (int i)
{
  return i;
}
