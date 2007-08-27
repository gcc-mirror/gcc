/* { dg-do compile { target rs6000-*-* } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tstwu 1,-\[0-9\]*(1)\n" } } */

int Leaf (int i)
{
  return i + 1;
}
