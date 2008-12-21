/* { dg-options "-mbranch-cost=10 isa>=4 -O2" } */
NOMIPS16 int
foo (int x, int y, int z, int k)
{
  return x == k ? x + y : z - x;
}
/* { dg-final { scan-assembler "\t(movz|movn)\t" } } */
/* { dg-final { scan-assembler-not "\t(bne|beq)\t" } } */
