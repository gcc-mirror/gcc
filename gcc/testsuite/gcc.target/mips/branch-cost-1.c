/* { dg-mips-options "-mbranch-cost=1 -mips64 -O2" } */
NOMIPS16 int
foo (int x, int y, int z, int k)
{
  return x == k ? x + y : z - x;
}
/* { dg-final { scan-assembler-not "\t(movz|movn)\t" } } */
/* { dg-final { scan-assembler "\t(bne|beq)\t" } } */
