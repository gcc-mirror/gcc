/* { dg-final { scan-assembler "\t.globl\tf15" } } */
/* { dg-final { scan-assembler "\tsll\t" } } */
/* { dg-final { scan-assembler "\tsubu\t" } } */
/* { dg-final { scan-assembler-not "\tli\t" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
int
f15(int x)
{
  return x * 15;
}
