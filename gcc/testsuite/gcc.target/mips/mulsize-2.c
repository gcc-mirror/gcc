/* { dg-final { scan-assembler "\t.globl\tf9" } } */
/* { dg-final { scan-assembler "\tsubu\t" } } */
/* { dg-final { scan-assembler-not "\tli\t" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
int
f9(int x)
{
  return x * 9;
}
