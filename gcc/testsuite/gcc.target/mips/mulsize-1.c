/* { dg-final { scan-assembler "\t.globl\tf7" } } */
/* { dg-final { scan-assembler "\tsubu\t" } } */
/* { dg-final { scan-assembler-not "\tli\t" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
int
f7(int x)
{
  return x * 7;
}
