/* { dg-options "(!HAS_LSA)" } */
/* { dg-final { scan-assembler "\t.globl\tf17" } } */
/* { dg-final { scan-assembler "\tsll\t" } } */
/* { dg-final { scan-assembler "\taddu\t" } } */
/* { dg-final { scan-assembler-not "\tli\t" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
int
f17(int x)
{
  return x * 17;
}
				    