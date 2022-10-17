/* PR rtl-optimization/103908 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdisable-tree-cselim -fno-tree-sink" } */
/* { dg-final { scan-assembler "# insn 1" } } */
/* { dg-final { scan-assembler "# insn 2" } } */

int a, b;

void
foo (void)
{
  if (a)
    {
      b = 1;
      asm goto ("# insn 1" : : : : lab1);
    lab1:;
    }
  else
    {
      b = 1;
      asm goto ("# insn 2" : : : : lab2);
    lab2:;
    }
}
