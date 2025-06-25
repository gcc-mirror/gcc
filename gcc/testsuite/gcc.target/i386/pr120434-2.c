/* PR middle-end/120434 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler-not "\tmovslq\t%edi, %rdi" } } */
/* { dg-final { scan-assembler "\tmovl\t%edi, %edi" } } */

extern unsigned long long foo (unsigned long long x);

unsigned long long
bar (int x)
{
  if (x < 50)
    return 0;
  return foo (x);
}
