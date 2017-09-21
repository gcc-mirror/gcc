/* PR target/82260 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-Os -mtune=generic -masm=att -mtune-ctrl=^partial_reg_dependency" } */
/* { dg-final { scan-assembler-not {\mmovb\t%sil, %cl} } } */
/* { dg-final { scan-assembler {\mmovl\t%esi, %ecx} } } */
/* { dg-final { scan-assembler {\mmovb\t%dl, %cl} } } */
/* { dg-final { scan-assembler {\mmovb\t%r8b, %cl} } } */

int
foo (int x, int c)
{
  return x >> c;
}

int
bar (int x, int y, int z)
{
  return x >> z;
}

int
baz (int x, int y, int z, int u, int v)
{
  return x >> v;
}
