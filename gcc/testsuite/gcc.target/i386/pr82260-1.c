/* PR target/82260 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-Os -mtune=generic -masm=att -mno-bmi2" } */
/* movl %esi, %ecx is shorter than movb %sil, %cl.  While
   movl %edx, %ecx is the same size as movb %dl, %cl and
   movl %r8d, %ecx is the same size as movb %r8b, %cl, movl
   is faster on contemporary CPUs.  */
/* { dg-final { scan-assembler-not {\mmovb\M} } } */

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
