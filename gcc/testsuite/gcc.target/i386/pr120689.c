/* PR target/120689 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=generic -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-not "\t\(movzbl\|shrl\|salq\|orq\)\t" } } */

struct S { char a, b, c; };

[[gnu::noipa]]
void foo (struct S x, struct S y, struct S z)
{
}

void
bar (struct S x, struct S y, struct S z)
{
  [[gnu::musttail]] return foo (x, y, z);
}
