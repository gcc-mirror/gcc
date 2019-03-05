/* PR target/54589 */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler "movl\[ \t]+(?:t\\+336\\(%r..\\)|336\\(%r..,%r..\\)), %eax" } } */
/* { dg-final { scan-assembler "movl\[ \t]+340\\(%r..,%r..\\), %eax" } } */
/* { dg-final { scan-assembler-times "salq\[^\n\r]*4, %" 2 } } */
/* { dg-final { scan-assembler-not "addq\[ \t]" } } */

struct S { int a, b, c, d; };
struct T { struct S e[16]; struct S f[1024]; } t;

int
foo (unsigned long x)
{
  return t.f[x + 5].a;
}

int
bar (struct T *x, unsigned long y)
{
  return x->f[y + 5].b;
}
