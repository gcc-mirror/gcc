/* PR target/92549 */
/* { dg-do compile } */
/* { dg-options "-Os -masm=att" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */
/* { dg-final { scan-assembler "xchgl" } } */

__attribute__((noipa)) int
bar (int a, int b)
{
  return b - a + 5;
}

int
foo (int a, int b)
{
  return 1 + bar (b, a);
}
