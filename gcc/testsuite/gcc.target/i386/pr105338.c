/* PR target/105338 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -masm=att" } */
/* { dg-final { scan-assembler-times "\tnegl\t" 3 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t" 3 } } */
/* { dg-final { scan-assembler-times "\tandl\t" 3 } } */

int
foo (int i)
{
  return i ? 5 : 0;
}

int
bar (int b)
{
  return !!b * 5;
}

int
baz (int b)
{
  if (!b)
    return 0;
  return 5;
}
