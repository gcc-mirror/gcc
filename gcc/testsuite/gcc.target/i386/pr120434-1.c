/* PR middle-end/120434 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler-times "\tsar\[lq]\t" 2 } } */
/* { dg-final { scan-assembler-times "\tshr\[lq]\t" 2 } } */

[[gnu::noipa]] int
foo (int x)
{
  return x / 200;
}

[[gnu::noipa]] int
bar (int x)
{
  if (x < 0)
    __builtin_unreachable ();
  return x / 200;
}

[[gnu::noipa]] int
baz (int x)
{
  if (x >= 0)
    return x / 200;
  else
    return 24;
}
