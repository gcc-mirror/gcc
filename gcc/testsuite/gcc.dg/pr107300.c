/* PR ipa/107300 */
/* { dg-do compile } */
/* { dg-options "-O2 -fipa-cp-clone -funreachable-traps -fno-inline" } */

void
bar (int x, int y)
{
  if (x)
    __builtin_unreachable ();

  if (y)
    __builtin_abort ();
}

void
foo (void)
{
  bar (0, 0);
}
