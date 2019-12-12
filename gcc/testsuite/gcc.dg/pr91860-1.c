/* { dg-do compile } */
/* { dg-options "-Og -fipa-cp -g --param=max-combine-insns=3" } */

char a;
int b;

static void
bar (short d)
{
  d <<= __builtin_sub_overflow (0, d, &a);
  b = __builtin_bswap16 (~d);
}

void
foo (void)
{
  bar (21043);
}
