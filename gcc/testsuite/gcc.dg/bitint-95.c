/* PR rtl-optimization/114044 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O -fno-tree-dce" } */

void
foo (void)
{
  unsigned _BitInt (575) a = 3;
  __builtin_clzg (a);
}

void
bar (void)
{
  unsigned _BitInt (575) a = 3;
  __builtin_ctzg (a);
}

void
baz (void)
{
  signed _BitInt (575) a = 3;
  __builtin_clrsbg (a);
}

void
qux (void)
{
  signed _BitInt (575) a = 3;
  __builtin_ffsg (a);
}

void
garply (void)
{
  unsigned _BitInt (575) a = 3;
  __builtin_parityg (a);
}

void
corge (void)
{
  unsigned _BitInt (575) a = 3;
  __builtin_popcountg (a);
}
