/* PR tree-optimization/126471 */
/* { dg-do run { target bitint575 } } */
/* { dg-options "-O2" } */

[[gnu::noipa]] int
foo (unsigned _BitInt(129) x)
{
  return __builtin_parityg (~x);
}

[[gnu::noipa]] int
bar (unsigned _BitInt(7) x)
{
  return __builtin_parityg (~x);
}

[[gnu::noipa]] int
baz (unsigned _BitInt(256) x)
{
  return __builtin_parityg (~x);
}

int
main ()
{
  if (foo (0) != 1
      || foo (~(unsigned _BitInt(129)) 0) != 0
      || foo (1) != 0
      || bar (0) != 1
      || bar (~(unsigned _BitInt(7)) 0) != 0
      || bar (1) != 0
      || baz (0) != 0
      || baz (~(unsigned _BitInt(256)) 0) != 0
      || baz (1) != 1)
    __builtin_abort ();
}
