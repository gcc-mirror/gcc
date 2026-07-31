/* PR tree-optimization/126476 */
/* { dg-do run { target bitint } } */

[[gnu::noipa]] int
foo (unsigned _BitInt(4) n)
{
  return ((1ULL << n) & (1ULL << 20)) != 0;
}

[[gnu::noipa]] int
bar (unsigned _BitInt(4) n)
{
  return (((1ULL << 40) >> n) & (1ULL << 20)) != 0;
}

int
main ()
{
  for (unsigned i = 0; i < 16; i++)
    if (foo (i) != 0 || bar (i) != 0)
        __builtin_abort ();
}
