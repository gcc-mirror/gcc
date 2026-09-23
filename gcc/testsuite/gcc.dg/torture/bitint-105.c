/* PR tree-optimization/126504 */
/* { dg-do run { target bitint } } */

typedef unsigned _BitInt(4) U;

[[gnu::noipa]] int
foo (U n)
{
  return ((1 << n) & (1 << 20)) != 0;
}

[[gnu::noipa]] int
bar (U n)
{
  return ((unsigned) (1 << n) & (1u << 20)) != 0;
}

int
main ()
{
  for (unsigned i = 0; i < 16; i++)
    if (foo (i) || bar (i))
      __builtin_abort ();
}
