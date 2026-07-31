/* PR tree-optimization/126503 */
/* { dg-do run { target bitint575 } } */

[[gnu::noipa]] unsigned _BitInt(400)
foo (_BitInt(257) a)
{
  return (unsigned _BitInt(400)) (-a);
}

[[gnu::noipa]] unsigned _BitInt(300)
bar (_BitInt(7) a, unsigned _BitInt(17) b)
{
  _BitInt(257) x = (_BitInt(257)) a + -1;
  return ~((unsigned _BitInt(300)) x ^ (unsigned _BitInt(300)) b);
}

[[gnu::noipa]] _BitInt(129)
baz (_BitInt(129) x)
{
  return x - 24;
}

[[gnu::noipa]] int
qux (_BitInt(129) x, _BitInt(129) y)
{
  return x == y;
}

int
main ()
{
  if (foo (-1wb) != 1uwb)
    __builtin_abort ();
  if (bar (1wb, 3uwb) != (unsigned _BitInt(300)) -4wb)
    __builtin_abort ();
  if (!qux (baz (100), 76))
    __builtin_abort ();
}
