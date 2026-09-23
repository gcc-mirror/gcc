/* PR tree-optimization/126490 */
/* { dg-do run { target bitint } } */

typedef unsigned _BitInt(1) T;

[[gnu::noipa]] int
foo (T a, T b)
{
  return ((a & b) == (a ^ b)) + 1;
}

[[gnu::noipa]] int
bar (T a, T b)
{
  return ((a & b) == (a ^ b)) != 0;
}

[[gnu::noipa]] int
baz (T a, T b)
{
  return ((a & b) == (a ^ b)) == 0;
}

[[gnu::noipa]] int
qux (T a, T b)
{
  return ((a & b) == (a ^ b)) < 1;
}

[[gnu::noipa]] int
corge (int a, int b)
{
  return (a & b) == (a ^ b);
}

[[gnu::noipa]] int
garply (T a, T b)
{
  return ((a & b) ^ (a == b)) + 1;
}

[[gnu::noipa]] int
fred (T a, T b)
{
  return ((a & b) ^ (a == b)) != 0;
}

[[gnu::noipa]] int
xyzzy (T a, T b)
{
  return ((a & b) ^ (a == b)) == 0;
}

[[gnu::noipa]] int
waldo (T a, T b)
{
  return ((a & b) ^ (a == b)) < 1;
}

int
main ()
{
  for (int i = 0; i < 4; ++i)
    {
      int a = i & 1;
      int b = i >> 1;
      int c = corge (a, b);
      if (foo (a, b) != c + 1
          || bar (a, b) != (c != 0)
          || baz (a, b) != (c == 0)
          || qux (a, b) != (c < 1)
	  || garply (a, b) != c + 1
          || fred (a, b) != (c != 0)
          || xyzzy (a, b) != (c == 0)
          || waldo (a, b) != (c < 1))
	__builtin_abort ();
    }
}
