/* PR target/127352 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O3" } */

_BitInt(65) a, b, c;
__int128 d;
_BitInt(7) e;

long
foo (long x)
{
  return x;
}

_BitInt(13)
bar (unsigned x, _BitInt(3) y)
{
  return y == 0 ? : x;
}

_BitInt(65)
baz (_BitInt(65) x, _BitInt(5) y)
{
  return y == 0 ? x : y;
}

_BitInt(5)
qux ()
{
  _BitInt(47) f;
  unsigned g = d & 15;
  for (unsigned i = 0; i < g; i++)
    {
      switch (a)
	{
	case 0:
	  b = 0;
	case 8:
	  e = 0;
	  break;
	case 10:
	  long h = g;
	  f = ~foo (~h);
	  e = f;
	}
      if (0 >= b)
	continue;
      for (unsigned j = 0; j < 1; j++)
	b = (_BitInt(7)) (0 >= d ? bar (e, a) : 0);
      e = c ? baz (b, i) : 0;
    }
  return b;
}
