/* PR tree-optimization/113466 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2" } */

int foo (int);

__attribute__((returns_twice, noipa)) _BitInt(325)
bar (_BitInt(575) x)
{
  (void) x;
  return 0wb;
}

__attribute__((returns_twice, noipa)) _BitInt(325)
garply (_BitInt(575) x, _BitInt(575) y, _BitInt(575) z, int u, int v, _BitInt(575) w)
{
  (void) x;
  (void) y;
  (void) z;
  (void) u;
  (void) v;
  (void) w;
  return 0wb;
}

_BitInt(325)
baz (_BitInt(575) y)
{
  foo (1);
  return bar (y);
}

_BitInt(325)
qux (int x, _BitInt(575) y)
{
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  return bar (y);
}

void
corge (int x, _BitInt(575) y, _BitInt(325) *z)
{
  void *q[] = { &&l1, &&l2, &&l3, &&l3 };
  if (x == 25)
    {
    l1:
      x = foo (2);
    }
  else if (x == 42)
    {
    l2:
      x = foo (foo (3));
    }
l3:
  *z = bar (y);
  if (x < 4)
    goto *q[x & 3];
}

_BitInt(325)
freddy (int x, _BitInt(575) y)
{
  bar (y);
  ++y;
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  return bar (y);
}

_BitInt(325)
quux (_BitInt(575) x, _BitInt(575) y, _BitInt(575) z)
{
  _BitInt(575) w = x + y;
  foo (1);
  return garply (x, y, z, 42, 42, w);
}

_BitInt(325)
grault (int x, _BitInt(575) y, _BitInt(575) z)
{
  _BitInt(575) v = x + y;
  _BitInt(575) w = x - y;
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  return garply (y, z, v, 0, 0, w);
}

_BitInt(325)
plugh (int x, _BitInt(575) y, _BitInt(575) z, _BitInt(575) v, _BitInt(575) w)
{
  garply (y, z, v, 1, 2, w);
  ++y;
  z += 2wb;
  v <<= 3;
  w *= 3wb;
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  return garply (y, z, v, 1, 2, w);
}
