/* Test that the assume can be copied before creating the edges.  */

/* { dg-do compile } */
/* { dg-options "-O3" } */

struct S
{
  int x, y;
};

inline bool
baz (S x)
{
  return x.x < 42;
}

inline bool
qux (S x, int h)
{
  [[assume (baz (x))]];
  return x.x < 42 && x.y > 52 && h < 50;
}

void
corge (S x, S y, S z, bool *p)
{
  for (int i = 0; i < x.x + x.y; i++)
    {
      p[0] = qux (x, 41);
      p[1] = qux (y, 60);
      p[2] = qux (z, 20);
    }
}
