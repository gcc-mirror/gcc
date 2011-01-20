/* PR tree-optimization/46130 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce" } */

extern int bar (int);

static int foo (int x)
{
  int z, w;
  if (x <= 1024)
    {
      z = 16;
      w = 17;
    }
  else
    {
      bar (bar (bar (bar (bar (bar (bar (bar (bar (16)))))))));
      if (x > 131072)
	w = 19;
      else
	w = 21;
      z = 32;
    }
  w = w + 121;
  return z;
}

int
baz (int x)
{
  return foo (x + 6) + foo (x + 15) + foo (x + 24);
}
