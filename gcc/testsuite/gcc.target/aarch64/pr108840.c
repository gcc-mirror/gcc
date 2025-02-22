/* PR target/108840.  Check that the explicit &31 is eliminated.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdisable-rtl-combine" } */

int
foo (int x, int y)
{
  return x << (y & 31);
}

void
bar (int x[3], int y)
{
  x[0] <<= (y & 31);
  x[1] <<= (y & 31);
  x[2] <<= (y & 31);
}

void
baz (int x[3], int y)
{
  y &= 31;
  x[0] <<= y;
  x[1] <<= y;
  x[2] <<= y;
}

void corge (int, int, int);

void
qux (int x, int y, int z, int n)
{
  n &= 31;
  corge (x << n, y << n, z >> n);
}

/* { dg-final { scan-assembler-not {and\tw[0-9]+, w[0-9]+, 31} } } */

