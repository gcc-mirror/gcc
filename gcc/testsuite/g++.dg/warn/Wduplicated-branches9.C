// PR c++/120940
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

static char a[16][8], b[16][8];

char *
foo (int x, int y)
{
  return (x ? a : b)[y];
}
