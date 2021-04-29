/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-bit-ccp -ftree-slp-vectorize" } */

#include <vector>

struct T
{
};

struct S
{
  std::vector < std::vector < T > > v;
  char x;
  char y[16];
  char z[16];
};

S s, g[1];

void
foo (char *buf)
{
  s = g[*buf];
}

char c;

int
main ()
{
  foo (&c);
  return 0;
}
