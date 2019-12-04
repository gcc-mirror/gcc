/* PR tree-optimization/92734 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* Verify there are no binary additions or subtractions left.  There can
   be just casts and negations.  */
/* { dg-final { scan-tree-dump-not " \[+-] " "optimized" } } */

int
f1 (int x, unsigned y)
{
  int a = x + y;
  return a - x;
}

unsigned
f2 (unsigned x, int y)
{
  unsigned a = (int) x + y;
  return a - x;
}

int
f3 (int x, unsigned y)
{
  int a = x - y;
  return a - x;
}

unsigned
f4 (unsigned x, int y)
{
  unsigned a = (int) x - y;
  return a - x;
}

int
f5 (unsigned x, int y)
{
  int a = x - y;
  return a + y;
}

unsigned
f6 (int x, unsigned y)
{
  unsigned a = x - (int) y;
  return a + y;
}

int
f7 (int x, unsigned y)
{
  int a = x + y;
  return x - a;
}

unsigned
f8 (unsigned x, int y)
{
  unsigned a = (int) x + y;
  return x - a;
}

int
f9 (int x, unsigned y)
{
  int a = x - y;
  return x - a;
}

unsigned
f10 (unsigned x, int y)
{
  unsigned a = (int) x - y;
  return x - a;
}
