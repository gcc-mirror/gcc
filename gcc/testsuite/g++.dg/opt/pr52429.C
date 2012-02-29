// PR tree-optimization/52429
// { dg-do compile }
// { dg-require-effective-target pthread }
// { dg-options "-O -g -ftree-parallelize-loops=4" }

struct B
{
  B () : b (__null) {}
  int *b;
};

void *
operator new (__SIZE_TYPE__, void *p)
{
  return p;
}

void
foo (B *x, unsigned y)
{
  while (y--)
    new (x) B;
}
