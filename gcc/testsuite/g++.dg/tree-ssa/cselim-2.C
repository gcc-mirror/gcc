// PR tree-optimization/127053
// { dg-do compile }
// { dg-options "-O2 -ftree-cselim -fdump-tree-phiopt1-details" }
// testcase reduced from std::optional::reset.

void sink(int*);
typedef int T;

int f(int b, T *c)
{
  int a;
  sink(&a);
  if (b)
    {
      a = 0;
      c->~T();
    }
  else
      a = 0;
  return a;
}

// { dg-final { scan-tree-dump "factoring out stores" "phiopt1" } }
