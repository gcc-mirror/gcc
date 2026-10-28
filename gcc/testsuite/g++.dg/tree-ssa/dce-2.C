/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
#include <new>

bool ne (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  delete[] p;
  return p != 0;
}

bool eq (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  delete[] p;
  return p == 0;
}

bool conv (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  delete[] p;
  return p;
}

int ternary (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  delete[] p;
  return p != 0 ? 1 : 0;
}

int copy (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  int c = p != 0;
  delete[] p;
  return c;
}

int result;
void store (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  result = p != 0;
  delete[] p;
}

int twice (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  int a = p != 0;
  int b = p == 0;
  delete[] p;
  return a + b;
}

/* { dg-final { scan-tree-dump-not "operator new" "optimized" } } */
/* { dg-final { scan-tree-dump-not "operator delete" "optimized" } } */
