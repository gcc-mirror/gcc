/* Every allocation here must survive DCE.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-mismatched-new-delete" } */
#include <new>

bool cmp_other (unsigned s, int *q)
{
  int *p = new (std::nothrow) int[s];
  delete[] p;
  return p == q;
}

int *sink;
void escape (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  sink = p;
  delete[] p;
}

void vol (unsigned s)
{
  char *p = new (std::nothrow) char[s + 1];
  *(volatile char *) p = 4;
  delete[] p;
}

bool direct (unsigned s)
{
  void *p = ::operator new (s, std::nothrow);
  ::operator delete (p, std::nothrow);
  return p != 0;
}

/* Mismatched new[] / delete.  */
bool mismatch (unsigned s)
{
  int *p = new (std::nothrow) int[s];
  delete p;
  return p != 0;
}

/* { dg-final { scan-tree-dump-times "operator new" 5 "optimized" } } */
