// { dg-do compile }
// { dg-options "-O2 -fdump-tree-fre1" }

struct mytest
{
  float a;
  char buf[256];
};

int foo(mytest *m, int *i)
{
  int tmp = *i;
  m->a = 10.0f;
  return tmp + *i;
}

// we should be able to CSE *i despite mytest having a cbar[] buffer
// and thus being subject to TYPE_TYPELESS_STORAGE
// { dg-final { scan-tree-dump-times "\\*i" 1 "fre1" } }
