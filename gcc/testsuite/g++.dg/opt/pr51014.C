// PR rtl-optimization/51014
// { dg-do compile }
// { dg-options "-O2 -funroll-loops -fcompare-debug" }

struct S
{
  ~S() { delete s; }
  int *s;
};

void
f (S *x, S *y)
{
  for (; x != y; ++x)
    x->~S();
}
