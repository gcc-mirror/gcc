// PR c++/58646
// { dg-additional-options -Wno-vla }

void foo(int n)
{
  int a[2][n] = {};
}
