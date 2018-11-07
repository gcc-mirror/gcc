// PR c++/87152
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<int> void foo()
{
  int a[] = { 1, 1, 1 };
  for (int i = 0; auto x : a);
  int i;
}

void
bar ()
{
  foo<0>();
}
