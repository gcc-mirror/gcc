// PR c++/31038
// { dg-options "" }

template<int> void foo()
{
  int i = (int) { 0 };
}

template void foo<0>();
int f();

template<int> void bar()
{
  int i = (int) { f() };
}

template void bar<0>();
