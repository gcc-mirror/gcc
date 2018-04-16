// PR c++/85258
// { dg-do compile { target c++11 } }

template<int> void foo()
{
  int x[8];
  for (int& i, j : x) // { dg-error "multiple" }
    i = 0; // { dg-error "local variable" }
}

void bar()
{
  foo<0>();
}
