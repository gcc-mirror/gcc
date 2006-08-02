// PR c++/28557

struct A
{
  template<typename T> operator T() { return T(); }
};

template<int> void foo()
{
  A().operator int();
}

void bar()
{
  foo<0>();
}
