// PR c++/58843

struct A {};

template<typename T> void foo(T t)
{
  t.T::~X();  // { dg-error "no type" }
}

void bar()
{
  foo(A());
}
