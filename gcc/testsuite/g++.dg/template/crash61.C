// PR c++/29733

template<typename T> void foo()
{
  T t = 0; // { dg-error "function type" }
}

void bar()
{
  foo<int()>();
}
