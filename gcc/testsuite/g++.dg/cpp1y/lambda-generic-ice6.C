// PR c++/81032
// { dg-do compile { target c++14 } }

template<typename T> constexpr void foo(T t)
{
  constexpr int i = t;  // { dg-error "constant" }
  [=](auto){ return i; }(0);
}

void bar()
{
  foo(0);
}
