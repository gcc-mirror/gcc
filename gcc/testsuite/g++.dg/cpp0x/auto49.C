// PR c++/84350
// { dg-do compile { target c++11 } }

template<typename... T> void foo(T... t)
{
  new auto(t...);  // { dg-error "invalid use" }
}

void bar()
{
  foo();
}
