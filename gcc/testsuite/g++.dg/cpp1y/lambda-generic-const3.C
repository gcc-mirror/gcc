// PR c++/79640
// { dg-do compile { target c++14 } }

template<typename F> void foo(F f)
{
  f(1);
}

template<int> void bar()
{
  const int i = i;
  foo([] (auto) { sizeof(i); });
}

void baz() { bar<1>(); }
