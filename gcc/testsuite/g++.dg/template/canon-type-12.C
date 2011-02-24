// { dg-options "-std=c++0x" }

template<class T, T t = (T)0>
struct S
{
  void
  foo(decltype(t) = t);
};

template<class T, T t>
void
S<T, t>::foo(decltype(t))
{
}

void
bar()
{
  S<int> s;
  s.foo();
}
