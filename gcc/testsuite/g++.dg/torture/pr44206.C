// { dg-do compile }

template<int> struct A
{
  void foo(void(*)(A));
  void bar(void(*f)(A)) { foo(f); foo(f); }
};

template<int N> inline void FOO(A<N> a)
{
  a.foo(0);
}

extern template void FOO(A<0>);

void BAR()
{
  A<0> a;
  FOO(a);
  a.bar(FOO);
}
