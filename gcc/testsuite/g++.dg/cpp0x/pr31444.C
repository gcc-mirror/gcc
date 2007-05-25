// { dg-options "-std=gnu++0x" }
template<typename... T> struct A
{
  template<int> void foo(A<T>); // { dg-error "not expanded|T" }
};

void bar()
{
  A<int>().foo<0>(A<int>()); // { dg-error "no member named" }
};
