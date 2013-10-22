// { dg-options "-std=gnu++11" }

template<int, typename... T> struct A
{
  template<int N> void foo(A<N,T>); // { dg-error "parameter packs|T" }
};

void bar()
{
  A<0,int>().foo(A<0,int>()); // { dg-error "no member named" }
}
