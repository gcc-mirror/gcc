// Origin: PR c++/46394
// { dg-options "-std=c++0x" }
// { dg-do compile }

template<class T>
struct S0
{
  typedef T type;
};

template<class... X>
struct S1
{
  typedef int I;
};

struct A
{
  template<class...U, class V=typename S1<typename S0<U>::type...>::I>
  A(U...u);
};

int
main()
{
  A a(1, 2);
}
