// PR c++/85462
// { dg-do compile { target c++11 } }

template <class T> struct D { using d = T *; };
template <class, class, class> struct E;
template <class T, class U> struct E<T, U, U> { using d = typename D<T>::d; };
template <class T> struct G { using d = typename E<T, int, int>::d; };
template <class T, class U> typename G<T>::d foo (U);
#define A(n) class A##n {};
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
#define D(n) C(n##0) C(n##1) C(n##2) C(n##3) C(n##4)
D(1)
class H;
template <typename>
struct I
{
  bool bar ();
#undef A
#define A(n) void f##n (A##n *);
D(1)
  void baz ();
};
A1000 v;
template <typename T>
bool I<T>::bar ()
{
#undef A
#define A(n) A##n k##n = *foo<A##n> (v); f##n (&k##n);
D(1)
  foo<H> (v);
  baz ();
  return false;
}
struct J : I<int>
{
  void qux () { bar (); }
};
