// PR c++/71225
// { dg-do compile { target c++11 } }

template <bool, class> struct A;
template <class T> struct B;
template <typename T>
struct C
{
  struct D
  {
    template <int N = 42, typename A<N == 43 || B<T>(), int>::type = 0>
    void foo () const {}
    template <int N = 42, typename A<N == 43 || !B<T> (), int>::type = 0>
    void foo () const {}
    void bar () { foo (); }
  };
};
