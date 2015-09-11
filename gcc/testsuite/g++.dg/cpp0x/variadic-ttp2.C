// PR c++/58734
// { dg-do compile { target c++11 } }

template <typename R1, typename R2>
struct W1 { };

template <template <typename, typename> class W>
struct A { };

template <template <typename, typename> class ...W>
struct B { };

template <template <typename, typename> class ...W>
void f(A<W...> &a, B<W...> &b);

void g()
{
  A<W1> a;
  B<W1> b;

  ::f(a, b);
}
