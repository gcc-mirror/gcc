// PR c++/77598
// { dg-do compile { target c++11 } }

template <typename T>
struct A { static constexpr T a = T (); };
template <typename T>
constexpr T A<T>::a;
struct B
{
  int b;
  constexpr int foo () const { return b; }
  constexpr B (const int &x) : b(x) {};
};
struct C : public B
{
  constexpr C () : B(50) {};
};
struct D : public C
{
};
struct E
{
  static constexpr const auto &e = A<D>::a;
};
constexpr const B &f = E::e;
constexpr const int g = f.foo ();
