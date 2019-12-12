// { dg-do compile }

template <typename>
struct A {
  void operator= (int);
  template <int> class B;
};
template <typename C>
template <int>
struct A<C>::B : A {
  using A::operator=;
  void operator= (B);
};
