// { dg-do assemble  }
template <unsigned X, class T> struct Foo {
  friend void operator<<(int, Foo const &) {}
};
template <unsigned X> class Bar : public Foo<X,int> {};
inline Bar<0> bar(int,int,int) { return Bar<3>(); }  // { dg-error "" } no conversion

