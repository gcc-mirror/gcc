// PR c++/95888
// { dg-do compile { target c++17 } }

template <typename T> class A {
  A(int, int);
  template <typename> friend class A;
  friend T;
};

template<typename U> struct B {
  template<auto V> struct C {
    A<B> begin() { return {1, 0}; }
  };
  template<auto Z, int *P = nullptr>
  C<Z> fn();
};

int
main ()
{
  B<int> b;
  b.fn<1>().begin();
}
