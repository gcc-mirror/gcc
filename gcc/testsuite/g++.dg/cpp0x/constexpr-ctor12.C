// PR c++/55753
// { dg-do compile { target c++11 } }

template <typename Tp>
struct C {
  C() = default;
  constexpr C(const Tp& r) { }
};

template <typename Tp>
struct B {
  B() {
    C<double> cpl = C<double>((true ? 1.0 : C<double>()));
  }
};
