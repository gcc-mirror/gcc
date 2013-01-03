// PR c++/55753
// { dg-options -std=c++11 }

template <typename Tp>
struct C {
  constexpr C(const Tp& r) { }
};

template <typename Tp>
struct B {
  B() {
    C<double> cpl = C<double>((true ? 1.0 : C<double>()));
  }
};
