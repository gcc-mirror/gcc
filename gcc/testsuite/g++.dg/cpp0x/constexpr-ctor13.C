// PR c++/55753
// { dg-options -std=c++11 }

struct A
{
  double r,i;
  constexpr A(double r = 0.0, double i = 0.0): r(r), i(i) {}
};

template <typename Tp>
struct B {
  B() {
    A((true ? 1.0 : A()));
  }
};
