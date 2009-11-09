// PR c++/41972

struct X {
  static const double  x;
};
template <const double& _test_>
  class Foo { };
template <typename _ignore_>
struct Y {
  typedef Foo<X::x> type;
};

