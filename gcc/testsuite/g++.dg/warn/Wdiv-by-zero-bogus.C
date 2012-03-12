// PR c++/52299

template<unsigned x>
struct test0 {
  static const unsigned a_
  = x ? 10 / x : 10;
};

template<unsigned x>
struct test1 {
  static const unsigned a_
  = !x ? 10 : 10 / x;
};

template<bool x>
struct test2 {
  static const unsigned a_
  = x ? 10 / x : 10;
};

template<bool x>
struct test3 {
  static const unsigned a_
  = !x ? 10 : 10 / x;
};

unsigned i0 = test0<0>::a_;
unsigned i1 = test1<0>::a_;
unsigned i2 = test2<false>::a_;
unsigned i3 = test3<false>::a_;
