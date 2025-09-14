// { dg-lto-do assemble }
// { dg-lto-options {{-flto -Wno-return-type}} }

struct a {
  static int b;
};
template <int> struct c {
  struct d {
    d e(unsigned, unsigned, bool, bool, bool, unsigned, a);
  };
};
template <>
c<2>::d c<2>::d::e(unsigned, unsigned, bool, bool, bool, unsigned, const a) { }
