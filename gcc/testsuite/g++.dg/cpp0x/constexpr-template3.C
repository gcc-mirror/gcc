// { dg-options -std=c++0x }

struct A { constexpr operator int() { return 42; } };

template<class T>
struct B {
  static const int versionConst = A();
  enum { versionEnum = versionConst };
};
