// { dg-do compile { target c++11 } }

struct A { constexpr operator int() { return 42; } };

template<class T>
struct B {
  static const int versionConst = A();
  enum { versionEnum = versionConst };
};
