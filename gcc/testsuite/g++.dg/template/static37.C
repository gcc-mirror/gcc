// PR c++/84582

class C {
  static const long b = 0;
  static const unsigned c = (b);
};
class D {
  static const long b = 0;
  static const unsigned c = b;
};
template <class> class A {
  static const long b = 0;
  static const unsigned c = (b);
};
template <class> class B {
  static const long b = 0;
  static const unsigned c = b;
};
