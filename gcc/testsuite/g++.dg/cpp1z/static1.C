// PR c++/84582
// { dg-options -std=c++17 }

class C {
  static inline const long b = 0;
  static inline const unsigned c = (b);
};
class D {
  static inline const long b = 0;
  static inline const unsigned c = b;
};
template <class> class A {
  static inline const long b = 0;
  static inline const unsigned c = (b);
};
template <class> class B {
  static inline const long b = 0;
  static inline const unsigned c = b;
};
