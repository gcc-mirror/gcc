// P0634R3
// { dg-do compile { target c++20 } }

struct W {
  template<typename T>
  static int fn1 (T::X);
  template<typename T>
  static int fn2 (T::X);
  template<typename T>
  static int fn2 (T::X, int);
};

template<typename T>
int W::fn1 (T::X p) { return p; }

template<typename T>
int W::fn2 (T::X p) { return p; }

template<typename T>
int fn2 (typename T::X p) { return p; }
