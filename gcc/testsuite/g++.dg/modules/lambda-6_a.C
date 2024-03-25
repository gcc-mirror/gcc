// PR c++/107398
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi Lambda6 }

export module Lambda6;

template <typename T>
struct R { static int x; };

template <typename T>
int R<T>::x = []{int i; return 1;}();

export int foo();
int foo() {
  return R<int>::x;
}
