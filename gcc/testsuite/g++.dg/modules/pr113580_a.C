// PR c++/113580
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi A }

export module A;

export {
  template <typename T>
  void fun(T x) {}
}
