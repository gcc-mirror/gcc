// PR c++/122381
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> struct color_ref {
  operator int() const { return 0; }
  int foo(color_ref x) {
    return x.operator int();
  }
};
