// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

export struct A {
  static int f() { return -1; }
  static inline int x = f();
};
