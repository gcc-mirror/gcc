// PR c++/122699
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
export namespace ns::inner {
  template <typename T> struct S {
    inline friend void f();
  };
  void f();
}
