// PR c++/114917
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

export namespace ns {
  template <typename T> struct S {};
  template <typename T> struct S<T*> { using a = int; };
  template <> struct S<int*> { using b = int; };
  template struct S<int>;
};

export extern "C++" namespace ns {
  template <typename T> void foo() {}
  template <> void foo<int>() {}
}
