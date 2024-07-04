// P2615R1 valid declarations
// PR c++/107688
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

export {}
export { static_assert(true); }

export namespace Empty {}
export using namespace Empty;

export {
  template <typename T> struct S {};
  template <typename T> struct S<T*> { using a = int; };
  template <> struct S<int*> { using b = int; };
  template struct S<int>;
}

extern "C++" {
  export void foo();
}
