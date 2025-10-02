// PR c++/117658
// { dg-additional-options "-fmodules" }
// { dg-module-cmi A }

export module A;

export template <typename T> void test(T t) { foo(t); }

namespace ns1 {
  export struct S {};
  /* not exported */ void foo(S) {}
}

namespace ns2 {
  export template <typename T> struct S {
    friend void foo(S) {}
  };
}
