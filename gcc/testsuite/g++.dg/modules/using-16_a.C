// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:S }

export module M:S;

namespace foo {
  // propagate hidden from partitions
  export struct S {
    friend void f(S);
  };
};
