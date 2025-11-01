// Before P1787 (C++20), only hidden friends are included in ADL.
// After P1787, all friends are included.

// { dg-additional-options "-fmodules -Wno-global-module" }

export module M;

namespace N {
  namespace NN {
    export struct A;
  }
  export using NN::A;

  export void fn (A);

  namespace NN {
    struct A {
      friend void N::fn (A);
    };
  }
}
