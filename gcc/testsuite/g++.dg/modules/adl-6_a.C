// PR c++/117658
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

namespace R {
  export struct X {};
  export void f(X);
}

namespace S {
  export void f(R::X, R::X);
}

namespace I {
  inline namespace A {
    export struct Y {};
  }
  inline namespace B {
    export void f(Y);
    export extern "C++" void f(Y, int);
  }
  inline namespace C {
    export struct f {} f;
  }
}

namespace O {
  export void g(I::Y);
  export extern "C++" void h(I::Y);
}
namespace I {
  inline namespace D {
    export using O::g;
    export using O::h;
  }
}
