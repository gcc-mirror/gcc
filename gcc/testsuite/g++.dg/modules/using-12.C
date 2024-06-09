// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !bad }

// Like using-10.C, but test exporting names within the same namespace.

export module bad;

// internal linkage
namespace s {
  namespace {
    struct a1 {};  // { dg-message "declared here with internal linkage" }

    template <typename T>
    struct b1;  // { dg-message "declared here with internal linkage" }

    int x1;  // { dg-message "declared here with internal linkage" }

    template <typename T>
    T y1;  // { dg-message "declared here with internal linkage" }

    void f1();  // { dg-message "declared here with internal linkage" }

    template <typename T>
    void g1();  // { dg-message "declared here with internal linkage" }

    export using s::a1;  // { dg-error "does not have external linkage" }
    export using s::b1;  // { dg-error "does not have external linkage" }
    export using s::x1;  // { dg-error "does not have external linkage" }
    export using s::y1;  // { dg-error "does not have external linkage" }
    export using s::f1;  // { dg-error "does not have external linkage" }
    export using s::g1;  // { dg-error "does not have external linkage" }
  }
}

// module linkage
namespace m {
  struct a2 {};  // { dg-message "declared here with module linkage" }

  template <typename T>
  struct b2;  // { dg-message "declared here with module linkage" }

  int x2;  // { dg-message "declared here with module linkage" }

  template <typename T>
  T y2;  // { dg-message "declared here with module linkage" }

  void f2();  // { dg-message "declared here with module linkage" }

  template <typename T>
  void g2();  // { dg-message "declared here with module linkage" }

  export using m::a2;  // { dg-error "does not have external linkage" }
  export using m::b2;  // { dg-error "does not have external linkage" }
  export using m::x2;  // { dg-error "does not have external linkage" }
  export using m::y2;  // { dg-error "does not have external linkage" }
  export using m::f2;  // { dg-error "does not have external linkage" }
  export using m::g2;  // { dg-error "does not have external linkage" }
}

namespace t {
  using a = int;  // { dg-message "declared here with no linkage" }

  template <typename T>
  using b = int;  // { dg-message "declared here with no linkage" }

  typedef int c;  // { dg-message "declared here with no linkage" }

  export using t::a;  // { dg-error "does not have external linkage" }
  export using t::b;  // { dg-error "does not have external linkage" }
  export using t::c;  // { dg-error "does not have external linkage" }
}

// { dg-prune-output "not writing module" }
