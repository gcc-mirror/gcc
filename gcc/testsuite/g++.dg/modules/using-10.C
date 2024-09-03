// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !bad }

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
  }
}

export using s::a1;  // { dg-error "does not have external linkage" }
export using s::b1;  // { dg-error "does not have external linkage" }
export using s::x1;  // { dg-error "does not have external linkage" }
export using s::y1;  // { dg-error "does not have external linkage" }
export using s::f1;  // { dg-error "does not have external linkage" }
export using s::g1;  // { dg-error "does not have external linkage" }

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
}

export using m::a2;  // { dg-error "does not have external linkage" }
export using m::b2;  // { dg-error "does not have external linkage" }
export using m::x2;  // { dg-error "does not have external linkage" }
export using m::y2;  // { dg-error "does not have external linkage" }
export using m::f2;  // { dg-error "does not have external linkage" }
export using m::g2;  // { dg-error "does not have external linkage" }

// no linkage
namespace n {
  using a3 = struct { int x; };  // { dg-message "declared here with no linkage" }

  struct {} tmp_s;  // { dg-message "declared here with no linkage" }
  using b3 = decltype(tmp_s);

  enum {} tmp_e;  // { dg-message "declared here with no linkage" }
  using c3 = decltype(tmp_e);

  auto foo() {
    struct s {};  // { dg-message "declared here with no linkage" }
    return s{};
  }
  using d3 = decltype(foo());
}

export using n::a3;  // { dg-error "does not have external linkage" }
export using n::b3;  // { dg-error "does not have external linkage" }
export using n::c3;  // { dg-error "does not have external linkage" }
export using n::d3;  // { dg-error "does not have external linkage" }

// typedefs
namespace t {
  // aliases have the linkage of the entity they ultimately refer to
  using a = int;
  typedef a b;

  // a template is not an alias
  template <typename T>
  using c = int;  // { dg-message "declared here with module linkage" }

  // anonymous type with typedef name for linkage purposes
  typedef struct {} d;  // { dg-message "declared here with module linkage" }

  // non-empty enum gets linkage of enumerator name
  enum { X } e;  // { dg-message "declared here with module linkage"} 
}

export using t::a;
export using t::b;
export using t::c;  // { dg-error "does not have external linkage" }
export using t::d;  // { dg-error "does not have external linkage" }
export using t::e;  // { dg-error "does not have external linkage" }
