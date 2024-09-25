// { dg-additional-options "-fmodules-ts -std=c++2a" }
// { dg-module-cmi !bad }

export module bad;

namespace s {
  namespace {
    enum e1 { x1 };  // { dg-message "declared here with internal linkage" }
    enum class e2 { x2 };  // { dg-message "declared here with internal linkage" }
  }
}

namespace m {
  enum e3 { x3 };  // { dg-message "declared here with module linkage" }
  enum class e4 { x4 };  // { dg-message "declared here with module linkage" }
}

export using enum s::e1;  // { dg-error "does not have external linkage" }
export using enum s::e2;  // { dg-error "does not have external linkage" }
export using enum m::e3;  // { dg-error "does not have external linkage" }
export using enum m::e4;  // { dg-error "does not have external linkage" }
