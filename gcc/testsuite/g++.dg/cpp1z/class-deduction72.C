// PR c++/91759
// { dg-do compile { target c++17 } }

namespace N {
  template <typename T>
  struct X{ X(int); };	// { dg-message "declared here" }
}

using N::X;

X(int) -> X<int>;	// { dg-error "must be declared in the same scope as" }
