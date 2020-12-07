// PR c++/97093
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-diagnostics-depth=3" }

template<class X, X x>
concept C = requires {
    requires (X)x;                     // { dg-message "false" }
  };

template<class X, X x>
concept D = requires {
    requires false || (X)x;                    // { dg-message "false" }
  };

int main() {
  static_assert(C<bool, 0>); // { dg-error "failed" }
  static_assert(D<bool, 0>); // { dg-error "failed" }
}
