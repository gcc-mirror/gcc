// PR c++/92966
// { dg-do compile { target c++20 } }

struct S {
  int operator==(const S&) const = default;	// { dg-error "must return 'bool'" }
  int s;					// { dg-message "declared here" "" { target *-*-* } .-1 }
};
static_assert(S{} == S{});			// { dg-error "" }
