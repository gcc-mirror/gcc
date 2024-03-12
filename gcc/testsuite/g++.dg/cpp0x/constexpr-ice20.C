// PR c++/78645
// { dg-do compile { target c++11 } }

typedef bool (*Function)(int);
constexpr bool check(int x, Function p) { return p(x); }  // { dg-message "in .constexpr. expansion of" }
// { dg-error "not a constant expression" "" { target *-*-* } .-1 }

static_assert(check(2, check), "");  // { dg-error "conversion|constant|in .constexpr. expansion of" }
