// PR c++/100055
// { dg-do compile { target concepts } }

void foo(auto&& arg) requires({}); // { dg-error "statement-expressions are not allowed|braced-groups" }

template <auto = 0> requires ([]{}()); // { dg-error "expected unqualified-id" }
auto f() requires ([]{}());
