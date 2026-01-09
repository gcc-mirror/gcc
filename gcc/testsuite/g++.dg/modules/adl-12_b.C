// { dg-additional-options "-fmodules" }
// Example from [temp.dep.candidate]

namespace Q {
  struct X {};
}
namespace Incomplete {
  struct Z {};
};

import M;

void test(Q::X x) {
  g(x);  // OK
  Partial<Q::X>::f<int>();
  Partial<Q::X>::o<int>();
  incomplete(0);  // OK
  needs_completion(0);  // { dg-bogus "required from here" "PR123235" { xfail *-*-* } }
  // { dg-prune-output "not declared in this scope" }

#if __cpp_impl_three_way_comparison >= 201907L
  rewrite_ops(0);  // OK
  rewrite_ops_error(0);  // { dg-message "required from here" "" { target c++20 } }
  // { dg-prune-output "no match for" }
#endif
}
