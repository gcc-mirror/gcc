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

  // This should error, but lookup_qualified_name in add_candidates
  // doesn't look in the instantiation context of this call, so
  // we don't see the operator!= and think we can validly rewrite.
  rewrite_ops_error(0);  // { dg-error "required from here" "PR122609" { xfail *-*-* } }
#endif
}
