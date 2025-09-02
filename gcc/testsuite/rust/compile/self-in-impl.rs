// the error message here is what rustc >=1.66 emits
// rustc <1.66 emits a "cycle detected" error when
//   trying to calculate the impl type
//
// since we aren't trying to match error messages too closely
// and the >=1.66 error message is nicer
// we may as well mimic that

impl ((Self, i32)) {}
// { dg-error ".Self. is not valid in the self" "" { target *-*-* } .-1 }

trait Foo {}

impl Foo for ((Self, i32)) {}
// { dg-error ".Self. is not valid in the self" "" { target *-*-* } .-1 }
