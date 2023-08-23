// { dg-options "-frust-name-resolution-2.0" }

pub mod foo {
    pub macro bar() {}
}

use foo::biz; // { dg-error "unresolved import .foo::biz. .E0433." }

use foo::{bar, baz, biz};
// { dg-error "unresolved import .foo::baz. .E0433." "" { target *-*-* } .-1 }
// { dg-error "unresolved import .foo::biz. .E0433." "" { target *-*-* } .-2 }

fn main() {
    bar!();
}
