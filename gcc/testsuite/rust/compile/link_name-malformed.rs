// { dg-options "-w" }
#[link_name] // { dg-error "malformed .link_name. attribute input" }
fn foo() {}

// { dg-note "must be of the form" "" { target *-*-* } .-3 }
