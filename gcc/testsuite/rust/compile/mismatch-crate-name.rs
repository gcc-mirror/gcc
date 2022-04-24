// { dg-additional-options "-frust-crate=another_name" }
#![crate_name = "legit_name"]
// { dg-error ".-frust-crate-name. and .#.crate_name.. are required to match, but .another_name. does not match .legit_name." "" { target *-*-* } .-1 }
fn main() {}
