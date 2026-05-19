// TODO: remove dg-ice once range refutability is implemented.
// Once implemented, this should emit E0005 instead of ICEing.
#![feature(no_core)]
#![no_core]
// { dg-ice "" }
fn a(1..=5: i32) {} // { dg-bogus {sorry, unimplemented: range pattern} "catch E0005 once implemented" { xfail *-*-* } }
fn main() {
    let 1..=5: i32;
}
