#![feature(no_core)]
#![no_core]
// TODO: remove xfails once parsing of 0.. in fn params and let bindings is implemented.
// { dg-do compile { xfail *-*-* } }
// { dg-excess-errors "parsing of open-ended ranges in patterns not implemented" { xfail *-*-* } }
fn a(0..: u8) {}
fn main() {
    let 0..: u8;
}
