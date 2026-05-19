#![feature(no_core)]
#![no_core]
enum E {
    A,
    B,
}
fn a((E::A | E::B): E) {} // { dg-bogus {sorry, unimplemented: alt pattern} TODO { xfail *-*-* } }
                          // { dg-ice "" }
fn main() {
    let (E::A | E::B): E;
}
