#![feature(no_core)]
#![no_core]
fn _c(&_x: &i32) {}
fn main() {
    let &_x = &1; // { dg-bogus {sorry, unimplemented: reference pattern let} TODO { xfail *-*-* } }
}
