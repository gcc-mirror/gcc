#![feature(no_core)]
#![no_core]

fn get_forty_two() -> i32 {
    return 42;
}

fn main() {
    let myname = get_forty_two() else { return () };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
