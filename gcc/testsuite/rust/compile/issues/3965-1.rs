#![feature(no_core)]
#![no_core]

fn main() {
    [(); { continue }];
    // { dg-error ".continue. outside of a loop .E0268." "" { target *-*-* } .-1 }
}
