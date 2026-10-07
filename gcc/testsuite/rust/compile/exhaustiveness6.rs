#![feature(no_core)]
#![no_core]

// Non-exhaustive matches report E0004, like rustc.

enum E {
    A(),
    B(),
}

fn f(e: E) {
    match e {
        // { dg-error "non-exhaustive patterns: 'E::B..' not covered .E0004." "" { target *-*-* } .-1 }
        E::A() => {}
    }
}

fn main() {}
