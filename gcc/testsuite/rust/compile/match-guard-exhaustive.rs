#![feature(no_core)]
#![no_core]

enum T {
    A(),
    B(),
}

fn main() -> i32 {
    let t = T::A();
    match 0 {
        0 if match t {
            // { dg-error "non-exhaustive patterns: 'T::B..' not covered" "" { target *-*-* } .-1 }
            T::A() => true,
        } => 1,
        _ => 0,
    }
}
