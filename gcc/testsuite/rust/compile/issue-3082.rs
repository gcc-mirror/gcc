#![allow(unused)]
fn main() {
    trait Hello {
        type Who;

        fn hello() -> <i32>::You;
        // { dg-error "failed to resolve return type" "" { target *-*-* } .-1 }
    }
}
