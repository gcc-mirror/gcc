#![allow(unused)]
fn main() {
    trait Hello {
        type Who;

        fn hello() -> <Self as Hello>::You;
        // { dg-error "cannot find associated type .You. in trait .Hello. .E0576." "" { target *-*-* } .-1 }
        // { dg-error "failed to resolve return type" "" { target *-*-* } .-2 }
    }
}
