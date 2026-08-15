#![feature(no_core)]
#![no_core]

macro_rules! mac_trait {
    ($i:item) => {
        trait T { $i }
    }
}

mac_trait! {
    fn foo() {
    fn foo();
    // { dg-error "free function without a body" "" { target *-*-* } .-1 }
}
}
