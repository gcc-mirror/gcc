#![feature(no_core)]
#![no_core]

pub trait X {
    fn x() {
        fn f(&mut self) {}
        // { dg-error ".self. parameter is only allowed in associated functions" "" { target *-*-* } .-1 }
        f();
    }
}
