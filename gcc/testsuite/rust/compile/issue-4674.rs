// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

fn foo(mut n: i32) {
    // { dg-warning "function is never used: .foo." "" { target *-*-* } .-1 }
    if false {
        n = 0i32;
    }

    if n > 0i32 {
        let _ = 1i32 / n;
    }
}

