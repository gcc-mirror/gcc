// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]
fn foo(mut n: i32) {
    if false {
        n = 1i32;
        // { dg-warning "unused assignment .n." "" { target *-*-* } .-1 }
    }

    n = 1i32;
    // { dg-warning "unused assignment .n." "" { target *-*-* } .-1 }

    n = 2i32;
    bar(n);

    if n > 0i32 {
        let _ = 1i32 / n;
    }
    let _ = 1i32 / n;

    let mut n;

    n = 10;
    // { dg-warning "unused assignment .n." "" { target *-*-* } .-1 }

    n = 5;
    if n > 0 {
        let _ = 1i32 / n;
    }
}

fn bar(_n: i32) {}

fn main() {
    foo(1);
}
