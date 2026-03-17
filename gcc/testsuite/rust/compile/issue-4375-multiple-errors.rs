#![feature(no_core)]
#![no_core]

fn foo() -> i32 {
    1
}
fn bar() -> i32 {
    2
}
fn baz() -> i32 {
    3
}

fn main() {
    let a = foo() else {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        return ();
    };
    let b = bar() else {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        return ();
    };
    let c = baz() else {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        return ();
    };
}
