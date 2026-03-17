#![feature(no_core)]
#![no_core]

// { dg-warning "function is never used: 'bar'" "" { target *-*-* } .+1 }
fn bar() {
    foo();
}

// { dg-warning "function is never used: 'foo'" "" { target *-*-* } .+1 }
fn foo() {
    bar();
}

fn f() {

}

fn main() {
    f();
}
