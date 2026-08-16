#![feature(no_core)]
#![no_core]

pub fn f1() {
    return f11();

    // no warning
    fn f11() {}

    f11() // { dg-warning "unreachable expression" }
}

pub fn f2() {
    return;
    f1(); // { dg-warning "unreachable statement" }
}

pub fn f3() {
    return;
    f1() // { dg-warning "unreachable expression" }
}
