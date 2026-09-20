#![feature(no_core)]
#![no_core]

extern "C" {
    fn f1(a: i32, a: i32);
}

trait A {
    fn f2(a: i32, a: i32);
}
