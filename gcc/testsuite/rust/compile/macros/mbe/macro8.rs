#![feature(no_core)]
#![no_core]

fn f() {}

macro_rules! expr {
    ($($a:expr)?) => {
        f();
    };
}

fn main() {
    expr!();
    expr!(14);
}
