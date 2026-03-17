#![feature(no_core)]
#![no_core]

macro_rules! foo {
    ($a:ident, $a:ident) => {0} // { dg-error "duplicate matcher binding" }
}

fn main() {
    foo!(a, b);
}
