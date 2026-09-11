#![feature(no_core)]
#![no_core]

#![allow(unused)]
fn main() {
enum Dragon {
    Born,
}

fn oblivion() -> Dragon::Born {
// { dg-error "expected type, found variant of .Dragon::Born." "" { target *-*-* } .-1 }
    Dragon::Born
}

enum Wizard {
    Gandalf,
    Saruman,
}

trait Isengard {
    fn wizard(_: Wizard::Saruman);
    // { dg-error "expected type, found variant of .Wizard::Saruman." "" { target *-*-* } .-1 }
}
}
