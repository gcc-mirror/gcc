#![allow(unused)]
fn main() {
enum Dragon {
    Born,
}

fn oblivion() -> Dragon::Born {
// { dg-error "expected type, found variant of Dragon" "" { target *-*-* } .-1 }
// { dg-error "failed to resolve return type" "" { target *-*-* } .-2 }
    Dragon::Born
}

enum Wizard {
    Gandalf,
    Saruman,
}

trait Isengard {
    fn wizard(_: Wizard::Saruman);
    // { dg-error "expected type, found variant of Wizard" "" { target *-*-* } .-1 }
}
}
