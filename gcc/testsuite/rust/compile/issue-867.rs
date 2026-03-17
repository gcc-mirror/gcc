#![feature(no_core)]
#![no_core]

fn main() {
    let _ = 42;
    let a = _ + 123; // { dg-error "use of '_' is not allowed on the right-side of an assignment" }
}
