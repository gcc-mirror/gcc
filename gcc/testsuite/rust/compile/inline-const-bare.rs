#![feature(no_core)]
#![no_core]

fn main() {
    const; // { dg-error "unexpected token .;. in some sort of const production" }
}
