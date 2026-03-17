#![feature(no_core)]
#![no_core]

fn get_tuple() -> (i32, i32) {
    (5, 10)
}

fn main() {
    let (x, y) = get_tuple() else {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        return;
    };
}
