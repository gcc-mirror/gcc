#![feature(no_core)]
#![no_core]

fn main() {
    let rust = "crab";
    let res = loop {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        break rust;
    };
}
