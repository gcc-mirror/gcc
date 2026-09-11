// { dg-warning "unused name 'y'" "" { target *-*-* } 7 }
// { dg-warning "unused name 'z'" "" { target *-*-* } 7 }
#![feature(no_core)]
#![no_core]

fn main() {
    let (ref y, z) = (1i32, 2u32);
}
