#![feature(no_core)]
#![no_core]


fn main() {
    let x: &[isize] = &[1, 2, 3];
    let y: &u32 = (&x) as &'static u32; // { dg-error "casting .& & .isize.. as .& u32. is invalid .E0606." }
}