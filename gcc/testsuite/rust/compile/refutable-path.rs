#![feature(no_core)]
#![no_core]
enum E {
    A,
    B,
}
const A: u32 = 0;
struct S;
impl S {
    const C: u32 = 0;
}
fn a(E::A: E) {} // { dg-error "refutable pattern in function parameter" }
fn b(A: u32) {} // { dg-error "refutable pattern in function parameter" }
fn main() {
    let A: u32; // { dg-error "refutable pattern in local binding" }
    let E::A: E; // { dg-error "refutable pattern in local binding" }
    let S::C: u32; // { dg-error "refutable pattern in local binding" }
}
fn f<const N: usize>(x: usize) {
    let N = x; // { dg-error "refutable pattern in local binding" }
}

struct U;
impl U {
    const K: U = U;
}
fn g() {
    let U::K: U; // { dg-error "refutable pattern in local binding" }
}
