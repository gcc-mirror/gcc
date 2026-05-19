#![feature(no_core)]
#![no_core]
struct S {
    x: i32,
    y: i32,
}
struct T(i32, i32);
enum E {
    T(i32, i32),
    U(i32),
}
fn a(S { x, y: 1 }: S) {} // { dg-error "refutable pattern" }
fn b(S { x: 1, y: _ }: S) {} // { dg-error "refutable pattern" }
fn c(T(1, _): T) {} // { dg-error "refutable pattern" }
fn d(T(1, ..): T) {} // { dg-error "refutable pattern" }
fn e(E::T(_, _): E) {} // { dg-error "refutable pattern" }
fn main() {
    let S { x, y: 1 }: S; // { dg-error "refutable pattern" }
    let S { x: 1, y: _ }: S; // { dg-error "refutable pattern" }
    let T(1, _): T; // { dg-error "refutable pattern" }
    let T(1, ..): T; // { dg-error "refutable pattern" }
    let E::T(_, _): E; // { dg-error "refutable pattern" }
}
