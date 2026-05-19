#![feature(no_core)]
#![no_core]

// this test ICEs because in CompilePatternBindings::visit (HIR::TupleStructPattern &pattern)
// we are missing an implementation handling 'E::T(a, .., b)' with rest patterns
// once that is implemented, this test should pass
// { dg-ice "" }
//
struct S {
    x: i32,
    y: i32,
}
struct T(i32, i32);
enum E {
    T(i32, i32),
}
fn a(S { x, y }: S) {}
fn b(S { x: a, y: b }: S) {}
fn c(S { x: _, y: _ }: S) {}
fn d(T(a, b): T) {}
fn e(T(a, .., b): T) {}
fn g(E::T(a, .., b): E) {}
fn h(E::T(a, b): E) {}
fn main() {
    let S { x, y }: S;
    let S { x: a, y: b }: S;
    let S { x: _, y: _ }: S;
    let T(a, b): T;
    let T(a, .., b): T;
    let E::T(a, .., b): E;
    let E::T(a, b): E;
}
