// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
trait Sized {}

struct A { next: *const A }
struct B { next: *const B }
fn nominal(a: A) -> B {
    a // { dg-error "mismatched types" "" { target *-*-* } .-1 }
}
struct Node<T> { next: *const Node<T>, value: T }
fn generic(a: Node<i32>) -> Node<u32> {
    a // { dg-error "mismatched types" "" { target *-*-* } .-1 }
}
struct Left<T> { right: *const Right<T>, value: T }
struct Right<T> { left: *const Left<T>, value: T }
fn mutual(a: Left<i32>) -> Left<u32> {
    a // { dg-error "mismatched types" "" { target *-*-* } .-1 }
}
