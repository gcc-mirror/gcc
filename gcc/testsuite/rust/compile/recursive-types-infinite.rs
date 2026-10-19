// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
trait Sized {}

struct A { b: B } // { dg-error "recursive type .A. has infinite size" }
struct B { a: A }
struct TupleBad((TupleBad,)); // { dg-error "recursive type .TupleBad. has infinite size" }
struct ArrayBad([ArrayBad; 1]); // { dg-error "recursive type .ArrayBad. has infinite size" }
struct Mixed {
    pointer: *const Mixed,
    inline: Mixed, // { dg-error "recursive type .Mixed. has infinite size" }
}
