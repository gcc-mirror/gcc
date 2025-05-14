#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait A<T: ?Sized> {}

struct Cell<X> {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    x: X,
}

impl<T, U> A<Cell<U>> for Cell<T> where T: A<U> {}
