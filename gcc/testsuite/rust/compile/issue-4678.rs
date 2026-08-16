#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

trait A {}
trait B {}

struct S<T>(T);

impl<T: A> B for S<T> {}

struct Bad;

fn require_b<T: B>(_: T) {}

fn main() {
    let b = S(Bad);
    require_b(b);
    // { dg-error "the trait bound .Bad: A. is not satisfied .E0277." "" { target *-*-* } .-1 }
}
