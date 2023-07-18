// https://doc.rust-lang.org/error_codes/E0323.html
#![allow(unused)]
fn main() {
trait Foo {
    type N;
}

struct Bar;

impl Foo for Bar {
    const N : u32 = 0; // { dg-error "item .N. is an associated const, which does not match its trait .Foo." }
    // error: item `N` is an associated const, which doesn't match its
    //        trait `<Bar as Foo>`
}
}