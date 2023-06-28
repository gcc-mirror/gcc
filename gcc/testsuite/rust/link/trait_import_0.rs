// { dg-xfail-if "https://github.com/Rust-GCC/gccrs/issues/2349" { *-*-* } }
// { dg-excess-errors "" { xfail *-*-* } }

extern crate trait_import_1;
use trait_import_1::Add;

struct Foo(i32);

impl Add for Foo {
    type Output = Foo;

    fn add(self, other: Foo) -> Foo {
        Foo(self.0 + other.0)
    }
}

fn main() -> i32 {
    let a;
    a = Foo(1) + Foo(2);

    0
}
