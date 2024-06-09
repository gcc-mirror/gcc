#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    fn Bar(self) -> i32;
}

struct Baz;
// { dg-warning "struct is never constructed: .Baz." "" { target *-*-* } .-1 }

impl Foo for Baz {
    fn Bar(self) -> i32 {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        123
    }
}

fn main() {}
