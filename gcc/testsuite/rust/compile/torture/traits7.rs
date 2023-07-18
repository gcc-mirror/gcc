#[lang = "sized"]
pub trait Sized {}

trait Foo {
    const A: i32;

    fn test(self);
}

struct Bar;
impl Foo for Bar {
    const A: i32 = 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    fn test(self) {}
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

fn main() {
    let a = Bar;
    a.test();
}
