#[lang = "sized"]
trait Sized {}

trait Foo {
    fn id(&self) -> u8;
}

struct Bar;

impl Foo for Bar {
    fn id(&self) -> u8 {
        1
    }
}

fn takes(val: impl Foo) -> u8 {
    val.id()
}

fn main() {
    let b = Bar;
    let x = takes::<Bar>(b);
    // { dg-error "cannot provide explicit generic arguments when .impl Trait. is used in argument position .E0632." "" { target *-*-* } .-1 }
}
