#[lang = "sized"]
pub trait Sized {}

trait A {
    fn get(self) -> f64;
}

trait B {
    fn get(self) -> u8;
}

struct Foo(u8, f64);

impl A for Foo {
    fn get(self) -> f64 {
        self.1
    }
}

impl B for Foo {
    fn get(self) -> u8 {
        self.0
    }
}

fn main() {
    let _a;
    _a = Foo(123, 456f64);

    let _b: f64;
    _b = <Foo as A>::get(_a);

    let _a;
    _a = Foo(123, 456f64);

    let _c: u8;
    _c = <Foo as B>::get(_a)
}
