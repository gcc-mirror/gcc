#[lang = "sized"]
trait Sized {}

pub trait Value {
    fn get(&self) -> i32;
}

struct Foo(i32);
struct Bar(i32);

impl Value for Foo {
    fn get(&self) -> i32 {
        self.0
    }
}
impl Value for Bar {
    fn get(&self) -> i32 {
        self.0
    }
}

pub fn foo(a: impl Value, b: impl Value) -> i32 {
    a.get() + b.get()
}

fn main() -> i32 {
    let a = Foo(1);
    let b = Bar(2);

    foo(a, b) - 3
}
