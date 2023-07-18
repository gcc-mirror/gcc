#[lang = "sized"]
pub trait Sized {}

trait Foo {
    fn get(self) -> i32;
}

struct Bar(i32);
impl Foo for Bar {
    fn get(self) -> i32 {
        self.0
    }
}

fn type_bound_test<T: Foo>(a: T) -> i32 {
    Foo::get(a)
}

fn main() {
    let a;
    a = Bar(456);

    let b;
    b = type_bound_test(a);
}
