struct Foo(i32, bool);

impl Foo {
    fn new(a: i32, b: bool) -> Self {
        Self(a, b)
    }
}

fn main() {
    let a;
    a = Foo::new(1, true);
}
