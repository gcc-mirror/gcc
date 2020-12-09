struct Foo {
    one: i32,
    two: i32,
}

impl Foo {
    fn new(a: i32, b: i32) -> Foo {
        return Foo { one: a, two: b };
    }
}

fn main() {
    let cake = Foo::new(3, 4);
}
