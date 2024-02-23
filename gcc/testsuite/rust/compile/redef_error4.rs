struct Foo(i32, bool);

impl Foo {
    fn new(a: i32, b: bool) -> Foo {
        Foo(a, b)
    }

    fn test() -> i32 {
        test()
    }

    fn test() -> bool { // { dg-error "defined multiple times" }
        true
    }
}

fn test() -> i32 {
    123
}

fn main() {
    let a;
    a = Foo::new(1, true);

    let b;
    b = Foo::test();
}
