struct Foo(i32, bool);

impl Foo {
    const number: i32 = 456;

    fn new(a: i32, b: bool) -> Foo {
        Foo(a, b)
    }

    fn test2() -> i32 {
        test_forward_decl()
    }
}

fn test_forward_decl() -> i32 {
    123
}

fn main() {
    let a;
    a = Foo::new(1, true);

    let b;
    b = Foo::test2();

    let c;
    c = Foo::new(Foo::number, true);
}
