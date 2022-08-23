enum Foo {
    A,
    B,
    C(char),
    D { x: i64, y: i64 },
}

fn main() {
    let _a = Foo::A;
    let _b = Foo::B;
    let _c = Foo::C('x');
    let _d = Foo::D { x: 20, y: 80 };
}
