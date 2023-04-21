enum Foo {
    A,
    B(i32),
}

fn main() -> i32 {
    let result = Foo::A;

    let value = match result {
        Foo::A => 15,
        Foo::B(x) => x,
    };

    value - 15
}
