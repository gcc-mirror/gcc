struct Foo {
    a: i32,
    b: i32,
}

fn foo() -> Foo {
    Foo { a: 42, b: 32 }
}

fn main() {
    let _f = Foo { a: 10, ..foo() };
}
