enum Foo {
    I(i32),
}

fn main() {
    let x = Foo::I(1);

    match x {
        a @ Foo::I(b) => {},
        _ => {},
    };
}
