fn foo() {}

enum Foo {
    A,
    B,
}

fn main() {
    let a = Foo::A;

    loop {
        match a {
            Foo::A => break,
            Foo::B => foo(),
        }
    }
}
