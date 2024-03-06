fn foo() {}

enum Foo {
    A,
    B,
}

fn main() {
    let a = Foo::A;

    loop {
        match a {
            Foo::B => foo(),
            Foo::A => break,
        }
    }
}
