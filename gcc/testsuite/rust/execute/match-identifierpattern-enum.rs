enum Foo {
    I(i32),
}

fn main() -> i32 {
    let x = Foo::I(0);
    let ret = 1;

    match x {
        _ @ Foo::I(b) => { ret = b },
        _ => {},
    };

    ret
}
