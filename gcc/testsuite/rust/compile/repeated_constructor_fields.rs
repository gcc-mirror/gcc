struct Foo {
    x: i32,
}

fn main() {
    let x = Foo {
        x: 0,
        x: 0, // { dg-error "field 'x' specified more than once" }
    };
}
