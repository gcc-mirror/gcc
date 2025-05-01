enum Foo {
    A { x: i32 },
    B { y: i32 }
}

fn main() -> i32 {
    let x = Foo::A { x: 12 };
    match x {
        Foo::A { x: 10 } => 1,
        Foo::B { y: 11 } => 2,
        Foo::A { x: abc } => { abc - 12 }
    }
}
