struct Foo {
    a: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: i32,
}

fn foo() -> Foo {
    Foo { a: 42, b: 32 }
}

fn main() {
    let _f = Foo { a: 10, ..foo() };
}
