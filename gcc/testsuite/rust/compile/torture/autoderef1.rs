struct Foo(i32, bool);
struct Bar {
    a: i32,
    b: bool,
}

fn main() {
    let a = &Foo(123, false);
    let _b: i32 = a.0;
    let _c: bool = a.1;

    let a = &Bar { a: 456, b: false };
    let _b: i32 = a.a;
    let _c: bool = a.b;
}
