#[lang = "sized"]
pub trait Sized {}

struct Foo<T>(T);

struct Bar<T> {
    a: Foo<T>,
    b: bool,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let a: Bar<i32> = Bar::<i32> {
        a: Foo::<i32>(123),
        b: true,
    };
    let b: i32 = a.a.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
