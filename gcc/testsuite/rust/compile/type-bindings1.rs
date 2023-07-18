#[lang = "sized"]
pub trait Sized {}

struct Foo<A, B>(A, B);

fn main() {
    let a;
    a = Foo::<A = i32, B = f32>(123f32);
    // { dg-error "associated type bindings are not allowed here" "" { target *-*-* } .-1 }
    // { dg-error {Failed to resolve expression of function call} "" { target *-*-* } .-2 }
}
