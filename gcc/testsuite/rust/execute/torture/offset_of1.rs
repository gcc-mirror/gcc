// { dg-do run { target x86_64*-*-* } }
// { dg-additional-options "-frust-assume-builtin-offset-of" }

pub struct Foo {
    pub a: i32,
    pub b: i32,
}

fn main() -> i32 {
    let a = offset_of!(Foo, a); // valid
    let b = offset_of!(Foo, b); // valid

    let res = a + b - 4;

    res as i32
}
