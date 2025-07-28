// { dg-additional-options "-frust-compile-until=compilation -frust-assume-builtin-offset-of" }

pub struct Foo {
    a: i32,
}

fn main() {
    let _ = offset_of!(Foo, a); // valid
}
