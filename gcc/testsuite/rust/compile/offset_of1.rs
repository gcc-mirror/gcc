// { dg-additional-options "-frust-compile-until=lowering -frust-assume-builtin-offset-of" }

pub struct Foo {
    a: i32,
}

fn main() {
    let _ = offset_of!(Foo, a); // valid
    let _ = offset_of!("bloop", a); // { dg-error "could not parse type" }
    let _ = offset_of!(Foo, 15); // { dg-error "could not parse field" }
}
