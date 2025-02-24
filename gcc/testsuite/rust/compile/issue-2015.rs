// { dg-additional-options "-frust-compile-until=lowering" }

macro_rules! impl_foo {
	() => { impl Foo }
}

pub trait Foo {}

pub trait Bar {
    type Baz;
}

pub fn foo(_value: impl Bar<Baz = impl_foo!()>) -> i32 {
    15
}

pub fn bar(_value: impl Bar<Baz = impl Foo>) -> i32 {
    16
}
