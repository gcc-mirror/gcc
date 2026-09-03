#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Iterator {
    type Item;
}

pub trait IntoIterator {
    type IntoIter;
}

pub struct FlattenCompat<I, U>(I, U);

pub struct Flatten<I: Iterator<Item: IntoIterator>> {
    pub inner: FlattenCompat<I, <I::Item as IntoIterator>::IntoIter>,
}

pub trait Foo {
    type Bar;

    fn foo(&self);
}

pub trait Copy {}

pub trait Container<T> {
    type Item;
}

pub fn c<F: Foo<Bar: Foo>>(value: &F::Bar)
where
    F::Bar: Copy,
{
    value.foo();
}

pub fn nested<F: Foo<Bar: Foo>>() {
    let _: *const <F::Bar as Foo>::Bar;
}

pub fn multiple_bounds<F: Foo<Bar: Foo + Copy>>() {
    let _: *const <F::Bar as Foo>::Bar;
}

pub fn positional_and_bound<T, C: Container<T, Item: Foo>>() {
    let _: *const <C::Item as Foo>::Bar;
}
