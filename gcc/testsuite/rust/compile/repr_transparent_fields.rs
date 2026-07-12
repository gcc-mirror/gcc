#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

#[repr(transparent)]
struct Foo { // { dg-error "transparent struct needs at most one field with non-trivial size or alignment, but has 2" }
    foo: i32,
    bar: i32
}

#[repr(transparent)]
struct Bar {}

#[repr(transparent)]
struct Baz {
    foo: i32
}

#[repr(transparent)]
struct Qux {
    foo: i32,
    phantom: PhantomData<i32>,
}
