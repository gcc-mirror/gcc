#![feature(no_core)]
#![no_core]
#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

pub mod result {
    pub enum Result<T, E> {
        #[lang = "Ok"]
        Ok(T),
        #[lang = "Err"]
        Err(E),
    }
}

mod fmt {
    struct Formatter; // { dg-warning "is never constructed" }
    struct Error; // { dg-warning "is never constructed" }

    type Result = crate::result::Result<(), Error>;

    trait Debug {
        fn fmt(&self, fmt: &mut Formatter) -> Result;
    }
}

#[derive(Debug)]
// { dg-warning "stub implementation" "" { target *-*-* } .-1 }
pub struct Foo {
    pub a: i32,
    pub b: i64,
}

#[derive(Debug)]
// { dg-warning "stub implementation" "" { target *-*-* } .-1 }
struct Bar(i32, i32); // { dg-warning "is never constructed" }

#[derive(Debug)]
// { dg-warning "stub implementation" "" { target *-*-* } .-1 }
enum Baz {
    A,
    B(i32),
    C { a: i32 },
}
