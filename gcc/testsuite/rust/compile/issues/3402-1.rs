#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

pub struct Foo {
    a: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}
pub struct Bar(i32);

#[lang = "sized"]
pub trait Sized {}

pub mod core {
    pub mod default {
        use crate::Sized;

        pub trait Default: Sized {
            fn default() -> Self;
        }

        impl Default for i32 {
            fn default() -> Self {
                0
            }
        }
    }
}

impl ::core::default::Default for Bar {
    #[inline]
    fn default() -> Bar {
        Bar(core::default::Default::default())
    }
}
