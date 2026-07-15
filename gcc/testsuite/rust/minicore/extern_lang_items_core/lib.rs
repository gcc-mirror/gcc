#![feature(no_core)]
#![no_core]
#![feature(lang_items)]
#![feature(arbitrary_self_types)]

#[lang = "sized"]
pub trait Sized {}

#[lang = "copy"]
pub trait Copy {}

mod clone {
    #[lang = "clone"]
    trait Clone {
        pub fn clone(&self) -> Self;
    }

    impl Clone for i32 {
        fn clone(&self) -> Self {
            *self
        }
    }
}

impl Copy for i32 {}
