#![feature(no_core)]
#![no_core]

mod primitive {
    pub use i32;
}

pub fn foo() -> primitive::i32 {
    1
}
