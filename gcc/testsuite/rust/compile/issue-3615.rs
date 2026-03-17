#![feature(no_core)]
#![no_core]

pub trait Trait {
    pub fn nrvo(init: fn()) -> [u8; 4096] {
        let mut buf = [0; 4096];

        buf
    }
}
