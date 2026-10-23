#![feature(no_std)]
#![no_std]

pub fn main() {
    let _: crate::core::Titi = ::core::Titi;
    let _: ::core::Titi = crate::core::Titi;
}
