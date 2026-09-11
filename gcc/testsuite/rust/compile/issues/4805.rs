#![no_core]
#![feature(no_core)]
#![feature(min_const_generics)]

trait A {}

impl<const N: usize> A for [u8; N] {}
