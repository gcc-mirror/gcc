#![feature(no_core)]
#![no_core]

#![feature(negative_impls)]

trait ExampleTrait {}

impl !ExampleTrait for i32 {}


fn main() {}
