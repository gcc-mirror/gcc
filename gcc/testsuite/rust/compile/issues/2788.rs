// { dg-additional-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]

struct Foo {
    arg_1: u32,
    arg_2: i32,
}

impl Foo {
    async fn asynchronous_function_1(&self) {}
    async fn asynchronous_function_2() {}
}
