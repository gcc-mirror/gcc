// { dg-additional-options "-frust-compile-until=lowering" }
struct Foo {
    arg_1: u32,
    arg_2: i32,
}

impl Foo {
    async fn asynchronous_function_1(&self) {}
    async fn asynchronous_function_2() {}
}
