#![no_std]

#[derive(Clone)]
struct MyStruct {
    inner: i32,
}

fn main() -> i32 {
    let a: MyStruct = MyStruct { inner: 8 };
    a.inner
}
