#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

#[derive(Clone)]
struct Foo {
    a: i32,
}

#[derive(Clone)]
struct S {
    a: i32,
    b: Foo,
}

impl Clone for i32 {
    fn clone(&self) -> Self { *self }
}

fn main() -> i32 {
    let s1 = S { a: 15, b: Foo { a: 14 }};
    let s2 = s1.clone();

    let l = s1.a - s2.a;
    let r = s1.b.a - s2.b.a;

    // should be 0 if all fields were cloned correctly
    l + r
}
