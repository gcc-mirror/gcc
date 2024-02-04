#[lang = "sized"]
pub trait Sized {}

struct S<T>(T);

impl S<u8> {
    fn foo<U>() {}
}

fn main() {
    S::foo::<i32>();
}
