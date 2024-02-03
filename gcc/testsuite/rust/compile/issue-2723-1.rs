#[lang = "sized"]
pub trait Sized {}

struct S<T>(T);

impl S<i32> {
    fn f<S>(t: S) -> S {
        t
    }
}

pub fn main() {
    S::<i32>::f::<i32>(0);
}
