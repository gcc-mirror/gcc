#[lang = "sized"]
pub trait Sized {}

struct S<T1, T2>(T1, T2);

impl S<i32, i32> {
    fn f<S>(t: S) -> S {
        t
    }
}

pub fn main() {
    S::<i32, i32>::f::<i32>(0);
}
