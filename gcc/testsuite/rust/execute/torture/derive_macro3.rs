#[lang = "sized"]
pub trait Sized {}

pub trait Clone {
    fn clone(&self) -> Self;
}

impl Clone for i32 {
    fn clone(&self) -> i32 {
        *self
    }
}

#[derive(Clone)]
struct S(i32, i32);

fn main() -> i32 {
    let a = S(15, 15);
    let b = a.clone();

    b.0 - b.1
}
