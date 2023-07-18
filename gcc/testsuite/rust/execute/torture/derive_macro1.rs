#[lang = "sized"]
pub trait Sized {}

pub trait Clone {
    fn clone(&self) -> Self;
}

pub trait Copy {}

impl Copy for i32 {}

impl<T> Clone for T
where
    T: Copy,
{
    fn clone(&self) -> Self {
        *self
    }
}

fn main() -> i32 {
    let a = 15i32;
    let b = a.clone();

    a - b
}
