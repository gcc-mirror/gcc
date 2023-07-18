#[lang = "sized"]
pub trait Sized {}

mod core {
    mod ops {
        #[lang = "add"]
        pub trait Add<Rhs = Self> {
            type Output;

            fn add(self, rhs: Rhs) -> Self::Output;
        }
    }
}

impl core::ops::Add for i32 {
    type Output = i32;

    fn add(self, rhs: i32) -> Self::Output {
        self + rhs
    }
}

pub fn foo<T: core::ops::Add<Output = i32>>(a: T) -> i32 {
    a + a
}

pub fn main() -> i32 {
    foo(1) - 2
}
