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

struct Foo(i32);

impl core::ops::Add for Foo {
    type Output = i32;

    fn add(self, rhs: Foo) -> Self::Output {
        self.0 + rhs.0
    }
}

pub fn bar<T: core::ops::Add<Output = i32>>(a: T) -> i32 {
    a + a
}

pub fn main() -> i32 {
    let a = Foo(1);

    bar(a) - 2
}
