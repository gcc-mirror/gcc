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

pub fn foo<T: core::ops::Add<Output = i32>>(a: T) -> i32 {
    a + a
}

pub fn main() {
    foo(123f32);
    // { dg-error "bounds not satisfied for f32 .Add. is not satisfied" "" { target *-*-* } .-1 }
}
