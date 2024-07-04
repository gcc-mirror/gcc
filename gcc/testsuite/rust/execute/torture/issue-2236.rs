// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

mod core {
    mod ops {
        #[lang = "deref"]
        trait Deref {
            type Target;
            fn deref(&self) -> &Self::Target;
        }

        impl<T> Deref for &T {
            type Target = T;

            fn deref(&self) -> &T {
                *self
            }
        }
    }
}

impl i32 {
    fn max(self, other: i32) -> i32 {
        if self > other {
            self
        } else {
            other
        }
    }
}

fn foo<T: core::ops::Deref<Target = i32>>(t: T) -> i32 {
    t.max(2)
}

fn main() -> i32 {
    let a: i32 = 1;
    foo(&a) - 2
}
