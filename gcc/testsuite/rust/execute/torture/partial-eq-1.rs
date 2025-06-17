/* { dg-output "a == b\r*\na != c\r*\n" }*/
/* { dg-options "-w" } */

mod core {
    mod marker {
        #[lang = "phantom_data"]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub struct PhantomData<T: ?Sized>;

        #[unstable(feature = "structural_match", issue = "31434")]
        #[lang = "structural_peq"]
        pub trait StructuralPartialEq {
            // Empty.
        }

        #[unstable(feature = "structural_match", issue = "31434")]
        #[lang = "structural_teq"]
        pub trait StructuralEq {
            // Empty.
        }

        #[lang = "sized"]
        pub trait Sized {}
    }

    pub mod cmp {
        use super::marker::Sized;

        #[lang = "eq"]
        pub trait PartialEq<Rhs: ?Sized = Self> {
            fn eq(&self, other: &Rhs) -> bool;

            fn ne(&self, other: &Rhs) -> bool {
                !self.eq(other)
            }
        }

        pub trait Eq: PartialEq<Self> {
            fn assert_receiver_is_total_eq(&self) {}
        }
    }
}

use core::cmp::{Eq, PartialEq};

// PartialEq for i32 and u32 so we can compare across types
impl PartialEq<u32> for i32 {
    fn eq(&self, other: &u32) -> bool {
        *self >= 0 && (*self as u32) == *other
    }
}
impl PartialEq<i32> for u32 {
    fn eq(&self, other: &i32) -> bool {
        *other >= 0 && *self == *other as u32
    }
}

// Our generic struct
struct Foo<T> {
    value: T,
}

// Manual impl of PartialEq for different generic params
impl<T, U> PartialEq<Foo<U>> for Foo<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Foo<U>) -> bool {
        self.value.eq(&other.value)
    }
}

impl<T: PartialEq> Eq for Foo<T> {}

extern "C" {
    fn puts(s: *const i8);
}

fn print(s: &str) {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

fn main() -> i32 {
    let a = Foo { value: 42i32 };
    let b = Foo { value: 42u32 };
    let c = Foo { value: 7u32 };

    if a == b {
        print("a == b");
    } else {
        print("a != b");
    }

    if a == c {
        print("a == c");
    } else {
        print("a != c");
    }

    0
}
