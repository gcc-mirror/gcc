/* { dg-output "a == b\r*\na != c\r*\n" }*/
/* { dg-options "-w" } */

#![feature(intrinsics)]

mod core {
    mod option {
        pub enum Option<T> {
            #[lang = "None"]
            None,
            #[lang = "Some"]
            Some(T),
        }
    }

    mod marker {
        #[lang = "sized"]
        pub trait Sized {}
    }

    mod cmp {
        use super::marker::Sized;
        use super::option::Option;

        pub enum Ordering {
            Less = -1,
            Equal = 0,
            Greater = 1,
        }

        #[lang = "eq"]
        pub trait PartialEq<Rhs: ?Sized = Self> {
            fn eq(&self, other: &Rhs) -> bool;

            fn ne(&self, other: &Rhs) -> bool {
                !self.eq(other)
            }
        }

        #[lang = "partial_ord"]
        pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;
        }
    }
}

use core::cmp::{Ordering, PartialEq, PartialOrd};
use core::marker::Sized;
use core::option::Option;

impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl PartialOrd for i32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self > *other {
            Option::Some(Ordering::Greater)
        } else if *self < *other {
            Option::Some(Ordering::Less)
        } else {
            Option::Some(Ordering::Equal)
        }
    }
}

struct Foo {
    a: i32,
}

impl PartialEq for Foo {
    fn eq(&self, other: &'_ Self) -> bool {
        self.a == other.a
    }
}

impl PartialOrd for Foo {
    fn partial_cmp(&self, other: &'_ Foo) -> Option<::core::cmp::Ordering> {
        ::core::cmp::PartialOrd::partial_cmp(&self.a, &other.a)
    }
}

extern "C" {
    fn puts(s: *const i8);
}

fn print(s: &str) {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

fn main() -> i32 {
    let a = Foo { a: 42i32 };
    let b = Foo { a: 42i32 };
    let c = Foo { a: 7i32 };

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
