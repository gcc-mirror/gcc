// { dg-options "-w" }
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
            fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;
        }
    }
}

use core::cmp::{Ordering, PartialEq, PartialOrd};
use core::marker::Sized;
use core::option::Option;

impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

impl PartialOrd for i32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Option::Some(Ordering::Equal)
    }
}

struct Foo {
    a: i32,
}

impl PartialEq for Foo {
    fn eq(&self, other: &'_ Self) -> bool {
        ::core::cmp::PartialEq::eq(&self.a, &other.a)
    }
}
