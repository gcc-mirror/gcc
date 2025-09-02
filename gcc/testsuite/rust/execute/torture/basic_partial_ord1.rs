/* { dg-output "less\r*" }*/
mod core {
    mod option {
        pub enum Option<T> {
            None,
            Some(T),
        }
    }

    mod marker {
        #[lang = "phantom_data"]
        pub struct PhantomData<T: ?Sized>;

        #[lang = "structural_peq"]
        pub trait StructuralPartialEq {}

        #[lang = "structural_teq"]
        pub trait StructuralEq {}

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

        pub trait Eq: PartialEq<Self> {
            fn assert_receiver_is_total_eq(&self) {}
        }

        #[lang = "partial_ord"]
        pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
            fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

            fn lt(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Less) => true,
                    _ => false,
                }
            }

            fn le(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Less) | Option::Some(Ordering::Equal) => true,
                    _ => false,
                }
            }

            fn gt(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Greater) => true,
                    _ => false,
                }
            }

            fn ge(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Greater) | Option::Some(Ordering::Equal) => true,
                    _ => false,
                }
            }
        }

        pub trait Ord: Eq + PartialOrd<Self> {
            fn cmp(&self, other: &Self) -> Ordering;
        }
    }
}

use core::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use core::option::Option;

// Needed impls for primitives
impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl PartialOrd for i32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self < *other {
            Option::Some(Ordering::Less)
        } else if *self > *other {
            Option::Some(Ordering::Greater)
        } else {
            Option::Some(Ordering::Equal)
        }
    }

    fn lt(&self, other: &Self) -> bool {
        *self < *other
    }
    fn le(&self, other: &Self) -> bool {
        *self <= *other
    }
    fn ge(&self, other: &Self) -> bool {
        *self >= *other
    }
    fn gt(&self, other: &Self) -> bool {
        *self > *other
    }
}

impl Eq for i32 {}
impl Ord for i32 {
    fn cmp(&self, other: &Self) -> Ordering {
        if *self < *other {
            Ordering::Less
        } else if *self > *other {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

// Manual impl for struct Bar
struct Bar {
    a: i32,
    b: i32,
}

impl PartialEq for Bar {
    fn eq(&self, other: &Self) -> bool {
        self.a.eq(&other.a) && self.b.eq(&other.b)
    }
}

impl Eq for Bar {}

impl PartialOrd for Bar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.a.partial_cmp(&other.a) {
            Option::Some(Ordering::Equal) => self.b.partial_cmp(&other.b),
            ord => ord,
        }
    }
}

impl Ord for Bar {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.a.cmp(&other.a) {
            Ordering::Equal => self.b.cmp(&other.b),
            ord => ord,
        }
    }
}

// External print shim
extern "C" {
    fn puts(s: *const i8);
}

fn print(s: &str) {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

fn main() -> i32 {
    let x = Bar { a: 1, b: 2 };
    let y = Bar { a: 1, b: 3 };

    match x.partial_cmp(&y) {
        Option::Some(Ordering::Less) => print("less"),
        Option::Some(Ordering::Greater) => print("greater"),
        Option::Some(Ordering::Equal) => print("equal"),
        _ => print("none"),
    }

    0
}
