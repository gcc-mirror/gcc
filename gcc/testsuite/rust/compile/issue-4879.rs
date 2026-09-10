#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "structural_peq"]
pub trait StructuralPartialEq {}

#[lang = "structural_teq"]
pub trait StructuralEq {}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

pub mod option {
    pub enum Option<T> {
        #[lang = "None"]
        None,
        #[lang = "Some"]
        Some(T),
    }
}

pub use option::Option;

pub mod cmp {
    use crate::Sized;

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

    pub enum Ordering {
        Less = -1,
        Equal = 0,
        Greater = 1,
    }

    #[lang = "partial_ord"]
    pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
        fn partial_cmp(&self, other: &Rhs) -> crate::Option<Ordering>;
    }

    pub trait Ord: Eq + PartialOrd<Self> {
        fn cmp(&self, other: &Self) -> Ordering;
    }
}

use cmp::{Eq, Ord, PartialEq, PartialOrd};

#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub struct Wrapping<T>(pub T);

fn main() -> i32 {
    0
}
