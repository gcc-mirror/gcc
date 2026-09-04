// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

#![feature(rustc_attrs, lang_items)]

#[lang = "sized"]
trait Sized {}

#[lang = "add"]
trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}

#[lang = "partial_ord"]
trait PartialOrd<Rhs: ?Sized = Self> {
    fn lt(&self, other: &Rhs) -> bool;
}

macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;

            #[inline]
            #[rustc_inherit_overflow_checks]
            fn add(self, other: $t) -> $t { self + other }
        }
    )*)
}

macro_rules! partial_ord_impl {
    ($($t:ty)*) => ($(
        impl PartialOrd for $t {
            fn lt(&self, other: &$t) -> bool { *self < *other }
        }
    )*)
}

add_impl! { u8 u16 u32 u64 u128 usize i8 i16 i32 i64 isize }
partial_ord_impl! { u8 u16 u32 u64 u128 usize i8 i16 i32 i64 isize }

fn test(len: usize) -> u64 {
    let mut i = 0;
    let mut out = 0;
    if i + 3 < len {
        out = 123;
    } else {
        out = 456;
    }
    out
}

fn main() -> i32 {
    test(4) as i32 - 123 + test(3) as i32 - 456
}
