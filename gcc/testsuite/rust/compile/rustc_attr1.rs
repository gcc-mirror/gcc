// { dg-additional-options "-w" }

#![feature(rustc_attrs)]

pub struct NotI8(i8);

impl NotI8 {
    #[inline]
    #[rustc_inherit_overflow_checks]
    pub fn add(self, other: NotI8) -> NotI8 {
        NotI8(self.0 + other.0)
    }
}
