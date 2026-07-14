// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "RangeFull"]
pub struct RangeFull;

#[lang = "Range"]
pub struct Range<Idx> {
    pub start: Idx,
    pub end: Idx,
}

#[lang = "RangeFrom"]
pub struct RangeFrom<Idx> {
    pub start: Idx,
}

#[lang = "RangeTo"]
pub struct RangeTo<Idx> {
    pub end: Idx,
}

#[lang = "RangeInclusive"]
pub struct RangeInclusive<Idx> {
    pub start: Idx,
    pub end: Idx,
    pub exhausted: bool,
}

impl<Idx> RangeInclusive<Idx> {
    #[lang = "range_inclusive_new"]
    pub const fn new(start: Idx, end: Idx) -> Self {
        Self {
            start,
            end,
            exhausted: false,
        }
    }
}

fn test() {
    let a = 1..2; // range
    let b = 1..; // range from
    let c = ..3; // range to
    let d = 0..=2; // range inclusive
}
