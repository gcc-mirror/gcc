// { dg-additional-options "-w" }
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
}

fn test() {
    let a = 1..2; // range
    let b = 1..; // range from
    let c = ..3; // range to
    let d = 0..=2; // range inclusive
}
