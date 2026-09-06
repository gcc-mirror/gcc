#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

enum FpCategory {
    Zero,
    Subnormal,
    Infinite,
    Nan,
    Normal,
}

pub fn classify(bits: u32) -> FpCategory {
    const EXP_MASK: u32 = 0x7f800000;
    const MAN_MASK: u32 = 0x007fffff;

    match (bits & MAN_MASK, bits & EXP_MASK) {
        (0, 0) => FpCategory::Zero,
        (_, 0) => FpCategory::Subnormal,
        (0, EXP_MASK) => FpCategory::Infinite,
        (_, EXP_MASK) => FpCategory::Nan,
        _ => FpCategory::Normal,
    }
}
