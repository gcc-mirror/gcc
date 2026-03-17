#![feature(no_core)]
#![no_core]

#![feature(lang_items)]

#[lang = "structural_peq"]
pub trait StructuralPartialEq {
    // Empty.
}

#[lang = "structural_teq"]
pub trait StructuralEq {
    // Empty.
}
