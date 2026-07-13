#![feature(no_core, lang_items)]
#![no_core]

#[lang = "alloc_layout"]
pub struct Layout;

#[lang = "oom"]
pub fn _oom() {}
