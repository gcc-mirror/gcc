#![feature(no_core)]
#![feature(lang_items)]
#![feature(rustc_attrs)]
#![no_core]

#[rustfmt::skip]
pub fn foo() {}

#[clippy::something::useful]
#[diagnostic::diagnose_this]
struct Boo;

#[miri::save_us_all]
unsafe fn scary() {}

#[rust_analyzer::vade_retro]
unsafe fn stannanas() {}

#[not_a_tool::but_still_tool_like]
// { dg-error "macro not found" "" { target *-*-* } .-1 }
// { dg-error "could not resolve attribute macro invocation" "" { target *-*-* } .-2 }
pub fn ha_exclam() {}
