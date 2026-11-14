#![feature(no_core)]
#![no_core]

#[lang = "sized"] // { dg-error "lang items are subject to change" }
// { dg-message "add .#!.feature.lang_items... to the crate attributes to enable" "" { target *-*-* } .-1 }
pub trait Sized {}
