// { dg-additional-options "-frust-compat-version=1.84" }

// this time everything is enabled - check we fake name resolve properly and we have access to the derive

#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[derive(CoercePointee)] // { dg-warning "no effect" }
pub struct Floop<T>(*const T);
