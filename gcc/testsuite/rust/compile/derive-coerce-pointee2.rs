#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

// check that we error out when the feature is right but the compat mode is missing (opposite of derive-coerce-pointee1.rs)

#[lang = "sized"]
trait Sized {}

#[derive(CoercePointee)]
// { dg-error "derive.CoercePointee. requires a compatibility mode" "" { target *-*-* } .-1 }
// { dg-error "could not resolve trait .CoercePointee." "" { target *-*-* } .-2 }
pub struct Floop<T>(*const T);
