// { dg-additional-options "-frust-compat-version=1.84" }

// check that we error out when the compat mode is right but the feature is missing

#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[derive(CoercePointee)] // { dg-warning "no effect" }
                         // { dg-error "use of unstable library feature" "" { target *-*-* } .-1 }
struct Floop<T> {
    a: *const T,
}
