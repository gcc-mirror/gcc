// { dg-additional-options "-frust-compat-version=1.84" }

// check for repr transparent for structs with derive CoercePointee

#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[derive(CoercePointee)] // { dg-warning "no effect" }
                         // { dg-error "only applicable to struct/tuple with repr.transparent. layout" "" { target *-*-* } .+1 }
struct Bad<T: ?Sized> {
    a: *const T,
}
