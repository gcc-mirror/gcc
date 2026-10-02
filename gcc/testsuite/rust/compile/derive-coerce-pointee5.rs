// { dg-additional-options "-frust-compat-version=1.84" }

// check for validating structs with derive CoercePointee having minimum 1 field.

#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
// { dg-error "only be derived on a struct with at least one field" "" { target *-*-* } .+1 }
struct Bad<T: ?Sized> {}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
// { dg-error "only be derived on structs with .*repr.*transparent" "" { target *-*-* } .+1 }
enum SomeEnum<T: ?Sized> {
    A(*const T),
}
