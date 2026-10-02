// { dg-additional-options "-frust-compat-version=1.84" }

// Check if derive CoercePointee errors out on Union & Enums 

#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

#[lang = "sized"]
trait Sized {}

// user-defined trait
trait MyTrait {}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
// { dg-error "only be derived on structs with '#\\\[repr\\(transparent\\)\\\]'" "" { target *-*-* } .+1 }
enum GenericEnum<T: MyTrait> {
    A(T)
}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
union GenericUnion<T: MyTrait> { // { dg-error "only be derived on structs with '#\\\[repr\\(transparent\\)\\\]'" "" { target *-*-* } }
    a: T,
}
