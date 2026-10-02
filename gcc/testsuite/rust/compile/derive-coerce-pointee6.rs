// { dg-additional-options "-frust-compat-version=1.84" }
// { dg-prune-output "could not resolve attribute macro invocation 'pointee'" }

// checks for 1 non-generic type for structs with derive CoercePointee having more than 1 generic
// params

#![feature(no_core)]
#![feature(lang_items)]
#![feature(derive_coerce_pointee)]
#![no_core]

#[lang = "sized"]
trait Sized {}

// user-defined trait
trait MyTrait {}

#[lang = "phantom_data"]
pub struct PhantomData<T>;

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
                         // { dg-error "one generic type parameter must be marked as '#\\\[pointee\\\]'" "" { target *-*-* } .+1 }
struct MultipleGenericStruct<T: MyTrait, U> {
    a: *const T,
    b: PhantomData<U>,
}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
struct MultipleMarkedPointee<#[pointee] T: MyTrait, #[pointee] U> { // { dg-error "one generic type parameter must be marked as '#\\\[pointee\\\]'" "" { target *-*-* } }
    a: *const T,
    b: PhantomData<U>,
}

#[repr(transparent)]
#[derive(CoercePointee)] // { dg-warning "no effect" }
struct Good<#[pointee]T: MyTrait, U> {
    a: *const T,
    b: PhantomData<U>,
}
