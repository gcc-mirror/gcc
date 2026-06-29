// { dg-additional-options "-frust-compat-version=1.50" }
#![feature(no_core)]
#![no_core]
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! concat {
    () => {{}};
}

#[rustc_builtin_macro]
macro_rules! stringify {
    () => {{}};
}

// { dg-error "arbitrary expressions in key-value attributes are unstable" "" { target *-*-* } .+1 }
#[export_name = concat!(stringify!(non), stringify!(literal))]
pub extern "C" fn attribute_test_function() {}
