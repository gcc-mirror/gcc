// { dg-additional-options "-frust-compat-version=1.50" }
#![feature(rustc_attrs)]
#![feature(lang_items)]
#![feature(no_core)]
#![no_core]

#[rustc_builtin_macro]
macro_rules! concat {
    () => {{}};
}

#[rustc_builtin_macro]
macro_rules! stringify {
    () => {{}};
}

macro_rules! prefix_symbol(
    ($prefix : tt, { $($ident: ident, )* }) => {
        $(
            // { dg-error "arbitrary expressions in key-value attributes are unstable" "" { target *-*-* } .+1 }
            #[export_name = concat!(stringify!($prefix), stringify!($ident))]
            pub extern "C" fn $ident() {
            }
        )*
});

prefix_symbol!(a,{
    __func0,
});
