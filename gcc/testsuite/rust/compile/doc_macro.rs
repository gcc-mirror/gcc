#![feature(rustc_attrs)]
#![feature(no_core)]
#![no_core]
#![doc = concat!("AB")]

// This macro export should not be required for rust 1.49 because inner
// attributes are resolved after the macros, however it is required in newer
// versions of rustc.
#[macro_export]
#[rustc_builtin_macro]
macro_rules! concat {
    () => {{}};
}
