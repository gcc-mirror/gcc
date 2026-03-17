#![feature(no_core)]
#![no_core]

#![feature(decl_macro)]
macro foo {
    () => [],
    ($h: expr, $(t: expr),*) => ($h),
}
