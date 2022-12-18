#![feature(decl_macro)]
macro foo {
    () => [],
    ($h: expr, $(t: expr),*) => ($h),
}
