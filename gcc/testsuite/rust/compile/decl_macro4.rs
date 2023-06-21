#![feature(decl_macro)]
macro foo {
    () => { 0 },
    ($n: expr) => { $n + 1 },
}