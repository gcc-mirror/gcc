#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include {
    () => {};
}

include!("include_rs");

fn main() -> i32 {
    b();

    0
}
