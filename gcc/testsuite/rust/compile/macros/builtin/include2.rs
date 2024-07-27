#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include {
    () => {};
}

fn main() -> i32 {
    let _ = include!("include_rs2");
    0
}
