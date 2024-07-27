#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include {
    () => {};
}

macro_rules! my_file {
    () => {"include_rs2"};
}
fn main() -> i32 {
    let _ = include!(my_file!());

    0
}
