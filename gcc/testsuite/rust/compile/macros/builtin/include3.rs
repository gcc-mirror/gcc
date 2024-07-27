#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include {
    () => {};
}

macro_rules! my_file {
    () => {"include_rs"};
}


include!(my_file!());

fn main() -> i32 {
    b();

    0
}
