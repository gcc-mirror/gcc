#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_str {
  () => {{}};
}

macro_rules! file1 {
    () => {
        "builtin_macro_include_str.rs"
    };
}

fn main () {
  include_str!(file1!()); // ok
}
