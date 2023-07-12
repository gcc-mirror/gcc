#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

fn main() {
    include_str!("empty_file");
}
