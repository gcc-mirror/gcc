#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

fn main() {
    include_str!("nonexistent.txt"); // { dg-error "cannot open filename (.*?)nonexistent.txt: No such file or directory" }
}
