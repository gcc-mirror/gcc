#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

fn main() {
    include_bytes!("nonexistent.txt"); // { dg-error "cannot open filename (.*?)nonexistent.txt: No such file or directory" }
}
