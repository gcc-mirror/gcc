#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! stringify {
    () => {};
}

fn main() {
    let _a = stringify!(sample text with parenthesis () and things! This will become a "string".);
}
