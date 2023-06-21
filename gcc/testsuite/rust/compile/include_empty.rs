#[rustc_builtin_macro]
macro_rules! include {
    () => {};
}

include!("empty.in");

fn main() {}
