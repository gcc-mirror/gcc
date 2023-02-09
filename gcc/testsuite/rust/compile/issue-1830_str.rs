#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

fn main() {
    include_str!(""); // { dg-excess-errors "Is a directory" }
}
