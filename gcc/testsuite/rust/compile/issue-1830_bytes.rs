#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

fn main() {
    include_bytes!(""); // { dg-excess-errors "Is a directory" }
}
