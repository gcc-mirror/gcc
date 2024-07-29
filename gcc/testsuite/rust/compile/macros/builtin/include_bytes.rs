#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

fn main() {
    let file = "include.txt";
    include_bytes!(file); // { dg-error "argument must be a string literal" "" }
    include_bytes!(); // { dg-error "macro takes 1 argument" "" }
    include_bytes!("foo.txt", "bar.txt"); // { dg-error "macro takes 1 argument" "" }
    include_bytes!("include_bytes.rs"); // ok
    include_bytes!("include_bytes.rs",); // trailing comma ok
}
