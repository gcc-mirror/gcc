#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

macro_rules! my_file {
    () => {"include.txt"}
}

static G_STR:[u8;16] = include_str!(my_file!());

fn main() {
    let file = "include.txt";
    include_str!(file); // { dg-error "argument must be a string literal" "" }
    include_str!(); // { dg-error "macro takes 1 argument" "" }
    include_str!("foo.txt", "bar.txt"); // { dg-error "macro takes 1 argument" "" }
    include_str!("include_str.rs"); // ok
    include_str!("include_str.rs",); // trailing comma ok
    include_str!("invalid_utf8"); // { dg-error "invalid_utf8 was not a valid utf-8 file" "" }
    include_str!(my_file!());
    include_str!(my_file!(),);
}
