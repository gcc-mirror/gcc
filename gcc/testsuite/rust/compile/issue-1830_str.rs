#[rustc_builtin_macro]
macro_rules! include_str {
    () => {{}};
}

fn main() {
    include_str!("");
    // { dg-error {cannot open filename [^\n\r]+: Is a directory} {} { target *-*-* } 0 }
}
