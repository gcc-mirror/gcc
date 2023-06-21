#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

fn main() {
    include_bytes!("");
    // { dg-error {cannot open filename [^\n\r]+: Is a directory} {} { target *-*-* } 0 }
}
