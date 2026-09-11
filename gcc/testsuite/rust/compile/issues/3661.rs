#![feature(no_core)]
#![no_core]
#![feature(extended_key_value_attributes)]
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! stringify {
    () => {{}};
}

pub macro m($inner_str:expr) {
    #[m = $inner_str]
    // { dg-error "macro not found" "" { target *-*-* } .-1 }

    struct S;
}

fn main() {
    m!(stringify!(foo));
}
