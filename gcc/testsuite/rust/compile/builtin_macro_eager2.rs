#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! env {
    () => {};
}

macro_rules! a {
    () => {
        "__undefined__"
    };
}

fn main() {
    let _ = env!(a!()); // { dg-error "environment variable .__undefined__. not defined" }
    let _ = env!(a!(), "custom"); // { dg-error "custom" }
    let _ = env!(a!(), a!()); // { dg-error "__undefined__" }
}
