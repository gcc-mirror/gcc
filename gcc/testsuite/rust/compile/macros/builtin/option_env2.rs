#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! option_env {
    () => {}
}

#[lang = "sized"]
trait Sized {}

pub mod core {
    pub mod option {
        pub enum Option<T> {
            #[lang = "Some"]
            Some(T),
            #[lang = "None"]
            None,
        }
    }
}

use core::option::Option;

fn main() {
    let _: Option<&str> = option_env!(42);
    // { dg-error "argument must be a string literal" "" { target *-*-* } .-1 }
}
