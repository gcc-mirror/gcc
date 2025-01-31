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
    // Both a guaranteed-to-exist variable and a failed find should compile
    let _: Option<&str> = option_env!("PWD");
    let _: Option<&str> = option_env!("PROBABLY_DOESNT_EXIST");
}
