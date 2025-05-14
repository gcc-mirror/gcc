// { dg-output "VALUE\r*\nVALUE\r*\n" }
// { dg-set-compiler-env-var ENV_MACRO_TEST "VALUE" }

#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! option_env {
    () => {{}};
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

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print(s: &str) {
    unsafe {
        printf(
            "%s\n" as *const str as *const i8,
            s as *const str as *const i8,
        );
    }
}

macro_rules! env_macro_test {
    () => { "ENV_MACRO_TEST" }
}

fn main() -> i32 {
    let val0: Option<&'static str> = option_env!("ENV_MACRO_TEST");

    
    match val0 {
        Option::None => {},
        Option::Some(s) => {
            print(s);
        }
    }

    //eager expansion test
    let val1: Option<&'static str> = option_env!(env_macro_test!(),);

    match val1 {
        Option::None => {},
        Option::Some(s) => {
            print(s);
        }
    }
    0
}
