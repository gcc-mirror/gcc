#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! format_args {
    () => {};
}

#[lang = "sized"]
trait Sized {}

pub mod core {
    pub mod fmt {
        pub struct Formatter;
        pub struct Result;

        pub struct Arguments<'a>;

        impl<'a> Arguments<'a> {
            pub fn new_v1(_: &'a [&'static str], _: &'a [ArgumentV1<'a>]) -> Arguments<'a> {
                Arguments
            }
        }

        pub struct ArgumentV1<'a>;

        impl<'a> ArgumentV1<'a> {
            pub fn new<'b, T>(_: &'b T, _: fn(&T, &mut Formatter) -> Result) -> ArgumentV1 {
                ArgumentV1
            }
        }

        pub trait Display {
            fn fmt(&self, _: &mut Formatter) -> Result;
        }

        impl Display for i32 {
            fn fmt(&self, _: &mut Formatter) -> Result {
                // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
                Result
            }
        }
    }
}

fn main() {
    let _formatted = format_args!("hello {}", 15);
}
