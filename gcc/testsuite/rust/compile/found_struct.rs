// https://doc.rust-lang.org/error_codes/E0423.html
#![allow(unused)]
fn main() {
    struct Foo {
        a: bool,
    };

    let f = Foo(); // { dg-error "expected function, tuple struct or tuple variant, found struct .Foo." }
                   // error: expected function, tuple struct or tuple variant, found `Foo`
                   // `Foo` is a struct name, but this expression uses it like a function name
}
