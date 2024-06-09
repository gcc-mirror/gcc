#[lang = "sized"]
pub trait Sized {}

fn main() {
    bar();
    // { dg-error "type annotations needed" "" { target *-*-* } .-1 }
}

fn bar<T>() {}
