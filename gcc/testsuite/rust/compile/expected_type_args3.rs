#[lang = "sized"]
pub trait Sized {}

struct Foo<A>(A);

impl Foo {
    // { dg-error "generic item takes at least 1 type arguments but 0 were supplied" "" { target *-*-* } .-1 }
    fn test() -> i32 {
        123
    }
}
