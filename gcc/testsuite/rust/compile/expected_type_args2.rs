#[lang = "sized"]
pub trait Sized {}

struct Foo<A>(A);

fn main() {
    let a: Foo = Foo::<i32>(123);
    // { dg-error "generic item takes at least 1 type arguments but 0 were supplied" "" { target *-*-* } .-1 }
}
