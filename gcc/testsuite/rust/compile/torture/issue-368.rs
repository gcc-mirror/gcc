#[lang = "sized"]
pub trait Sized {}

struct S;

fn foo<S>(s: S) -> S {
    s
}

fn main() {
    let _s: S = foo(S);
}
