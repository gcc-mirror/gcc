#[lang = "sized"]
pub trait Sized {}

trait Trait {
    const FOO: usize;
    type Target;
}

struct S;
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl Trait for S {
    const FOO: usize = 0;
    type Target = usize;
}

fn main() {
    let a: <S as Trait>::Target;
    a = <S as Trait>::FOO;
}
