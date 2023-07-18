#[lang = "sized"]
pub trait Sized {}

struct Foo(i32);
trait Bar {
    fn baz(&self);
}

fn main() {
    let a;
    a = Foo(123);

    let b: &dyn Bar = &a;
    // { dg-error "bounds not satisfied for Foo .Bar. is not satisfied" "" { target *-*-* } .-1 }
    // { dg-error "expected" "" { target *-*-* } .-2 }
}
