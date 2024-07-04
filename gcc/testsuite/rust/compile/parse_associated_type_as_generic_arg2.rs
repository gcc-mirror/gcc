#[lang = "sized"]
pub trait Sized {}

trait Foo {
    type A;

    fn foo();
}

struct S; // { dg-warning "struct is never constructed" }

impl Foo for S {
    type A = ();

    fn foo() {}
}

enum Maybe<T> {
    Something(T),
    Nothing,
}

pub fn test() {
    let _a: Maybe<<S as Foo>::A> = Maybe::Something(());
}
