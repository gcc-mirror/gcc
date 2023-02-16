// { dg-additional-options "-fsyntax-only" }

trait Foo {
    type A;

    fn foo();
}

struct S;

impl Foo for S {
    type A = ();

    fn foo() {}
}

enum Maybe<T> {
    Something(T),
    Nothing,
}

fn main() {
    let a: Maybe<<S as Foo>::A> = Maybe::Something(());
}
