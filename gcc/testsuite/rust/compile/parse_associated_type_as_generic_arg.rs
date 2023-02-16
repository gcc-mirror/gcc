// { dg-additional-options "-fsyntax-only" }

trait Foo {
    type A;

    fn foo();
}

struct S;

impl Foo for S {
    type A = i32;

    fn foo() {}
}

enum Maybe<T> {
    Something(T),
    Nothing,
}

fn foo() -> Maybe<<S as Foo>::A> {
    Maybe::Something(15)
}
