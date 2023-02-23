// { dg-additional-options "-fsyntax-only" }

trait Foo {
    fn bar();
}

struct S;

impl Foo for S {
    default fn bar() {}
}
