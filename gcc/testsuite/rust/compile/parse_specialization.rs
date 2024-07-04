// { dg-additional-options "-fsyntax-only" }

trait Foo {
    fn bar();
    unsafe fn bar_u();
}

struct S;

impl Foo for S {
    default fn bar() {}
    default unsafe fn bar_u() {}
}
