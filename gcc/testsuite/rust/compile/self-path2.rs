// { dg-additional-options "-w" }
struct foo;

fn bar() -> self::foo {
    crate::foo
}

fn baz() {
    let a: foo = self::bar();

    crate::bar();

    crate::self::foo();
    // { dg-error "failed to resolve: .self. in paths can only be used in start position" "" { target *-*-* } .-1 }
}

type a = foo;
type b = crate::foo;
type c = self::foo;
type d = crate::self::foo;
// { dg-error "failed to resolve: .self. in paths can only be used in start position" "" { target *-*-* } .-1 }
