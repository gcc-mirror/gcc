// { dg-additional-options "-w" }
struct foo;

fn bar() -> self::foo {
    crate::foo
}

fn baz() {
    let a: foo = self::bar();

    crate::bar();

    crate::self::foo();
    // { dg-error "leading path segment .self. can only be used at the beginning of a path" "" { target *-*-* } .-1 }
}

type a = foo;
type b = crate::foo;
type c = self::foo;
type d = crate::self::foo;
// { dg-error "leading path segment .self. can only be used at the beginning of a path" "" { target *-*-* } .-1 }
