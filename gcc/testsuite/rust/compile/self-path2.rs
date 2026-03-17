// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

struct foo;

fn bar() -> self::foo {
    crate::foo
}

fn baz() {
    let a: foo = self::bar();

    crate::bar();

    crate::self::foo();
    // { dg-error ".self. in paths can only be used in start position" "" { target *-*-* } .-1 }
}

type a = foo;
type b = crate::foo;
type c = self::foo;
type d = crate::self::foo;
// { dg-error ".self. in paths can only be used in start position" "" { target *-*-* } .-1 }
