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
}
