#![feature(no_core)]
#![no_core]

// Rust-GCC/gccrs#4973: a pattern with fields nested in a constructor with
// several fields used to put the sub-patterns and their types in different
// columns, and crashed.

enum Foo {
    Bar(isize),
    Baz(),
}

struct Pair(Foo, Foo);

// This is a valid match
fn f(x: Pair) {
    match x {
        Pair(Foo::Bar(_), Foo::Bar(_)) => {}
        Pair(_, _) => {}
    }
}

fn f2(x: Pair) {
    match x {
        // { dg-error "non-exhaustive patterns: 'Pair . 0: Foo::Bar..., 1: Foo::Bar... .' not covered" "" { target *-*-* } .-1 }
        Pair(Foo::Bar(_), Foo::Baz()) => {}
        Pair(Foo::Baz(), _) => {}
    }
}

fn f3(x: Pair) {
    match x {
        // { dg-error "non-exhaustive patterns: 'Pair . 0: Foo::Baz.., 1: Foo::Baz.. .' not covered" "" { target *-*-* } .-1 }
        Pair(Foo::Baz(), Foo::Bar(_)) => {}
        Pair(Foo::Bar(_), _) => {}
    }
}

fn main() {}
