#![feature(no_core)]
#![no_core]

type Foo = Foo; // { dg-error "failed to resolve type path segment: .Foo." }

fn u() {
    Foo
}