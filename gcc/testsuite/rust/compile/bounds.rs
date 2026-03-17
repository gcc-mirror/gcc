// { dg-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]

trait Foo {
    type Bar;
}

trait Copy {}

fn c<F: Foo<Bar: Foo>>()
where
    F::Bar: Copy,
{
}
