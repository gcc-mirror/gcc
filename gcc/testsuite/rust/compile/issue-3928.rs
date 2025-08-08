// { dg-do compile }
// { dg-options "-fsyntax-only" }

#![feature(exclusive_range_pattern)]

fn Foo() {
    let x = 1u32;

    match x {
        3..-1 => 4,
    };
}
