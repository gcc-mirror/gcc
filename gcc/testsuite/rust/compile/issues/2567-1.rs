// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

enum Empty {}

fn foo(x: Empty) {
    let x: Empty = match x {
        // empty
    };
}
