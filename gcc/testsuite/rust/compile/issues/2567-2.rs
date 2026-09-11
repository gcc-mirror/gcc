// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

enum Empty {}

fn foo(x: Empty) {
    let x: i32 = match x {
        // empty
    };
}
