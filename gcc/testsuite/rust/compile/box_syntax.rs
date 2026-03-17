// { dg-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]

#![feature(box_syntax)]

fn main() {
    let x: Box<_> = box 1;
}
