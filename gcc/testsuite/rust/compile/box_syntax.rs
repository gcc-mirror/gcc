// { dg-options "-fsyntax-only" }
#![feature(box_syntax)]

fn main() {
    let x: Box<_> = box 1;
}
