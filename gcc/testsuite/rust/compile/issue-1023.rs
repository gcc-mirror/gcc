// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn foo(e: &str) -> &str {
    &""
}
