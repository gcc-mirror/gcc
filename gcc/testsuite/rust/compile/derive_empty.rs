#![feature(no_core)]
#![no_core]

#[derive()]
struct UnderivedStruct;

fn main() {
    let _ = UnderivedStruct;
}
