#![feature(no_core)]
#![no_core]

fn main() {
    let a;
    a = "hello world infer";

    let b: &str;
    b = "hello world specified";
}
