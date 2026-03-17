#![feature(no_core)]
#![no_core]

fn test() {
    let a;

    a = if true {
        return;
    } else {
        return;
    };
}

fn main() {
    test();
}
