#![feature(no_core)]
#![no_core]

fn main() {
    let mut a = 1;
    let mut b = 1;

    let mut c;
    while b > 10 {
        c = a + b;
        a = b;
        b = c;
    }
}
