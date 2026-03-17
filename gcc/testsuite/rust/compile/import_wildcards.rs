#![feature(no_core)]
#![no_core]

mod x {}

mod y {}

fn main() {
    use x as _;
    use y as _;
}
