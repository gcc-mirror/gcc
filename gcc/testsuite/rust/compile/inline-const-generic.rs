#![feature(no_core)]
#![no_core]

fn double<const N: usize>() -> usize {
    const { N * 2 }
}

fn main() {
    double::<3>();
}
