#![feature(no_core)]
#![no_core]

fn tail() -> i32 {
    const { 1 + 1 }
}

fn double<const N: i32>() -> i32 {
    const { N * 2 }
}

fn main() -> i32 {
    const { 3 };
    tail() + double::<3>() - 8
}
