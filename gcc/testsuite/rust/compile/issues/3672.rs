// { dg-options "-w" }
#![feature(no_core)]
#![no_core]
struct Y(usize, usize);

fn distinct_variant() {
    let y = Y(1, 2);

    let a = match y {
        Y(a, _) => a,
    };
}