#![feature(exclusive_range_pattern)]

fn main() {
    let x = 1;

    match x {
        -55..0 => 2,
        -99..-55 => 3,
    };
}