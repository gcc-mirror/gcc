#![feature(exclusive_range_pattern)]

fn main() -> i32 {
    let x = -77;

    match x {
        -55..99 => 1,
        -99..-55 => 0, // the correct case
        _ => 1,
    }
}