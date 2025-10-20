#![feature(exclusive_range_pattern)]

fn main() {
    let x = 3;

    match x {
        0..-1 => 2, // { dg-error "lower range bound must be less than upper .E0579." }
        _ => 3,
    };
}
