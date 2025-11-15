#![feature(exclusive_range_pattern)]

fn main() {
    let x = 1.0;

    match x { // { dg-message "sorry, unimplemented: match on floating-point types is not yet supported" }
        -1.2f32..-1.0f32 => 2,
    };
}