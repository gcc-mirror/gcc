#![feature(exclusive_range_pattern)]

fn main() {
    let x = 1.0;

    match x { // { dg-message "sorry, unimplemented: match on floating-point types is not yet supported" }
        -1.0f32..-1.2f32 => 2, // { dg-error "lower range bound must be less than upper .E0579." }
    };
}