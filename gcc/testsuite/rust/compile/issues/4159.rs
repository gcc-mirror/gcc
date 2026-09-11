#![feature(no_core)]
#![no_core]
#![feature()]
macro_rules! m {
    () => ( c.. ); // { dg-error "token type .* cannot be parsed as range pattern bound" }
}
fn main() {
    match [1, 1, 2] {
        [ c1 ] => {}
        [ m!() ] => {}
    }
}