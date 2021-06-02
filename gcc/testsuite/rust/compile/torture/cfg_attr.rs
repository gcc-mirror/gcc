use std::env; // Add one line so gccrs doesn't believe we're parsing a shebang

#[cfg_attr(feature = "somefeature", attribute = "someattr")]
struct Feature;
// { dg-warning "unused name" "" { target *-*-* } .-1 }

fn main() {
}
