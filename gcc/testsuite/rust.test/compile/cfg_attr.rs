use std::env; // Add one line so gccrs doesn't believe we're parsing a shebang

#[cfg_attr(feature = "somefeature", attribute = "someattr")]
struct Feature;

fn main() {
}
