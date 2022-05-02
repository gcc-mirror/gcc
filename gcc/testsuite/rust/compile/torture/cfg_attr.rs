mod fake {} // Add one line so gccrs doesn't believe we're parsing a shebang

#[cfg_attr(feature = "somefeature", attribute = "someattr")]
struct Feature;
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

fn main() {}
