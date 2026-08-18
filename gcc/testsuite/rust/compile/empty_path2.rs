#![feature(no_core)]
#![no_core]

// A path expression with no segments in a let initialiser segfaulted
// during type checking, see Rust-GCC/gccrs#4790.
fn main() {
    let x = ::;
    // { dg-error "expected identifier" "" { target *-*-* } .-1 }
}
