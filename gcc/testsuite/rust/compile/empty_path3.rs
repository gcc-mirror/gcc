#![feature(no_core)]
#![no_core]

// An empty path `::` in pattern position. The parser returned a path with no
// segments without reporting anything, so type checking dereferenced a null
// root type and the compiler crashed instead of diagnosing. Same defect as
// empty_path2.rs, which covers the let initialiser instead of the pattern.
// See Rust-GCC/gccrs#4790.
fn main() {
    let :: = 1;
    // { dg-error "expected identifier" "" { target *-*-* } .-1 }
}
