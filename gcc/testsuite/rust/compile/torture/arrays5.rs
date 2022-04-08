
// Checks that we don't try to allocate a 4TB array during compilation
fn main () {
    let x = [0; 4 * 1024 * 1024 * 1024 * 1024];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
