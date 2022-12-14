
// Checks that we don't try to allocate a 4TB array during compilation
fn foo() -> [u8; 4 * 1024 * 1024 * 1024 * 1024] {
    [0; 4 * 1024 * 1024 * 1024 * 1024]
}

fn main () {
    let x = foo ();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
