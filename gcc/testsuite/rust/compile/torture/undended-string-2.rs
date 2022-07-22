// { dg-excess-errors "...." }
fn main() {
    // { dg-error "unended byte string literal" "" { target *-*-* } .+1 }
    let s = b"123
}
