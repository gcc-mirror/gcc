// { dg-excess-errors "...." }
fn main() {
    // { dg-error "unended string literal" "" { target *-*-* } .+1 }
    let s = "123
}
