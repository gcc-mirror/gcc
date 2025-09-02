pub fn main() {
    const S: usize = 23 as i64;
    // { dg-error {mismatched types, expected .usize. but got .i64.} "" { target *-*-* } .-1 }
    [0; S];
    ()
}
