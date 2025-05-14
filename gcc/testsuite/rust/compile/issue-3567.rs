fn main() {
    let _: &[i8] = &[i8];
    // { dg-error "expected value .E0423." "" { target *-*-* } .-1 }
}
