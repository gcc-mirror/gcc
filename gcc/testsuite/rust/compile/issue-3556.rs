fn main() {
    let ref mut a @ (ref mut b,);
    // { dg-error "expected T\\?, found tuple" "" { target *-*-* } .-1 }
}