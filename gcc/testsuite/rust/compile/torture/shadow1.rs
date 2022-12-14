fn main() {
    let mut x = 5;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let mut x;
    x = true;
}
