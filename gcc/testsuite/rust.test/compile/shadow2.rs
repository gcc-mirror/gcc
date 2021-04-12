fn main() {
    let x = 1;
    let x = x + 1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
