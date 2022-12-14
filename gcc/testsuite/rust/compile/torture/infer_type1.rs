fn main() {
    let array: [_; 2] = [111, 222];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
