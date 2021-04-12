fn main() {
    let arr: [_; 5] = [1, 2, 3, 4, 5];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
