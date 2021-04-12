fn main() {
    let a = 123;
    let b = &a;
    let c = *b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
