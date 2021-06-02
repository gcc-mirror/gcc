fn main() {
    let a = 123;
    let b = a + (a * 2);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
