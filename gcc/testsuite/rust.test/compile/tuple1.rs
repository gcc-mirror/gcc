fn main() {
    let a: (i32, bool) = (123, true);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b;
    b = (456, 5f32);
}
