fn main() {
    let a: i32 = 123i32;
    let b: u8 = a as u8;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
