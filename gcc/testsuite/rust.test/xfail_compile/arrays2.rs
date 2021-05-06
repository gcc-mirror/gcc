fn main() {
    let array: [i32; 5] = [1, 2, 3];
    // { dg-error "expected ..i32:5.. got ..i32:3.." "" { target *-*-* } .-1 }
}
