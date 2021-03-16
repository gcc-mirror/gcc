fn main() {
    let array: [i32; 5] = [1, 2, 3]; // { dg-error "mismatch in array capacity" }
    // { dg-error "expected ..i32:5.. got ..i32:3.." "" { target { *-*-* } } .-1 }
    // { dg-error "failure in setting up let stmt type" "" { target { *-*-* } } .-2 }
}
