fn main() {
    let xs: [i32; 5] = [1, 2, 3, 4, 5];
    let a: bool = xs[0]; // { dg-error "expected .bool. got .i32." }
    // { dg-error "failure in setting up let stmt type" "" { target { *-*-* } } .-1 }
}
