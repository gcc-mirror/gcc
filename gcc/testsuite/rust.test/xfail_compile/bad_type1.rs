fn main() {
    let logical: bool = 123; // { dg-error "expected .bool. got .<integer>." }
    // { dg-error "failure in setting up let stmt type" "" { target { *-*-* } } .-1 }
}
