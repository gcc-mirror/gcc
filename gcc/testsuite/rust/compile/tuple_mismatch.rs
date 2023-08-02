fn main() {
    let (_,) = 1; // { dg-error "expected <integer>, found tuple" }
    let (_,) = (1, 2); // { dg-error "expected a tuple with 2 elements, found one with 1 element" }
    let (_, _) = (1, 2, 3); // { dg-error "expected a tuple with 3 elements, found one with 2 elements" }
    let (_, _) = (1,); // { dg-error "expected a tuple with 1 element, found one with 2 elements" }
}

// The lhs and rhs sizes don't match, but we still resolve 'a' to be bool, we don't
// error out immediately once we notice the size mismatch.
fn foo() -> i32 { // { dg-error "mismatched types, expected .i32. but got .bool." }
    let (a, _) = (true, 2, 3); // { dg-error "expected a tuple with 3 elements, found one with 2 elements" }
    a
}
