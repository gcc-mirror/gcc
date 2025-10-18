fn main() {
    match (1, 2.2, "not 3") {
        // { dg-error "expected a tuple with 3 elements, found one with 5 elements" "" { target *-*-* } .+1 }
        (a, b, .., c, d, e) => {
            let _ = b + c; // { dg-error "cannot apply operator .+. to types <float> and & str" }
        }
    }
}