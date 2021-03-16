// { dg-excess-errors "Noisy error and debug" }
fn main() { // { dg-error "expected .... got .<tyty::error>." }
    let a;
    a = 1;
    break a; // { dg-error "cannot `break` outside of a loop"
    // { dg-error "failed to type resolve expression" "" { target { *-*-* } } .-1 }
}
