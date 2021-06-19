fn main() {
    let a;
    a = 1;
    break a; // { dg-error "cannot 'break' outside of a loop" }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-1 }
}
