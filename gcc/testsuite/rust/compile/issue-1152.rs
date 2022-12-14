fn test() {
    let f = [0; -4_isize];
    // { dg-error "expected .usize. got .isize." "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
    let f = [0_usize; -1_isize];
    // { dg-error "expected .usize. got .isize." "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
