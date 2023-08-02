fn test() {
    let f = [0; -4_isize];
    // { dg-error "mismatched types, expected .usize. but got .isize." "" { target *-*-* } .-1 }
    let f = [0_usize; -1_isize];
    // { dg-error "mismatched types, expected .usize. but got .isize." "" { target *-*-* } .-1 }
}
