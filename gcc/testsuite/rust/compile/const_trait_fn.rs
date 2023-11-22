trait Osterkz {
    const fn x();
    // { dg-error "functions in traits cannot be declared const .E0379." "" { target *-*-* } .-1 }
}
