trait Osterkz {
    const fn x();
    // { dg-error "functions in traits cannot be declared .const." "" { target *-*-* } .-1 }
}
