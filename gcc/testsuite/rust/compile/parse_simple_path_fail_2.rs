mod A {
    struct B;
}

use A{B};
// { dg-error "unexpected token" "" { target *-*-* } .-1 }
// { dg-error "could not parse use tree" "" { target *-*-* } .-2 }
