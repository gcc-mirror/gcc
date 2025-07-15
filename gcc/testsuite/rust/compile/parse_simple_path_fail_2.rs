mod A {
    struct B;
}

use A{B};
// { dg-error "unexpected token" "" { target *-*-* } .-1 }
// { dg-error "could not parse use tree" "" { target *-*-* } .-2 }
// { dg-error "failed to parse item in crate" "" { target *-*-* } 10 }
// ^^^ TODO: should the above error happen at line 10?
