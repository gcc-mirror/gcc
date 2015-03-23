// PR c++/64626
// { dg-do compile { target c++14 } }

0''; // { dg-error "empty character constant" }

123'''; // { dg-error "empty character constant" }

// { dg-error "expected unqualified-id" "expected unqualified-id" { target *-*-* } 4 }

// { dg-error "missing terminating" "missing terminating" { target *-*-* } 6 }
// { dg-error "expected unqualified-id" "expected unqualified-id" { target *-*-* } 6 }
