struct unclosed { /* { dg-message "17: to match this '.'" } */
  int dummy; /* { dg-error "12: expected '.' at end of input" } */
  // { dg-error "expected unqualified-id at end of input" "" { target *-*-* } .-1 }
