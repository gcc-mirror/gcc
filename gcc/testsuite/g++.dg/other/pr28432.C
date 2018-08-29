
// Test to make sure we do not ICE on this invalid program.

// { dg-options "" }

struct A {}; // { dg-message "defined here" }

void A::foo(); // { dg-error "no declaration matches" }
// { dg-message "no functions named" "note" { target *-*-* } .-1 }
