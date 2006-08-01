
// Test to make sure we do not ICE on this invalid program.

// { dg-options "" }

struct A {};
void A::foo(); // { dg-error "member function declared in class|outside of class is not definition" }
