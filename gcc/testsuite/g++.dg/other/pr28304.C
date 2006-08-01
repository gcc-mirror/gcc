
// Test to make sure we do not ICE on this invalid program.

struct A {};

template<typename T> void A::foo(T) {}  // { dg-error "" }

void bar()
{
    A::foo(1); // { dg-error "no matching function for call" }
}
