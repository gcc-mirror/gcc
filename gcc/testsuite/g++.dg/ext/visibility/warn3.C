// Warn when a class member is specified to have greater visibility than
// its class.

// { dg-require-visibility "" }

struct __attribute ((visibility ("hidden"))) A
{
  __attribute ((visibility ("default"))) void f (); // { dg-warning "visibility" }
};

void A::f() { }
