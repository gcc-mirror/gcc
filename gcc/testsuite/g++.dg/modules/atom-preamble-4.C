// { dg-additional-options "-fmodules-ts" }
#define NAME(X) X;

export module NAME(bob)

// { dg-error "module name followed by '\\\('" "" { target *-*-* } .-2 }
// { dg-error "expected ';' before '\\\(' token" "" { target *-*-* } .-3 }
