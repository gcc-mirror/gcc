// PR c++/12613
// { dg-options "-fshow-column" }

enum { FOO = 1, BAR = 2 };
int a[] = { FOO: 1, BAR: 2 };

// the following 2 column locations are still not accurate enough
// { dg-error "28:name 'FOO' used in a GNU-style designated initializer for an array" "" { target *-*-* } 5 }
// { dg-error "28:name 'BAR' used in a GNU-style designated initializer for an array" "" { target *-*-* } 5 }
