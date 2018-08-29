// { dg-do compile }

// Avoid -pedantic-error default
// { dg-options "" }

struct A { static int a; };

int t = A::A ? : 0; // { dg-error "cannot resolve" }
