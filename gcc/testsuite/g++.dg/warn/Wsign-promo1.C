// Check that we get joust warnings from comparing the final champ to a
// candidate between it and the previous champ.

// { dg-additional-options -Wsign-promo }

struct A { A(int); };

enum E { e };

int f(int, A);
int f(unsigned, A);
int f(int, int);

int i = f(e, 42);		// { dg-warning "passing 'E'" }
// { dg-warning "in call to 'int f" "" { target *-*-* } .-1 }
