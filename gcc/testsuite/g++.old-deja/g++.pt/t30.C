// Build don't link: 

template <class X, int n> X f (auto X (*x)[n]) { return (*x)[n/2]; }
extern int i[30], i2[33];
extern double d[99];

int foo (int ii) { return f (&i) + f(&i2); }	// causes abort
double foo (double dd) { return f (&d); }
