// { dg-do assemble { target c++98 } }

template <class X, int n> X f (auto X (*x)[n]) { return (*x)[n/2]; }
extern int i[30];
extern double d[99];

int foo (int ii) { return f (&i); }		// causes abort
double foo (double dd) { return f (&d); }
