// PR c++/71446
// { dg-do compile { target c++11 } }
// { dg-options "" }

#include <initializer_list>

int foo (int);				// { dg-message "initializing argument 1 of" }
int x = foo ({.foo = 0});		// { dg-error "cannot convert" }

int bar (_Complex int);			// { dg-message "initializing argument 1 of" }
int y = bar ({.real = 0, .imag = 1});	// { dg-error "cannot convert" }

int baz (std::initializer_list<int>);
int z = baz ({.one = 1, .two = 2, .three = 3});	// { dg-error "could not convert" }
