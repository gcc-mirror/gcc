// { dg-do compile { target c++14 } }

auto f1 ();
auto a = 5, f2 ();		// { dg-error "in declaration with more than one declarator" }
auto f3 (), b = 6;		// { dg-error "in declaration with more than one declarator" }
auto f4 (), f5 (), f6 ();	// { dg-error "in declaration with more than one declarator" }
auto f1 () { return 3; }
auto f2 () { return 4; }
auto f3 () { return 5; }
auto f4 () { return 6; }
auto f5 () { return 7; }
auto f6 () { return 8; }
