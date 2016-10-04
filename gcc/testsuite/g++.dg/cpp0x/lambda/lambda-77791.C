// PR c++/77791
// { dg-do compile { target c++11 } }

auto a = [] (int i, int i = 0) {};	// { dg-error "redefinition of" }
