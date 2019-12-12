// { dg-do compile }

// Origin: Christian Ehrhardt <ehrhardt@mathematik.uni-ulm.de>

// PR c++/9364: ICE processing typename with name error.

void find(typename int&); // { dg-error "6:variable or field .find. declared void" }
// { dg-error "expected" "" { target *-*-* } .-1 }
