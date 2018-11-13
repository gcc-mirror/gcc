// PR c++/51464
// { dg-do compile { target c++11 } }

template<int = sizeof([])> struct A {}; // { dg-error "" } 
