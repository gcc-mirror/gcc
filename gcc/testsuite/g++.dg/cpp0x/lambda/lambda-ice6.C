// PR c++/51464
// { dg-options "-std=c++0x" }

template<int = sizeof([])> struct A {}; // { dg-error "lambda" } 
