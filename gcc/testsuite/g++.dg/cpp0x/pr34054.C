// PR c++/34054
// { dg-do compile { target c++11 } }

template<typename... T> T foo() { return T(); } // { dg-error "not expanded|T" }
