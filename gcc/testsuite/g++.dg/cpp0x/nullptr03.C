// { dg-do compile { target c++11 } }

// Test assignment to int

const int n1 = nullptr;     // { dg-error "cannot convert " }
decltype(nullptr) mynull = 0;
const int n2 = mynull;      // { dg-error "cannot convert " }

