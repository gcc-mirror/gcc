// { dg-do compile }
// { dg-options "-std=c++11" }

// Test assignment to method pointer

class F { };

typedef void (F::*pmf)();

const pmf pmf1 = nullptr;
const pmf pmf2 = __null;
const pmf pmf3 = 0;
decltype(nullptr) mynull = 0;
const pmf pmf4 = mynull;
