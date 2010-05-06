// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test assignment to method pointer

class F { };

typedef void (F::*pmf)();

const pmf pmf1 = nullptr;
const pmf pmf2 = __null;
const pmf pmf3 = 0;
