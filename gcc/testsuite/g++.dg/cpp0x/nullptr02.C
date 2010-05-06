// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test assignment to nullptr_t

typedef decltype(nullptr) nullptr_t;

const nullptr_t np1 = nullptr;
const nullptr_t np2 = __null;
const nullptr_t np3 = 0;
