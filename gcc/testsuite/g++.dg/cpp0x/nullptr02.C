// { dg-do compile }
// { dg-options "-std=c++11" }

// Test assignment to nullptr_t

typedef decltype(nullptr) nullptr_t;

const nullptr_t np1 = nullptr;
const nullptr_t np2 = __null;
const nullptr_t np3 = 0;
const nullptr_t np4 = np1;
const nullptr_t np5 = np2;
const nullptr_t np6 = np3;
const nullptr_t np7 = np4;
const nullptr_t np8 = np5;
const nullptr_t np9 = np6;
