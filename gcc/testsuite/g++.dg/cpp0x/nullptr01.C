// { dg-do compile { target c++11 } }

// Test assignment to pointer

char* const cp1 = nullptr;
char* const cp2 = __null;
char* const cp3 = 0;
decltype(nullptr) mynull = 0;
char* const cp4 = mynull;
