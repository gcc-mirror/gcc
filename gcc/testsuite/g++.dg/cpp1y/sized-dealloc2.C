// Test that -Wc++14-compat warns about the change in meaning.
// { dg-options "-Wall" }

typedef __SIZE_TYPE__ size_t;
void operator delete[] (void *p, size_t s) throw(); // { dg-warning "usual" "" { target { ! c++14 } } }
