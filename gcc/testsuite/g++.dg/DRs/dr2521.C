// CWG2521
// { dg-do compile { target c++11 } }

void operator "" _foo(const char *); // { dg-warning "deprecated" "" { target c++23 } }
void operator ""_bar(const char *);
