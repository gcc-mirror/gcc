// { dg-options "-Wsized-deallocation" }

void operator delete (void *p) throw() { __builtin_free(p); } // { dg-warning "sized" "" { target c++14 } }
