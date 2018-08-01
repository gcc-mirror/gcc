// PR c++/86190
// { dg-options "-Wsign-conversion -Wsign-compare" }

typedef unsigned long sz_t;
sz_t s();
bool f(int i) { return s() < (unsigned long) i; }
bool f2(int i) { return s() < static_cast<unsigned long>(i); }
bool f3(int i) { return s() < i; } // { dg-warning "comparison of integer expressions of different signedness" }
bool f4(int i) { return s() < (long) i; } // { dg-warning "comparison of integer expressions of different signedness" }
bool f5(short int i) { return s() < (int) i; } // { dg-warning "comparison of integer expressions of different signedness" }
bool f6(signed char i) { return s() < (int) i; } // { dg-warning "comparison of integer expressions of different signedness" }
bool f7(unsigned char i) { return s() < i; }
bool f8(signed char i) { return s() < i; } // { dg-warning "comparison of integer expressions of different signedness" }
