// PR c++/12132

inline template <int> void foo () {} // { dg-error "<" }
void abort (); // { dg-error ";" }
