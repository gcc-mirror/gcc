// PR c++/99062
// { dg-do compile }

#define INT_MIN (-__INT_MAX__ - 1)
void *f () __attribute__ ((assume_aligned (INT_MIN, 4))); // { dg-warning "is not positive" }
