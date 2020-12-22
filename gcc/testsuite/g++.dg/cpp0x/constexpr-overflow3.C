// PR c++/98332
// { dg-do compile { target c++11 } }

#include <limits.h>

struct S { int a = INT_MAX + 1; }; // { dg-warning "overflow" }
struct { S b[2][1][1][1]; } c;
