// PR c++/98332
// { dg-do compile { target c++11 } }

struct S { int a = 2147483647 + 1; }; // { dg-warning "overflow" }
struct { S b[2][1][1][1]; } c;
