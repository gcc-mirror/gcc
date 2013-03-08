// PR c++/56565
// { dg-do compile { target c++11 } }

struct bug { int a; int *b = [&]{ return &a; }(); };
