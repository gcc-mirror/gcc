// PR c++/60365
// { dg-do compile { target c++11 } }

void func [[noreturn, noreturn]] (); // { dg-warning "specified multiple times" }
