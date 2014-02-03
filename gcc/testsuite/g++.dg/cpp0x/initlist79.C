// PR c++/59646
// { dg-require-effective-target c++11 }

#include <initializer_list>

struct A {};

std::initializer_list<volatile A> x = {{}};
