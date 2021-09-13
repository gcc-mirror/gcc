// PR c++/60051
// { dg-require-effective-target c++11 }

#include <initializer_list>

auto x[2] = {};			// { dg-error "14:unable to deduce" }
