// PR c++/59080
// { dg-require-effective-target c++11 }

#include <initializer_list>

auto foo[] = {};    // { dg-error "6:.foo. declared as array of .auto" }
