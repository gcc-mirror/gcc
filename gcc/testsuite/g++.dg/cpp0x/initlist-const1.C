// { dg-do compile { target c++11 } }

#include <initializer_list>

const auto x = { 1, 2 };

// { dg-final { scan-assembler-not {\.data} } }
