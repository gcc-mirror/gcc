// PR c++/100261
// { dg-do compile }

#include <typeinfo>

namespace std {
  namespace decimal {
    class decimal32 {
      float private__decfloat32;
    };
  }
}

void
foo ()
{
  typeid (float);
  typeid (std::decimal::decimal32);
}
