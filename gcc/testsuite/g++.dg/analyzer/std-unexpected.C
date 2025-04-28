// { dg-require-effective-target c++14_down }
// { dg-additional-options "-Wno-deprecated-declarations" }

#include <exception>

void test_1 ()
{
  std::unexpected ();
}
