// PR c++/103534
// { dg-additional-options "-O -Wall" }

#include <string>

std::string foo(std::string x)
{
  // This used to get a bogus -Wstringop-overflow warning.
  return std::string("1234567890123456") + x;
}
