// PR middle-end/98994
// { dg-do compile }
// { dg-additional-options "-Wstringop-overread -O2" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

const char constantString[] = {42, 53};

void f(std::string& s)
{
  s.insert(0, static_cast<const char*>(constantString), 2);
}
