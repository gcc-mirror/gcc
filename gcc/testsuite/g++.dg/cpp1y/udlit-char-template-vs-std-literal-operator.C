// { dg-do run { target c++14 } }

#include <cassert>

template<typename CharT, CharT... String>
int operator"" _script () {
  return 1;
}

int operator"" _script (const char*, unsigned long) {
  return 2;
}

int main ()
{
  assert ("123"_script == 2);
}
