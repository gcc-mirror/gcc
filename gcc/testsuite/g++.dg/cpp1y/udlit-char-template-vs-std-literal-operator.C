// { dg-do run { target c++14 } }

#include <cassert>

typedef decltype(sizeof(0)) size_type;

template<typename CharT, CharT... String>
int operator"" _script () {
  return 1;
}

int operator"" _script (const char*, size_type) {
  return 2;
}

int main ()
{
  assert ("123"_script == 2);
}
