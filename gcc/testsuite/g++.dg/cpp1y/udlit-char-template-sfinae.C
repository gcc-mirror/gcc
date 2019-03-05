// { dg-do run { target c++14 } }
// { dg-options -w }

#include <cassert>

template<bool, typename _Tp = void>struct enable_if {};
template<typename _Tp> struct enable_if<true, _Tp> { typedef _Tp type; };


template<typename CharT, CharT... String>
typename enable_if<sizeof...(String) == 6, int>::type operator"" _script () {
  return 5;
}

template<typename CharT, CharT... String>
typename enable_if<sizeof...(String) == 3, int>::type operator"" _script () {
  return 3;
}

template<typename CharT, CharT... String>
typename enable_if<sizeof...(String) != 3 && sizeof...(String) != 6, int>::type operator"" _script () {
  return 1;
}

int main ()
{
  assert ("hello!"_script == 5);
  assert (u8"hi!"_script == 3);
  assert ("hey!"_script == 1);
}
