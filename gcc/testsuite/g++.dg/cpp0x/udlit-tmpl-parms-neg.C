// { dg-do compile { target c++11 } }

class Foo { };

template<wchar_t...>
  Foo operator"" _Foo(); // { dg-error "literal operator template|has invalid parameter list" }

template<char>
  Foo operator"" _Bar(); // { dg-error "literal operator template|has invalid parameter list" }

template<typename... Type>
  Foo operator"" _Bar(); // { dg-error "literal operator template|has invalid parameter list" }
