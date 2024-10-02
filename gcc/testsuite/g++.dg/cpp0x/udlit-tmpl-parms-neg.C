// { dg-do compile { target c++11 } }

class Foo { };

template<wchar_t...>
  Foo operator ""_Foo(); // { dg-error "7:literal operator template .Foo operator\"\"_Foo\\(\\). has invalid parameter list" }

template<char>
  Foo operator ""_Bar(); // { dg-error "7:literal operator template .Foo operator\"\"_Bar\\(\\). has invalid parameter list" }

template<typename... Type>
  Foo operator ""_Bar(); // { dg-error "7:literal operator template .Foo operator\"\"_Bar\\(\\). has invalid parameter list" }
