// { dg-options -std=c++0x }

class Foo { };

template<char...>
  Foo operator"" _Foo();
