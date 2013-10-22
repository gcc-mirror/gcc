// { dg-options -std=c++11 }

class Foo { };

template<char...>
  Foo operator"" _Foo();
