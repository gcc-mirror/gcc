// { dg-do compile { target c++11 } }

class Foo { };

template<char...>
  Foo operator ""_Foo();
