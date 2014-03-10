// { dg-do compile { target c++11 } }

template<char...>
  int operator"" _abc();
