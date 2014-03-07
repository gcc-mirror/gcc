// { dg-do compile { target c++11 } }
template<typename...>
class tuple;

template<typename... Args>
class tuple { };

template<typename T1, class... Args>
class tuple1p { };

