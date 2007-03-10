// { dg-options "-std=gnu++0x" }
template<typename...>
class tuple;

template<typename... Args>
class tuple { };

template<typename T1, class... Args>
class tuple1p { };

