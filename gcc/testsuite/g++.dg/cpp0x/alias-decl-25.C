// PR c++/54859
// { dg-do compile { target c++11 } }

template<unsigned N>
  using Num = int;

template<typename... Types>
  using Count = Num<sizeof...(Types)>;

Count<int, char, void> i;
