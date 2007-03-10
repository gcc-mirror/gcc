// { dg-options "-std=gnu++0x" }
template<typename...> struct Tuple { };
template<typename... Types>              char& g(Tuple<Types...>);       // #1
template<typename T1, typename... Types> short& g(Tuple<T1, Types...>);   // #2
template<typename T1, typename... Types> int& g(Tuple<T1, Types&...>);  // #3

void f() {
  //  char& x1 = g(Tuple<>());             // calls #1
  short& y1 = g(Tuple<int, float>());  // calls #2
  //  int& z1 = g(Tuple<int, float&>());   // calls #3
  //  int& z2 = g(Tuple<int>());           // calls #3
  //  int& z3 = g(Tuple<int>());           // calls #3
}
