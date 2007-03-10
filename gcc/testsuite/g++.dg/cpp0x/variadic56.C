// { dg-options "-std=gnu++0x" }
template<typename... Elements>
struct tuple { };

template<typename T, typename... Elements>
struct tuple<T, Elements...> {
  int foo();
};

template<typename T, typename... Elements>
struct tuple<T*, Elements...> {
  int bar();
};

template<typename T, typename... Elements>
int tuple<T, Elements...>::foo() { return 0; }

template<typename T, typename... Elements>
int tuple<T*, Elements...>::bar() { return 0; }
