// PR c++/50157
// { dg-do compile { target c++11 } }

template<class T>
T val();

template<class T, class Arg, class =
  decltype(::new T(val<Arg>()))
>
auto test(int) -> char;

template<class, class>
auto test(...) -> char (&)[2];

struct P {
  explicit operator bool(); // (#13)
};

typedef decltype(test<bool, P>(0)) type; // OK
typedef decltype(test<float, P>(0)) type2; // Error (#17)
