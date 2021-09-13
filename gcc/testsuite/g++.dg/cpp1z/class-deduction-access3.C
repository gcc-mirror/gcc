// { dg-do compile { target c++17 } }

template<class>
struct Cont;

template<class T>
class Base
{
  using type = T;
  friend Cont<T>;
};

template<class T>
struct Cont
{
  using argument_type = typename Base<T>::type;
  Cont(T, argument_type);
};

Cont c(1, 1);
