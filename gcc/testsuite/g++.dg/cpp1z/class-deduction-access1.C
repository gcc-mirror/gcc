// { dg-do compile { target c++17 } }

template<typename T>
struct Base
{
protected:
  using type = T;
};

template<typename T>
struct Cont : Base<T>
{
  using argument_type = typename Base<T>::type;

  Cont(T, argument_type) { }
};

Cont c(1, 1);
