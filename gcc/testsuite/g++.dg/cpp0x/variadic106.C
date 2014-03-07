// Origin: PR c++/47326
// { dg-do compile { target c++11 } }

template <int _N>
struct A
{
  typedef int value_type;
};

template <typename... _ARGS>
auto
f (_ARGS... args) -> typename A<sizeof...(args)>::value_type
{
  return 12;
}

int
main()
{
  f(1,2);
}
