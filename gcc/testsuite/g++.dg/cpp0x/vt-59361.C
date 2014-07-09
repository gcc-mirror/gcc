// PR c++/59361
// { dg-do compile { target c++11 } }

template<bool ...Bs>
struct and_ 
{
  constexpr static bool value{true};
};

template<typename T>
struct true_
{
  constexpr operator bool() const { return true; }
};

template<typename ...Ts>
constexpr bool foo(Ts...)
{
  return and_<(true_<Ts>())...>::value;
}
