// PR c++/57138
// { dg-do compile { target c++11 } }

template <template <typename ... X> class T, typename ... Y>
struct D
{
  template <typename ... Z>
  using type = T <Y..., Z...>;	// { dg-error "pack expansion" }
};
template <typename T>
class A {};
template <typename X, typename Y>
struct B;
template <typename T>
struct B <int, T>
{
  typedef A <T> type;
};
template <typename X, typename Y>
using C = typename B <X, Y>::type;
struct E : public D <C> {};
