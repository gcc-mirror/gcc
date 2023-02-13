// PR c++/108071
// { dg-do compile { target c++14 } }

#include <initializer_list>

template<bool> struct enable_if { };
template<> struct enable_if<true> { using type = void; };

template<typename T> constexpr bool is_array_v = false;
template<typename T, std::size_t N> constexpr bool is_array_v<T[N]> = true;

struct OUString
{
  template<typename T, typename = typename enable_if<is_array_v<T>>::type>
  OUString(T&) { }
};

struct vector
{
  vector(std::initializer_list<OUString>) { }
  template<typename Iter>
  vector(Iter i, Iter j) { if (i != j) OUString(*i); }
};

vector v = { "" };
