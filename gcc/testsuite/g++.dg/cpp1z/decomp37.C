// { dg-do compile { target c++17 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <memory>
#include <tuple>
#include <string>

struct X : private std::shared_ptr<int>
{
  std::string fun_payload;
};

template<int N> std::string& get(X& x)
{
  if constexpr(N==0) return x.fun_payload;
}

namespace std {
  template<> class tuple_size<X> : public std::integral_constant<int, 1> {};
  template<> class tuple_element<0, X> {public: using type = std::string;};
}

struct X2 : private std::shared_ptr<int>
{
  int fun_payload;
  template <class T> void get();
};

template<int N> int& get(X2& x)
{
  if constexpr(N==0) return x.fun_payload;
}

namespace std {
  template<> class tuple_size<X2> : public std::integral_constant<int, 1> {};
  template<> class tuple_element<0, X2> {public: using type = int;};
}

class X3
{
  double fun_payload;
public:
  template <int N> double& get()
  {
    if constexpr(N==0) return fun_payload;
  }
};

namespace std {
  template<> class tuple_size<X3> : public std::integral_constant<int, 1> {};
  template<> class tuple_element<0, X3> {public: using type = double;};
}

int main()
{
  X x;
  auto& [b1] = x;
  X2 x2;
  auto& [b2] = x2;
  X3 x3;
  auto& [b3] = x3;
}
