// PR c++/57196
// { dg-require-effective-target c++11 }

#include <initializer_list>

template<class T>
struct set {
  set() = default;
  set(std::initializer_list<T>){}
};

struct string {
  string(const char*){}
  ~string(){}
};

typedef decltype(sizeof(0)) size_t;

template <size_t> struct EqHelper { };

int IsNullLiteralHelper(...);

void Fn() {
  EqHelper<sizeof IsNullLiteralHelper(set<int>{1})>        eq1;  // ok
  EqHelper<sizeof IsNullLiteralHelper(set<string>())>      eq2;  // ok
  EqHelper<sizeof IsNullLiteralHelper(set<string>{"foo"})> eq3;  // error
}
