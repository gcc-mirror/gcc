// Test that conversion to std::initializer_list takes priority over other
// user-defined conversions.

// { dg-do link }
// { dg-options "-std=c++0x" }

#include <initializer_list>

struct string
{
  string (const char *) {}
  template <class Iter> string (Iter, Iter);
};
  
template <class T, class U>
struct pair
{
  pair (T t, U u) {}
};

template<class T, class U>
struct map
{
  void insert (pair<T,U>);
  void insert (std::initializer_list<pair<T,U> >) {}
};

int main()
{
  map<string,string> m;
  m.insert({ {"this","that"}, {"me","you"} });
}
