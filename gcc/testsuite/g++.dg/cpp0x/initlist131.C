// PR c++/108071
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct OptSpecifier {
  explicit OptSpecifier(bool);
  OptSpecifier(unsigned);
};
void f (std::initializer_list<OptSpecifier>);
int main()
{
  f({1});
}
