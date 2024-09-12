// PR c++/49355
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

struct T {
  std::string foobar;
};

int main()
{
  T* x = new T({""});
}
