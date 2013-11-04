// PR c++/49355
// { dg-options -std=c++11 }

#include <string>

struct T {
  std::string foobar;
};

int main()
{
  T* x = new T({""});
}
