// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <vector>

class T
{
  public:
  T();

};

std::vector <T> tp;

void f()
{
      tp.insert(tp.begin(), 10 , T());
}
