// Build don't link:
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
