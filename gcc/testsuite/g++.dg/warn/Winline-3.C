// { dg-options "-Winline -O" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

using namespace std;

int main(void)
{
  vector<int> v(10);
}
