// PR c++/22204
// { dg-options "-frepo" }

#include <typeinfo>
template<int>
struct function1
{
  function1()
  {
    typeid(int[100]);
  }
};
function1<1> b;

int main () {}
