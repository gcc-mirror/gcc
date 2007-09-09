// PR c++/22204
// { dg-options "-frepo" }
// { dg-require-host-local "" }

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

// { dg-final { cleanup-repo-files } }
