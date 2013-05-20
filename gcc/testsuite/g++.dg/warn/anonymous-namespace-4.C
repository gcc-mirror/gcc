// PR c++/57137

#include "anonymous-namespace-4.h"

namespace
{
  class NonCloneable;
  void fn1 ()
  {
    is_function_impl < NonCloneable > i;
  }
}
