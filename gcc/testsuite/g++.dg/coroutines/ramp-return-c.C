//  { dg-additional-options "-std=c++17" }
//  { dg-skip-if "requires hosted libstdc++ for vector in ramp-return.h" { ! hostedlib } }
#define DELETE_COPY_CTOR 1
#include "ramp-return.h"

task<int>
foo ()
{
 std::coroutine_handle<promise<int>> _handle;
 return task<int> (_handle); 
}

task<int>
bar ()
{
  co_return 0;
}

task<std::vector<int>>
baz ()
{
  co_return std::vector<int>();
}
