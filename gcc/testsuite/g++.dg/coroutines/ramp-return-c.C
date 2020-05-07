//  { dg-additional-options "-std=c++17" }
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
