//  { dg-additional-options "-std=c++14" }

#include "ramp-return.h"

task<int>
foo ()
{
 std::coroutine_handle<promise<int>> _handle;
 return task<int> (_handle);
}

// This ICEd for the PR.

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
