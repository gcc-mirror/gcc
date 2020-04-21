//  { dg-options "-fcoroutines -std=c++14" }
#define DELETE_COPY_CTOR 1
#include "ramp-return.h"

task<int>
foo ()
{
 std::coroutine_handle<promise<int>> _handle;
 return task<int> (_handle);  // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = int\]'} }
}

task<int>
bar ()
{
  co_return 0;
} // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = int\]'} }

task<std::vector<int>>
baz ()
{
  co_return std::vector<int>();
} // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = std::vector<int>\]'} }
