//  { dg-options "-fcoroutines -std=c++14" }
//  { dg-skip-if "requires hosted libstdc++ for vector in ramp-return.h" { ! hostedlib } }
#define DELETE_COPY_CTOR 1
#include "ramp-return.h"

task<int>
foo ()
{
 std::coroutine_handle<promise<int>> _handle;
 return task<int> (_handle);  // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = int\]'} }
}

task<int>
bar ()  // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = int\]'} }
{
  co_return 0;
}

task<std::vector<int>>
baz ()  // { dg-error {use of deleted function 'task<T>::task\(const task<T>&\) \[with T = std::vector<int>\]'} }
{
  co_return std::vector<int>();
}
