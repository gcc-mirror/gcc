// https://gcc.gnu.org/PR105475
// Test the case where we specialize coroutine_handle and break it.
#include <coroutine>

struct promise;

struct task
{ using promise_type = promise; };

struct promise
{
  void return_void () {}
  std::suspend_never final_suspend() noexcept { return {}; }
  std::suspend_never initial_suspend() noexcept { return {}; }
  void unhandled_exception () {}
  task get_return_object() { return {}; }
};

/* Invalid.  */
namespace std
{
  template<>
  struct coroutine_handle<promise>
  {};
};

task foo()
{ co_return; }
// { dg-error "'from_address' is not a member of 'std::__n4861::coroutine_handle<promise>'" "" { target *-*-* } {.-1} }
