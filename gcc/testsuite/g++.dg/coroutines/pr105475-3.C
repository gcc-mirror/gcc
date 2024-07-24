// https://gcc.gnu.org/PR105475
// Test the case where we create a non-static from_address.
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
  { coroutine_handle from_address(void*); };
};

task foo()
{ co_return; }
// { dg-error "std::__n4861::coroutine_handle<promise>::from_address' must be a non-overloaded static function" "" { target *-*-* } {.-1} }
