// https://gcc.gnu.org/PR105475
// Test the case where we create a static address.
struct promise;

namespace std
{
  template<typename>
  struct coroutine_handle
  {
    static coroutine_handle from_address(void*);

    static void* address();
  };

  struct suspend_never
  {
    bool await_ready();
    void await_suspend(coroutine_handle<void>);
    void await_resume();
  };

  template<typename T>
  struct coroutine_traits
  { using promise_type = typename T::promise_type; };
};


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

task foo() { co_return; }
// { dg-error "std::coroutine_handle<void>::address' must be a non-overloaded method" "" { target *-*-* } {.-1} }
