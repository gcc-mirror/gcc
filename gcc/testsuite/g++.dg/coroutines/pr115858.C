#include <coroutine>

struct task
{
  struct promise_type
  {
    void return_void () {}
    task get_return_object () { return {}; }
    void unhandled_exception () {}
    std::suspend_never initial_suspend () { return {}; }
    std::suspend_never final_suspend () noexcept { return {}; }
  };
};

task
f ()
{
  void* a = __builtin_alloca (10);
  // { dg-message "sorry, unimplemented: 'alloca' is not yet supported in coroutines" "" { target *-*-* } {.-1} }
  void* b = __builtin_alloca_with_align (10, 16);
  // { dg-message "sorry, unimplemented: 'alloca' is not yet supported in coroutines" "" { target *-*-* } {.-1} }
  co_return;
}
