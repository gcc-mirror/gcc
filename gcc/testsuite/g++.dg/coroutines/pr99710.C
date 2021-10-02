#include <coroutine>

struct task {
    struct promise_type {
        std::suspend_always initial_suspend();
        std::suspend_always final_suspend() noexcept;
        task get_return_object();
        void return_void();
        void unhandled_exception();
    };
};

task
my_coro ()
{
  try
    { }
  catch (...)
    {
      // [expr.await] An await-expression shall appear only in a potentially-
      // evaluated expression within the compound-statement of a function-body
      // outside of a handler 
      co_await std::suspend_always{}; // { dg-error "await expressions are not permitted in handlers" }
    }
}
