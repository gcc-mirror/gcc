#include <coroutine>

struct task
{
  struct promise_type
  {
    std::suspend_always initial_suspend () { return {}; }
    std::suspend_always final_suspend () noexcept { return {}; }
    void unhandled_exception () {}
    task get_return_object () noexcept { return {}; }
    void return_void () {}
  };
};

task foo ()
{
  void bar ();
  co_return;
}
