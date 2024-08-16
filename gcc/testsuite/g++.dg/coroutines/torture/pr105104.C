// https://gcc.gnu.org/PR105104
// { dg-additional-options "-O" }

#include <coroutine>

// ICE during GIMPLE pass: coro-early-expand-ifs. final_awaiter::await_resume is
// non-void, and optimizations are enabled.

struct return_object
{
  struct promise_type
  {
    static constexpr std::suspend_always initial_suspend () noexcept
    {
      return {};
    }

    struct final_awaiter
    {
      static constexpr bool await_ready () noexcept { return false; }
      static constexpr void await_suspend (std::coroutine_handle<>) noexcept {}
      static constexpr int await_resume () noexcept { return {}; }
    };
    static constexpr final_awaiter final_suspend () noexcept { return {}; }

    static void unhandled_exception () { throw; }

    return_object get_return_object () { return {}; }

    static constexpr void return_void () noexcept {}
  };
};

return_object
coroutine ()
{
  co_return;
}

return_object f = coroutine ();
