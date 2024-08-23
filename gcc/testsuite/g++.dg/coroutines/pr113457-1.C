// https://gcc.gnu.org/PR113457
#include <coroutine>

struct coro
{
  struct promise_type
  {
    std::suspend_never initial_suspend ();
    std::suspend_never final_suspend () noexcept;
    void return_void ();
    void unhandled_exception ();
    coro get_return_object ();
  };
};

struct not_quite_suspend_never : std::suspend_never
{};

coro
foo ()
{
  co_await std::suspend_never{},
    [] () -> coro { co_return; },
    co_await not_quite_suspend_never{};
}
