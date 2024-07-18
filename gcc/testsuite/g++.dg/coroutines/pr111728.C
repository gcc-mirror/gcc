// { dg-do compile }
// https://gcc.gnu.org/PR111728
#include <coroutine>
struct promise;
struct coroutine : std::coroutine_handle<promise>
{
    using promise_type = ::promise;
    bool await_ready() { return false; }
    void await_suspend(coroutine_handle h) {}
    int await_resume() { return {} ;}
};
struct promise
{
    coroutine get_return_object() { return {coroutine::from_promise(*this)}; }
    std::suspend_always initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
};
coroutine
write_fields() {
  int static_buffer[10];
  co_await [](auto)
  -> coroutine
  {
    if (sizeof(static_buffer));
      co_return;
  }(0);
}
