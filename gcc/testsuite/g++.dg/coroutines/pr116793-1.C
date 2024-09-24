// https://gcc.gnu.org/PR116793
#include <tuple>
#include <coroutine>
struct _cleanup_task {
  bool await_ready() const noexcept;
  template <typename Promise>
  bool await_suspend(std::coroutine_handle<Promise> parent) noexcept;
  std::tuple<int &> await_resume() noexcept;
};
struct _task1 {
  struct promise_type final {
    std::suspend_always initial_suspend() noexcept;
    _task1 get_return_object() noexcept;
    void unhandled_exception() noexcept;
    struct awaiter final {
      bool await_ready() noexcept;
      void await_resume() noexcept;
      void await_suspend(std::coroutine_handle<promise_type> h) noexcept;
    };
    awaiter final_suspend() noexcept;
  };
};
_cleanup_task func(int &&);
_task1 g() {
  auto &&[i] = co_await func(3);
}
