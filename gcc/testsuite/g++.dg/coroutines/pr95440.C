#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif
#if 0
struct suspend_n {
  const int x;
  constexpr suspend_n (int x) : x (x) {}
  constexpr static bool await_ready() { return false; }
  constexpr static void await_suspend(std::coroutine_handle<>) {}
  constexpr static void await_resume() {}
};
#endif
struct task
{
  struct promise_type
  {
    auto get_return_object() const { return task{}; }
#if 0
//    static constexpr suspend_n initial_suspend()  { return {2}; }
#endif
    static constexpr std::suspend_always initial_suspend()  { return {}; }
    static constexpr std::suspend_never final_suspend() noexcept { return {}; }
    static constexpr void return_void() {}
    static constexpr void unhandled_exception() {}
  };
};

task
test_task ()
{
  co_await std::suspend_always{};
}

auto t = test_task();

int main() {}
