#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif

struct task
{
  struct promise_type
  {
    auto get_return_object() const { return task{}; }
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
